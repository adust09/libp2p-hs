-- | GossipSub Switch integration handler (Phase 10b).
--
-- Bridges the GossipSub Router with the Switch by:
-- 1. Registering a StreamHandler for inbound /meshsub/1.1.0 streams
-- 2. Providing a sendRPC callback that opens/reuses outbound streams
-- 3. Managing lifecycle (heartbeat start/stop)
--
-- GossipSub maintains persistent bidirectional RPC streams, unlike
-- Identify/Ping which are one-shot. Each peer has at most one cached
-- outbound stream.
module Network.LibP2P.Protocol.GossipSub.Handler
  ( -- * Types
    GossipSubNode (..)
    -- * Construction
  , newGossipSubNode
    -- * Stream handling
  , handleGossipSubStream
    -- * Lifecycle
  , startGossipSub
  , stopGossipSub
    -- * Convenience API
  , gossipJoin
  , gossipLeave
  , gossipPublish
    -- * Constants
  , gossipSubProtocolId
  ) where

import Control.Concurrent.Async (Async, cancel)
import Control.Concurrent.STM
  ( TVar
  , atomically
  , newTVarIO
  , readTVar
  , writeTVar
  , modifyTVar'
  )
import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime)
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.MultistreamSelect.Negotiation
  ( NegotiationResult (..)
  , ProtocolId
  , StreamIO (..)
  , negotiateInitiator
  )
import Network.LibP2P.Protocol.GossipSub.Heartbeat (runHeartbeat)
import Network.LibP2P.Protocol.GossipSub.Message (readRPCMessage, writeRPCMessage)
import Network.LibP2P.Protocol.GossipSub.Router
  ( addPeer
  , handleRPC
  , join
  , leave
  , newRouter
  , publish
  , removePeer
  )
import Network.LibP2P.Protocol.GossipSub.Types
  ( GossipSubParams
  , GossipSubRouter (..)
  , PeerProtocol (..)
  , RPC
  , Topic
  , maxRPCSize
  )
import Network.LibP2P.Switch.ConnPool (lookupConn)
import Network.LibP2P.Switch.Switch (removeStreamHandler, setStreamHandler)
import Network.LibP2P.Switch.Types
  ( Connection (..)
  , MuxerSession (..)
  , Switch (..)
  )

-- | GossipSub protocol ID.
gossipSubProtocolId :: ProtocolId
gossipSubProtocolId = "/meshsub/1.1.0"

-- | A GossipSub node: Router + Switch integration.
data GossipSubNode = GossipSubNode
  { gsnRouter    :: !GossipSubRouter
  , gsnSwitch    :: !Switch
  , gsnHeartbeat :: !(TVar (Maybe (Async ())))
  , gsnStreams   :: !(TVar (Map.Map PeerId StreamIO))  -- ^ Cached outbound streams per peer
  }

-- | Create a new GossipSub node with a Router wired to the Switch.
--
-- The Router's gsSendRPC callback opens/reuses outbound streams to peers
-- via the Switch's connection pool.
newGossipSubNode :: Switch -> GossipSubParams -> IO GossipSubNode
newGossipSubNode sw params = do
  streamsVar <- newTVarIO Map.empty
  hbVar <- newTVarIO Nothing
  -- Create router with real sendRPC that uses the Switch
  let localPid = swLocalPeerId sw
  router <- newRouter params localPid (sendRPCviaSwitch sw streamsVar) getCurrentTime
  pure GossipSubNode
    { gsnRouter    = router
    , gsnSwitch    = sw
    , gsnHeartbeat = hbVar
    , gsnStreams   = streamsVar
    }

-- | Send an RPC to a peer via cached or newly opened stream.
sendRPCviaSwitch :: Switch -> TVar (Map.Map PeerId StreamIO) -> PeerId -> RPC -> IO ()
sendRPCviaSwitch sw streamsVar pid rpc = do
  -- Try to use cached stream
  mCached <- atomically $ Map.lookup pid <$> readTVar streamsVar
  case mCached of
    Just stream -> do
      -- Try sending on cached stream; reopen on failure
      sendResult <- trySend stream rpc
      case sendResult of
        Right () -> pure ()
        Left _ -> do
          atomically $ modifyTVar' streamsVar (Map.delete pid)
          openAndSend sw streamsVar pid rpc
    Nothing -> openAndSend sw streamsVar pid rpc

-- | Open a new outbound stream to a peer and send an RPC.
openAndSend :: Switch -> TVar (Map.Map PeerId StreamIO) -> PeerId -> RPC -> IO ()
openAndSend sw streamsVar pid rpc = do
  mStream <- openStreamToPeer sw pid
  case mStream of
    Nothing -> pure ()  -- No connection to peer; fire-and-forget
    Just stream -> do
      atomically $ modifyTVar' streamsVar (Map.insert pid stream)
      _ <- trySend stream rpc
      pure ()

-- | Open a new mux stream to a peer and negotiate GossipSub protocol.
openStreamToPeer :: Switch -> PeerId -> IO (Maybe StreamIO)
openStreamToPeer sw pid = do
  mConn <- atomically $ lookupConn (swConnPool sw) pid
  case mConn of
    Nothing -> pure Nothing
    Just conn -> do
      result <- (Right <$> openAndNegotiate conn) `catch`
                  (\(_ :: SomeException) -> pure (Left ()))
      case result of
        Left () -> pure Nothing
        Right mStream -> pure mStream

-- | Open a mux stream and negotiate /meshsub/1.1.0.
openAndNegotiate :: Connection -> IO (Maybe StreamIO)
openAndNegotiate conn = do
  stream <- muxOpenStream (connSession conn)
  negResult <- negotiateInitiator stream [gossipSubProtocolId]
  case negResult of
    Accepted _ -> pure (Just stream)
    NoProtocol -> pure Nothing

-- | Try to send an RPC on a stream, catching exceptions.
trySend :: StreamIO -> RPC -> IO (Either () ())
trySend stream rpc =
  (writeRPCMessage stream rpc >> pure (Right ()))
    `catch` (\(_ :: SomeException) -> pure (Left ()))

-- | Handle an inbound GossipSub stream.
--
-- Reads framed RPCs in a loop and dispatches each to the Router's handleRPC.
-- On error or EOF, cleans up the peer's cached stream and removes the peer.
handleGossipSubStream :: GossipSubNode -> StreamIO -> PeerId -> IO ()
handleGossipSubStream node stream pid = do
  -- Register peer with router
  now <- getCurrentTime
  addPeer (gsnRouter node) pid GossipSubPeer False now
  -- Read loop
  readLoop
  -- Cleanup on disconnect
  removePeer (gsnRouter node) pid
  atomically $ modifyTVar' (gsnStreams node) (Map.delete pid)
  where
    readLoop = do
      result <- readRPCMessage stream maxRPCSize
      case result of
        Left _ -> pure ()  -- Error/EOF: stop loop
        Right rpc -> do
          handleRPC (gsnRouter node) pid rpc
          readLoop

-- | Start the GossipSub node: register stream handler, notifier, and start heartbeat.
startGossipSub :: GossipSubNode -> IO ()
startGossipSub node = do
  -- Register inbound stream handler on Switch
  setStreamHandler (gsnSwitch node) gossipSubProtocolId
    (handleGossipSubStream node)
  -- Register connection notifier to auto-open GossipSub streams to new peers
  atomically $ modifyTVar' (swNotifiers (gsnSwitch node))
    (onNewConnection node :)
  -- Start heartbeat background thread
  hbAsync <- runHeartbeat (gsnRouter node)
  atomically $ writeTVar (gsnHeartbeat node) (Just hbAsync)

-- | Called on new connection: open a GossipSub stream to the peer.
-- This registers the peer in the router and creates a read loop.
onNewConnection :: GossipSubNode -> Connection -> IO ()
onNewConnection node conn = do
  let pid = connPeerId conn
  -- Open a mux stream and negotiate GossipSub protocol
  mStream <- openAndNegotiate conn
  case mStream of
    Nothing -> pure ()  -- Peer doesn't support GossipSub
    Just stream -> do
      -- Cache the outbound stream
      atomically $ modifyTVar' (gsnStreams node) (Map.insert pid stream)
      -- Register peer and start read loop (blocks until disconnect)
      now <- getCurrentTime
      addPeer (gsnRouter node) pid GossipSubPeer True now

-- | Stop the GossipSub node: cancel heartbeat and unregister handler.
stopGossipSub :: GossipSubNode -> IO ()
stopGossipSub node = do
  -- Cancel heartbeat
  mHb <- atomically $ do
    hb <- readTVar (gsnHeartbeat node)
    writeTVar (gsnHeartbeat node) Nothing
    pure hb
  case mHb of
    Just hbAsync -> cancel hbAsync `catch` (\(_ :: SomeException) -> pure ())
    Nothing -> pure ()
  -- Unregister stream handler
  removeStreamHandler (gsnSwitch node) gossipSubProtocolId

-- | Subscribe to a topic.
gossipJoin :: GossipSubNode -> Topic -> IO ()
gossipJoin node topic = join (gsnRouter node) topic

-- | Unsubscribe from a topic.
gossipLeave :: GossipSubNode -> Topic -> IO ()
gossipLeave node topic = leave (gsnRouter node) topic

-- | Publish a message to a topic (signed with the Switch's identity key).
gossipPublish :: GossipSubNode -> Topic -> ByteString -> IO ()
gossipPublish node topic payload =
  publish (gsnRouter node) topic payload (Just (swIdentityKey (gsnSwitch node)))
