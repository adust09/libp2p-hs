-- | Listen loop for the Switch (docs/08-switch.md §Listening).
--
-- Accepts inbound connections, applies connection gating policy,
-- upgrades to secure multiplexed connections, and dispatches
-- inbound streams to registered protocol handlers.
module Network.LibP2P.Switch.Listen
  ( -- * Connection gating
    ConnectionGater (..)
  , defaultConnectionGater
    -- * Inbound connection handling
  , handleInbound
    -- * Stream dispatch
  , streamAcceptLoop
  , dispatchStream
    -- * Listen orchestration
  , switchListen
  , acceptLoop
  , switchListenAddrs
  ) where

import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Exception (SomeException, catch)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr)
import Network.LibP2P.MultistreamSelect.Negotiation
  ( NegotiationResult (..)
  , StreamIO
  , negotiateResponder
  )
import Network.LibP2P.Switch.ConnPool (addConn)
import Network.LibP2P.Switch.ResourceManager (Direction (..), reserveConnection)
import Network.LibP2P.Switch.Types
  ( ActiveListener (..)
  , Connection (..)
  , MuxerSession (..)
  , Switch (..)
  )
import Network.LibP2P.Switch.Upgrade (upgradeInbound)
import Network.LibP2P.Transport.Transport (Listener (..), RawConnection (..), Transport (..))

-- | Connection gater: policy-based admission control (docs/08-switch.md §Connection Gating).
--
-- Called at multiple points during connection establishment to allow
-- or deny based on policy (IP blocklist, Peer ID allowlist, etc.).
data ConnectionGater = ConnectionGater
  { gateAccept  :: !(Multiaddr -> IO Bool)  -- ^ Check after accepting raw connection (before upgrade)
  , gateSecured :: !(PeerId -> IO Bool)     -- ^ Check after security handshake (remote PeerId known)
  }

-- | Default gater that allows all connections.
defaultConnectionGater :: ConnectionGater
defaultConnectionGater = ConnectionGater
  { gateAccept  = \_ -> pure True
  , gateSecured = \_ -> pure True
  }

-- | Handle a single inbound connection: gate → upgrade → pool → stream accept loop.
--
-- This function blocks until the connection closes. Each accepted connection
-- should be spawned in its own async thread from the accept loop.
handleInbound :: Switch -> ConnectionGater -> RawConnection -> IO ()
handleInbound sw gater rawConn = do
  -- Gate 1: check remote address before any upgrade work
  allowed <- gateAccept gater (rcRemoteAddr rawConn)
  if not allowed
    then rcClose rawConn
    else do
      -- Upgrade: Noise XX handshake + Yamux session
      conn <- upgradeInbound (swIdentityKey sw) rawConn
      -- Gate 2: check remote PeerId after security handshake
      secured <- gateSecured gater (connPeerId conn)
      if not secured
        then muxClose (connSession conn)
        else do
          -- Gate 3: check resource limits (PeerId known after handshake)
          resCheck <- atomically $ reserveConnection (swResourceMgr sw) (connPeerId conn) Inbound
          case resCheck of
            Left _ -> muxClose (connSession conn)
            Right () -> do
              -- Add to connection pool
              atomically $ addConn (swConnPool sw) conn
              -- Block on stream accept loop until connection closes
              streamAcceptLoop sw conn

-- | Accept inbound streams and dispatch to registered protocol handlers.
--
-- Runs forever, accepting streams from the muxer and spawning a handler
-- thread for each. Uses multistream-select to negotiate the protocol,
-- then dispatches to the registered StreamHandler.
streamAcceptLoop :: Switch -> Connection -> IO ()
streamAcceptLoop sw conn = loop
  where
    loop = do
      -- Accept the next inbound stream from the muxer
      result <- safeAccept
      case result of
        Nothing -> pure ()  -- Muxer closed or error, stop the loop
        Just stream -> do
          -- Spawn handler for this stream, continue accepting
          _ <- async $ dispatchStream sw conn stream
          loop
    -- Catch exceptions from muxAcceptStream (e.g. session shutdown)
    safeAccept =
      (Just <$> muxAcceptStream (connSession conn))
        `catch` (\(_ :: SomeException) -> pure Nothing)

-- | Dispatch a single inbound stream to the appropriate protocol handler.
--
-- Runs multistream-select as responder to determine which protocol
-- the remote peer wants, then looks up and invokes the registered handler.
dispatchStream :: Switch -> Connection -> StreamIO -> IO ()
dispatchStream sw conn stream = do
  -- Get the list of supported protocols from the Switch
  supportedProtos <- atomically $
    Map.keys <$> readProtos
  -- Run multistream-select responder
  result <- negotiateResponder stream supportedProtos
  case result of
    Accepted proto -> do
      -- Look up the handler for the negotiated protocol
      mHandler <- lookupHandler proto
      case mHandler of
        Just handler -> handler stream (connPeerId conn)
        Nothing -> pure ()  -- Should not happen: proto was in supported list
    NoProtocol -> pure ()  -- No common protocol, stream will be closed
  where
    readProtos = readTVar (swProtocols sw)
    lookupHandler proto = atomically $
      Map.lookup proto <$> readTVar (swProtocols sw)

-- | Start listening on the given addresses.
--
-- For each address, selects a matching transport, binds a listener,
-- and spawns an accept loop that handles inbound connections.
-- Returns the actual bound addresses (port 0 resolved to actual port).
-- Fails if the switch is already closed.
switchListen :: Switch -> ConnectionGater -> [Multiaddr] -> IO [Multiaddr]
switchListen sw gater addrs = do
  closed <- atomically $ readTVar (swClosed sw)
  if closed
    then fail "switchListen: switch is closed"
    else do
      transports <- atomically $ readTVar (swTransports sw)
      activeListeners <- mapM (bindAndListen transports gater sw) addrs
      let newListeners = concat activeListeners
      atomically $ do
        existing <- readTVar (swListeners sw)
        writeTVar (swListeners sw) (existing ++ newListeners)
      pure (map alAddress newListeners)
  where
    -- Find a transport for the address, bind, and spawn accept loop
    bindAndListen transports gater' sw' addr = do
      case find (\t -> transportCanDial t addr) transports of
        Nothing -> fail $ "switchListen: no transport for " ++ show addr
        Just transport -> do
          listener <- transportListen transport addr
          loopThread <- async $ acceptLoop sw' gater' listener
          pure [ActiveListener
            { alListener   = listener
            , alAcceptLoop = loopThread
            , alAddress    = listenerAddr listener
            }]

-- | Accept loop: forever accepts connections and spawns handleInbound threads.
-- Catches exceptions from individual connections without stopping the loop.
-- Stops when the listener is closed (accept throws).
acceptLoop :: Switch -> ConnectionGater -> Listener -> IO ()
acceptLoop sw gater listener = loop
  where
    loop = do
      result <- (Right <$> listenerAccept listener)
                  `catch` (\(_ :: SomeException) -> pure (Left ()))
      case result of
        Left () -> pure ()  -- Listener closed, stop
        Right rawConn -> do
          _ <- async $ handleInbound sw gater rawConn
                         `catch` (\(_ :: SomeException) -> pure ())
          loop

-- | Get the current listen addresses from all active listeners.
switchListenAddrs :: Switch -> IO [Multiaddr]
switchListenAddrs sw = atomically $ do
  listeners <- readTVar (swListeners sw)
  pure (map alAddress listeners)
