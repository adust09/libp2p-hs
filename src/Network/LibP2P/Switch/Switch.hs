-- | Switch core operations.
--
-- The Switch is the central coordinator of the libp2p stack.
-- This module provides construction, transport management,
-- protocol handler registration, and shutdown.
module Network.LibP2P.Switch.Switch
  ( newSwitch
  , addTransport
  , selectTransport
  , setStreamHandler
  , removeStreamHandler
  , lookupStreamHandler
  , switchClose
  ) where

import Control.Concurrent.Async (cancel)
import Control.Concurrent.STM (atomically, newBroadcastTChanIO, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException, catch)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Network.LibP2P.Crypto.Key (KeyPair)
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr)
import Network.LibP2P.MultistreamSelect.Negotiation (ProtocolId)
import Network.LibP2P.Switch.ResourceManager (DefaultLimits (..), defaultPeerLimits, defaultSystemLimits, newResourceManager)
import Network.LibP2P.Switch.Types (ActiveListener (..), StreamHandler, Switch (..))
import Network.LibP2P.Transport.Transport (Listener (..), Transport (..))

-- | Create a new Switch with the given local identity.
-- All internal state is initialized empty.
newSwitch :: PeerId -> KeyPair -> IO Switch
newSwitch pid kp = do
  transportsVar   <- newTVarIO []
  poolVar         <- newTVarIO Map.empty
  protosVar       <- newTVarIO Map.empty
  eventsChan      <- newBroadcastTChanIO
  closedVar       <- newTVarIO False
  backoffsVar     <- newTVarIO Map.empty
  pendingDialsVar <- newTVarIO Map.empty
  resMgr <- newResourceManager DefaultLimits
    { dlSystemLimits = defaultSystemLimits
    , dlPeerLimits   = defaultPeerLimits
    }
  peerStoreVar <- newTVarIO Map.empty
  notifiersVar <- newTVarIO []
  listenersVar <- newTVarIO []
  pure Switch
    { swLocalPeerId  = pid
    , swIdentityKey  = kp
    , swTransports   = transportsVar
    , swConnPool     = poolVar
    , swProtocols    = protosVar
    , swEvents       = eventsChan
    , swClosed       = closedVar
    , swDialBackoffs = backoffsVar
    , swPendingDials = pendingDialsVar
    , swResourceMgr  = resMgr
    , swPeerStore    = peerStoreVar
    , swNotifiers    = notifiersVar
    , swListeners    = listenersVar
    }

-- | Register a transport with the switch.
-- Appends to the list of transports; order matters for selectTransport.
addTransport :: Switch -> Transport -> IO ()
addTransport sw t = atomically $ do
  ts <- readTVar (swTransports sw)
  writeTVar (swTransports sw) (ts ++ [t])

-- | Find the first registered transport that can dial the given multiaddr.
selectTransport :: Switch -> Multiaddr -> IO (Maybe Transport)
selectTransport sw addr = atomically $ do
  ts <- readTVar (swTransports sw)
  pure $ find (\t -> transportCanDial t addr) ts

-- | Register a protocol stream handler.
-- Overwrites any existing handler for the same protocol ID.
setStreamHandler :: Switch -> ProtocolId -> StreamHandler -> IO ()
setStreamHandler sw proto handler = atomically $
  do protos <- readTVar (swProtocols sw)
     writeTVar (swProtocols sw) (Map.insert proto handler protos)

-- | Remove a protocol stream handler.
removeStreamHandler :: Switch -> ProtocolId -> IO ()
removeStreamHandler sw proto = atomically $
  do protos <- readTVar (swProtocols sw)
     writeTVar (swProtocols sw) (Map.delete proto protos)

-- | Look up a registered stream handler by protocol ID.
lookupStreamHandler :: Switch -> ProtocolId -> IO (Maybe StreamHandler)
lookupStreamHandler sw proto = atomically $ do
  protos <- readTVar (swProtocols sw)
  pure $ Map.lookup proto protos

-- | Shut down the switch.
-- Cancels all accept loop threads, closes all listeners, then sets the closed flag.
switchClose :: Switch -> IO ()
switchClose sw = do
  -- Read and clear listeners atomically
  listeners <- atomically $ do
    ls <- readTVar (swListeners sw)
    writeTVar (swListeners sw) []
    writeTVar (swClosed sw) True
    pure ls
  -- Cancel accept loops and close listeners outside STM
  mapM_ closeListener listeners
  where
    closeListener al = do
      cancel (alAcceptLoop al) `catch` (\(_ :: SomeException) -> pure ())
      listenerClose (alListener al) `catch` (\(_ :: SomeException) -> pure ())
