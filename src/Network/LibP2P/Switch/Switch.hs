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

import Network.LibP2P.Crypto.Key (KeyPair)
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr)
import Network.LibP2P.MultistreamSelect.Negotiation (ProtocolId)
import Network.LibP2P.Switch.Types (StreamHandler, Switch)
import Network.LibP2P.Transport.Transport (Transport)

-- | Create a new Switch with the given local identity.
newSwitch :: PeerId -> KeyPair -> IO Switch
newSwitch = error "newSwitch: not yet implemented"

-- | Register a transport with the switch.
addTransport :: Switch -> Transport -> IO ()
addTransport = error "addTransport: not yet implemented"

-- | Find a transport that can dial the given multiaddr.
selectTransport :: Switch -> Multiaddr -> IO (Maybe Transport)
selectTransport = error "selectTransport: not yet implemented"

-- | Register a protocol stream handler.
setStreamHandler :: Switch -> ProtocolId -> StreamHandler -> IO ()
setStreamHandler = error "setStreamHandler: not yet implemented"

-- | Remove a protocol stream handler.
removeStreamHandler :: Switch -> ProtocolId -> IO ()
removeStreamHandler = error "removeStreamHandler: not yet implemented"

-- | Look up a registered stream handler by protocol ID.
lookupStreamHandler :: Switch -> ProtocolId -> IO (Maybe StreamHandler)
lookupStreamHandler = error "lookupStreamHandler: not yet implemented"

-- | Shut down the switch: close all connections, release resources.
switchClose :: Switch -> IO ()
switchClose = error "switchClose: not yet implemented"
