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
  ) where

import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr)
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO)
import Network.LibP2P.Switch.Types (Connection, Switch)
import Network.LibP2P.Transport.Transport (RawConnection)

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
handleInbound _sw _gater _rawConn = pure ()  -- STUB

-- | Accept inbound streams and dispatch to registered protocol handlers.
--
-- Runs forever, accepting streams from the muxer and spawning a handler
-- thread for each. Uses multistream-select to negotiate the protocol,
-- then dispatches to the registered StreamHandler.
streamAcceptLoop :: Switch -> Connection -> IO ()
streamAcceptLoop _sw _conn = pure ()  -- STUB

-- | Dispatch a single inbound stream to the appropriate protocol handler.
dispatchStream :: Switch -> Connection -> StreamIO -> IO ()
dispatchStream _sw _conn _stream = pure ()  -- STUB

-- | Start listening on the given addresses.
--
-- For each address, selects a matching transport, binds a listener,
-- and spawns an accept loop that handles inbound connections.
-- Returns the listener addresses (with resolved ports).
switchListen :: Switch -> ConnectionGater -> [Multiaddr] -> IO [Multiaddr]
switchListen _sw _gater _addrs = pure []  -- STUB
