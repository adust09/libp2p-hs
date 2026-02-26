-- | Core types for the libp2p Switch (central coordinator).
--
-- Defines connection states, direction, muxer session abstraction,
-- connection records, switch events, and the Switch itself.
-- See docs/08-switch.md for the full specification.
module Network.LibP2P.Switch.Types
  ( ConnState (..)
  , Direction (..)
  , MuxerSession (..)
  , Connection (..)
  , SwitchEvent (..)
  , StreamHandler
  , Switch (..)
  ) where

import Control.Concurrent.STM (TChan, TVar)
import Data.Map.Strict (Map)
import Network.LibP2P.Crypto.Key (KeyPair)
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr)
import Network.LibP2P.MultistreamSelect.Negotiation (ProtocolId, StreamIO)
import Network.LibP2P.Transport.Transport (Transport)

-- | Connection state machine (docs/08-switch.md §Connection States).
--
-- Connecting → ConnOpen → Closing → Closed
data ConnState
  = Connecting  -- ^ Raw transport established, upgrade in progress
  | ConnOpen    -- ^ Fully upgraded, streams can be opened/accepted
  | Closing     -- ^ Go Away sent/received, draining existing streams
  | ConnClosed  -- ^ Transport connection closed, resources freed
  deriving (Show, Eq)

-- | Direction of a connection relative to this node.
data Direction
  = Inbound   -- ^ Remote peer initiated the connection
  | Outbound  -- ^ Local node initiated the connection
  deriving (Show, Eq)

-- | Abstract muxer session interface.
--
-- Decouples Switch from a specific muxer (Yamux, mplex, etc.).
-- Each muxer implementation provides a MuxerSession adapter.
data MuxerSession = MuxerSession
  { muxOpenStream   :: !(IO StreamIO)   -- ^ Open a new outbound stream
  , muxAcceptStream :: !(IO StreamIO)   -- ^ Accept an inbound stream (blocks)
  , muxClose        :: !(IO ())         -- ^ Close the muxer session
  }

-- | An upgraded (secure + multiplexed) connection to a remote peer.
data Connection = Connection
  { connPeerId     :: !PeerId          -- ^ Remote peer identity
  , connDirection  :: !Direction        -- ^ Inbound or outbound
  , connLocalAddr  :: !Multiaddr       -- ^ Local multiaddr
  , connRemoteAddr :: !Multiaddr       -- ^ Remote multiaddr
  , connSecurity   :: !ProtocolId      -- ^ Negotiated security protocol (e.g. "/noise")
  , connMuxer      :: !ProtocolId      -- ^ Negotiated muxer protocol (e.g. "/yamux/1.0.0")
  , connSession    :: !MuxerSession    -- ^ Muxer session for opening/accepting streams
  , connState      :: !(TVar ConnState) -- ^ Mutable connection state
  }

-- | A protocol stream handler.
--
-- Receives the stream I/O and the remote peer's identity.
type StreamHandler = StreamIO -> PeerId -> IO ()

-- | Events emitted by the Switch for observability.
data SwitchEvent
  = Connected    !PeerId !Direction !Multiaddr  -- ^ Connection fully upgraded
  | Disconnected !PeerId !Direction !Multiaddr  -- ^ Connection closed
  deriving (Show, Eq)

-- | The Switch: central coordinator of the libp2p networking stack.
--
-- Manages transports, connection pool, protocol handlers, and events.
-- All mutable state is STM-based for safe concurrent access.
data Switch = Switch
  { swLocalPeerId :: !PeerId                                    -- ^ This node's peer identity
  , swIdentityKey :: !KeyPair                                   -- ^ This node's key pair
  , swTransports  :: !(TVar [Transport])                        -- ^ Registered transports
  , swConnPool    :: !(TVar (Map PeerId [Connection]))          -- ^ Active connections per peer
  , swProtocols   :: !(TVar (Map ProtocolId StreamHandler))         -- ^ Protocol registry
  , swEvents      :: !(TChan SwitchEvent)                       -- ^ Event broadcast channel
  , swClosed      :: !(TVar Bool)                               -- ^ Whether the switch is shut down
  }
