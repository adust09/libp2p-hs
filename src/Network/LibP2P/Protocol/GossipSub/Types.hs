-- | GossipSub core types and parameters (docs/11-pubsub.md).
--
-- Protocol ID: /meshsub/1.1.0
--
-- GossipSub constructs two overlapping overlay networks per topic:
-- a full-message mesh (eager push, degree D=6) and a metadata-only
-- gossip layer (lazy push via IHAVE/IWANT). This module defines
-- all wire types, configuration parameters, per-peer state, and
-- the router state record.
module Network.LibP2P.Protocol.GossipSub.Types
  ( -- * Protocol constants
    gossipSubProtocolId
  , maxRPCSize
    -- * Topic and message identity
  , Topic
  , MessageId
    -- * Wire types
  , PubSubMessage (..)
  , SubOpts (..)
  , ControlMessage (..)
  , IHave (..)
  , IWant (..)
  , Graft (..)
  , Prune (..)
  , PeerExchangeInfo (..)
  , RPC (..)
    -- * Signature policy
  , SignaturePolicy (..)
    -- * Configuration
  , GossipSubParams (..)
  , defaultGossipSubParams
    -- * Peer tracking
  , PeerProtocol (..)
  , PeerState (..)
    -- * Router state
  , GossipSubRouter (..)
    -- * Defaults
  , emptyRPC
  , emptyControlMessage
  , defaultMessageId
  ) where

import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Word (Word64)
import Network.LibP2P.Crypto.PeerId (PeerId)

-- | GossipSub v1.1 protocol identifier.
gossipSubProtocolId :: Text
gossipSubProtocolId = "/meshsub/1.1.0"

-- | Maximum RPC message size: 1 MiB.
maxRPCSize :: Int
maxRPCSize = 1048576

-- | Topic identifier (opaque string).
type Topic = Text

-- | Unique message identifier (typically from || seqno).
type MessageId = ByteString

-- Wire types

-- | A published message.
data PubSubMessage = PubSubMessage
  { msgFrom      :: !(Maybe ByteString)   -- ^ Original author's Peer ID bytes
  , msgData      :: !ByteString           -- ^ Application payload
  , msgSeqNo     :: !(Maybe ByteString)   -- ^ 8-byte big-endian sequence number
  , msgTopic     :: !Topic                -- ^ Topic this message belongs to
  , msgSignature :: !(Maybe ByteString)   -- ^ Signature over marshalled message
  , msgKey       :: !(Maybe ByteString)   -- ^ Author's public key (protobuf-encoded)
  } deriving (Show, Eq)

-- | Subscription change announcement.
data SubOpts = SubOpts
  { subSubscribe :: !Bool    -- ^ True = subscribe, False = unsubscribe
  , subTopicId   :: !Topic   -- ^ Topic identifier
  } deriving (Show, Eq)

-- | IHAVE: announce message IDs for a topic.
data IHave = IHave
  { ihaveTopic      :: !Topic        -- ^ Topic ID
  , ihaveMessageIds :: ![MessageId]  -- ^ Message IDs available
  } deriving (Show, Eq)

-- | IWANT: request full messages by ID.
data IWant = IWant
  { iwantMessageIds :: ![MessageId]  -- ^ Requested message IDs
  } deriving (Show, Eq)

-- | GRAFT: request mesh membership.
data Graft = Graft
  { graftTopic :: !Topic  -- ^ Topic to graft into
  } deriving (Show, Eq)

-- | PRUNE: remove from mesh with optional peer exchange and backoff.
data Prune = Prune
  { pruneTopic   :: !Topic                -- ^ Topic to prune from
  , prunePeers   :: ![PeerExchangeInfo]   -- ^ Peer exchange (v1.1)
  , pruneBackoff :: !(Maybe Word64)       -- ^ Backoff duration in seconds (v1.1)
  } deriving (Show, Eq)

-- | Peer exchange info in PRUNE messages (v1.1).
data PeerExchangeInfo = PeerExchangeInfo
  { pxPeerId            :: !ByteString          -- ^ Peer ID bytes
  , pxSignedPeerRecord  :: !(Maybe ByteString)  -- ^ Optional signed peer record
  } deriving (Show, Eq)

-- | GossipSub control message (IHAVE, IWANT, GRAFT, PRUNE).
data ControlMessage = ControlMessage
  { ctrlIHave :: ![IHave]
  , ctrlIWant :: ![IWant]
  , ctrlGraft :: ![Graft]
  , ctrlPrune :: ![Prune]
  } deriving (Show, Eq)

-- | RPC wrapper containing subscriptions, published messages, and control.
data RPC = RPC
  { rpcSubscriptions :: ![SubOpts]
  , rpcPublish       :: ![PubSubMessage]
  , rpcControl       :: !(Maybe ControlMessage)
  } deriving (Show, Eq)

-- Signature policy

-- | Signature policy for message signing/validation.
data SignaturePolicy
  = StrictSign       -- ^ All messages must be signed
  | StrictNoSign     -- ^ No signatures allowed
  deriving (Show, Eq)

-- Configuration

-- | GossipSub router parameters.
data GossipSubParams = GossipSubParams
  { paramD                  :: !Int              -- ^ Desired mesh degree (default 6)
  , paramDlo                :: !Int              -- ^ Lower bound (default 4)
  , paramDhi                :: !Int              -- ^ Upper bound (default 12)
  , paramDlazy              :: !Int              -- ^ Gossip emission peers (default 6)
  , paramDscore             :: !Int              -- ^ Score-retained peers (default 4)
  , paramDout               :: !Int              -- ^ Minimum outbound (default 2)
  , paramHeartbeatInterval  :: !NominalDiffTime  -- ^ Heartbeat period (default 1s)
  , paramFanoutTTL          :: !NominalDiffTime  -- ^ Fanout expiry (default 60s)
  , paramSeenTTL            :: !NominalDiffTime  -- ^ Seen cache TTL (default 120s)
  , paramPruneBackoff       :: !NominalDiffTime  -- ^ Prune backoff (default 60s)
  , paramUnsubBackoff       :: !NominalDiffTime  -- ^ Unsubscribe backoff (default 10s)
  , paramGossipFactor       :: !Double           -- ^ Gossip target fraction (default 0.25)
  , paramFloodPublish       :: !Bool             -- ^ Flood own messages (default True)
  , paramMessageIdFn        :: !(PubSubMessage -> MessageId)
    -- ^ Message ID function (default: from || seqno)
  , paramSignaturePolicy    :: !SignaturePolicy  -- ^ Signing policy (default StrictSign)
  , paramMcacheLen          :: !Int              -- ^ Message cache windows (default 5)
  , paramMcacheGossip       :: !Int              -- ^ Gossip windows (default 3)
  }

-- | Default message ID: concatenation of from and seqno fields.
defaultMessageId :: PubSubMessage -> MessageId
defaultMessageId msg =
  let from = maybe BS.empty id (msgFrom msg)
      seqno = maybe BS.empty id (msgSeqNo msg)
  in from <> seqno

-- | Default GossipSub parameters per spec.
defaultGossipSubParams :: GossipSubParams
defaultGossipSubParams = GossipSubParams
  { paramD                 = 6
  , paramDlo               = 4
  , paramDhi               = 12
  , paramDlazy             = 6
  , paramDscore            = 4
  , paramDout              = 2
  , paramHeartbeatInterval = 1
  , paramFanoutTTL         = 60
  , paramSeenTTL           = 120
  , paramPruneBackoff      = 60
  , paramUnsubBackoff      = 10
  , paramGossipFactor      = 0.25
  , paramFloodPublish      = True
  , paramMessageIdFn       = defaultMessageId
  , paramSignaturePolicy   = StrictSign
  , paramMcacheLen         = 5
  , paramMcacheGossip      = 3
  }

-- Peer tracking

-- | Peer's protocol capability.
data PeerProtocol
  = GossipSubPeer   -- ^ Supports GossipSub (/meshsub/1.1.0)
  | FloodSubPeer    -- ^ Supports FloodSub only
  deriving (Show, Eq)

-- | Per-peer state tracked by the router.
data PeerState = PeerState
  { psProtocol    :: !PeerProtocol   -- ^ Protocol support
  , psTopics      :: !(Set Topic)    -- ^ Subscribed topics
  , psIsOutbound  :: !Bool           -- ^ True if we dialed this peer
  , psConnectedAt :: !UTCTime        -- ^ Connection establishment time
  } deriving (Show, Eq)

-- Router state

-- | GossipSub router state with STM-managed concurrent state.
data GossipSubRouter = GossipSubRouter
  { gsParams      :: !GossipSubParams
  , gsLocalPeerId :: !PeerId
    -- Mesh and fanout state
  , gsMesh        :: !(TVar (Map Topic (Set PeerId)))
  , gsFanout      :: !(TVar (Map Topic (Set PeerId)))
  , gsFanoutPub   :: !(TVar (Map Topic UTCTime))
    -- Peer tracking
  , gsPeers       :: !(TVar (Map PeerId PeerState))
    -- Message deduplication
  , gsSeen        :: !(TVar (Map MessageId UTCTime))
    -- Backoff state
  , gsBackoff     :: !(TVar (Map (PeerId, Topic) UTCTime))
    -- Injectable functions for testability
  , gsSendRPC     :: !(PeerId -> RPC -> IO ())
    -- ^ Fire-and-forget RPC sender
  , gsGetTime     :: !(IO UTCTime)
    -- ^ Time source (injectable for deterministic tests)
  , gsOnMessage   :: !(TVar (Topic -> PubSubMessage -> IO ()))
    -- ^ Application message callback
  }

-- Defaults

-- | Empty RPC with no subscriptions, messages, or control.
emptyRPC :: RPC
emptyRPC = RPC [] [] Nothing

-- | Empty control message with no IHAVE/IWANT/GRAFT/PRUNE.
emptyControlMessage :: ControlMessage
emptyControlMessage = ControlMessage [] [] [] []
