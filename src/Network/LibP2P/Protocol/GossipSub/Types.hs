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
    -- * Scoring types
  , TopicScoreParams (..)
  , defaultTopicScoreParams
  , PeerScoreParams (..)
  , defaultPeerScoreParams
  , TopicPeerState (..)
  , defaultTopicPeerState
  , ScoreThresholds (..)
  , defaultScoreThresholds
    -- * Message cache types
  , CacheEntry (..)
  , MessageCache (..)
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
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
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
  { psProtocol         :: !PeerProtocol              -- ^ Protocol support
  , psTopics           :: !(Set Topic)               -- ^ Subscribed topics
  , psIsOutbound       :: !Bool                      -- ^ True if we dialed this peer
  , psConnectedAt      :: !UTCTime                   -- ^ Connection establishment time
  , psTopicState       :: !(Map Topic TopicPeerState) -- ^ Per-topic scoring state
  , psBehaviorPenalty  :: !Double                     -- ^ P7 behavioral penalty counter
  , psIPAddress        :: !(Maybe ByteString)         -- ^ IP address for P6
  , psCachedScore      :: !Double                     -- ^ Cached computed score
  } deriving (Show, Eq)

-- Scoring types

-- | Per-topic scoring parameters.
data TopicScoreParams = TopicScoreParams
  { tspTopicWeight                    :: !Double          -- ^ How much this topic contributes
  , tspTimeInMeshWeight               :: !Double          -- ^ P1 weight (small positive)
  , tspTimeInMeshQuantum              :: !NominalDiffTime -- ^ P1 time unit
  , tspTimeInMeshCap                  :: !Double          -- ^ P1 maximum value
  , tspFirstMessageDeliveriesWeight   :: !Double          -- ^ P2 weight (positive)
  , tspFirstMessageDeliveriesDecay    :: !Double          -- ^ P2 decay factor
  , tspFirstMessageDeliveriesCap      :: !Double          -- ^ P2 maximum counter
  , tspMeshMessageDeliveriesWeight    :: !Double          -- ^ P3 weight (negative)
  , tspMeshMessageDeliveriesDecay     :: !Double          -- ^ P3 decay factor
  , tspMeshMessageDeliveriesThreshold :: !Double          -- ^ P3 expected threshold
  , tspMeshMessageDeliveriesCap       :: !Double          -- ^ P3 maximum counter
  , tspMeshMessageDeliveriesActivation :: !NominalDiffTime -- ^ P3 grace period
  , tspMeshMessageDeliveryWindow      :: !NominalDiffTime  -- ^ P3 near-first window
  , tspMeshFailurePenaltyWeight       :: !Double          -- ^ P3b weight (negative)
  , tspMeshFailurePenaltyDecay        :: !Double          -- ^ P3b decay factor
  , tspInvalidMessageDeliveriesWeight :: !Double          -- ^ P4 weight (negative)
  , tspInvalidMessageDeliveriesDecay  :: !Double          -- ^ P4 decay factor
  } deriving (Show, Eq)

-- | Default topic scoring parameters (conservative values).
defaultTopicScoreParams :: TopicScoreParams
defaultTopicScoreParams = TopicScoreParams
  { tspTopicWeight                     = 1.0
  , tspTimeInMeshWeight                = 0.01
  , tspTimeInMeshQuantum               = 1     -- 1 second
  , tspTimeInMeshCap                   = 100
  , tspFirstMessageDeliveriesWeight    = 1.0
  , tspFirstMessageDeliveriesDecay     = 0.5
  , tspFirstMessageDeliveriesCap       = 100
  , tspMeshMessageDeliveriesWeight     = -1.0
  , tspMeshMessageDeliveriesDecay      = 0.5
  , tspMeshMessageDeliveriesThreshold  = 1.0
  , tspMeshMessageDeliveriesCap        = 100
  , tspMeshMessageDeliveriesActivation = 5    -- 5 seconds
  , tspMeshMessageDeliveryWindow       = 0.01 -- 10 ms
  , tspMeshFailurePenaltyWeight        = -1.0
  , tspMeshFailurePenaltyDecay         = 0.5
  , tspInvalidMessageDeliveriesWeight  = -100.0
  , tspInvalidMessageDeliveriesDecay   = 0.5
  }

-- | Global peer scoring parameters.
data PeerScoreParams = PeerScoreParams
  { pspTopicParams              :: !(Map Topic TopicScoreParams)
  , pspAppSpecificWeight        :: !Double            -- ^ w5 weight
  , pspAppSpecificScore         :: !(PeerId -> Double) -- ^ P5 callback
  , pspIPColocationFactorWeight :: !Double            -- ^ w6 weight (negative)
  , pspIPColocationFactorThreshold :: !Int            -- ^ P6 threshold
  , pspBehaviorPenaltyWeight    :: !Double            -- ^ w7 weight (negative)
  , pspBehaviorPenaltyDecay     :: !Double            -- ^ w7 decay factor
  , pspDecayInterval            :: !NominalDiffTime   -- ^ How often to decay
  , pspDecayToZero              :: !Double            -- ^ Zero-out threshold
  , pspRetainScore              :: !NominalDiffTime   -- ^ Keep score after disconnect
  , pspTopicScoreCap            :: !Double            -- ^ Cap for topic score sum
  }

-- | Default global scoring parameters.
defaultPeerScoreParams :: PeerScoreParams
defaultPeerScoreParams = PeerScoreParams
  { pspTopicParams              = Map.empty
  , pspAppSpecificWeight        = 1.0
  , pspAppSpecificScore         = const 0
  , pspIPColocationFactorWeight = -10.0
  , pspIPColocationFactorThreshold = 3
  , pspBehaviorPenaltyWeight    = -1.0
  , pspBehaviorPenaltyDecay     = 0.99
  , pspDecayInterval            = 1    -- 1 second
  , pspDecayToZero              = 0.01
  , pspRetainScore              = 3600 -- 1 hour
  , pspTopicScoreCap            = 100.0
  }

-- | Per-topic per-peer state for scoring counters.
data TopicPeerState = TopicPeerState
  { tpsMeshTime                :: !NominalDiffTime  -- ^ P1: time in mesh
  , tpsFirstMessageDeliveries  :: !Double           -- ^ P2 counter
  , tpsMeshMessageDeliveries   :: !Double           -- ^ P3 counter
  , tpsMeshFailurePenalty      :: !Double           -- ^ P3b counter
  , tpsInvalidMessages         :: !Double           -- ^ P4 counter
  , tpsGraftTime               :: !(Maybe UTCTime)  -- ^ When grafted
  , tpsInMesh                  :: !Bool             -- ^ Currently in mesh?
  } deriving (Show, Eq)

-- | Default empty topic peer state.
defaultTopicPeerState :: TopicPeerState
defaultTopicPeerState = TopicPeerState
  { tpsMeshTime               = 0
  , tpsFirstMessageDeliveries = 0
  , tpsMeshMessageDeliveries  = 0
  , tpsMeshFailurePenalty     = 0
  , tpsInvalidMessages        = 0
  , tpsGraftTime              = Nothing
  , tpsInMesh                 = False
  }

-- | Score thresholds controlling router behavior.
data ScoreThresholds = ScoreThresholds
  { stGossipThreshold            :: !Double  -- ^ Below: no gossip
  , stPublishThreshold           :: !Double  -- ^ Below: no flood publish
  , stGraylistThreshold          :: !Double  -- ^ Below: ignore all RPCs
  , stAcceptPXThreshold          :: !Double  -- ^ Above: accept PX from PRUNE
  , stOpportunisticGraftThreshold :: !Double -- ^ Below: trigger opportunistic graft
  } deriving (Show, Eq)

-- | Default score thresholds.
defaultScoreThresholds :: ScoreThresholds
defaultScoreThresholds = ScoreThresholds
  { stGossipThreshold             = -100
  , stPublishThreshold            = -1000
  , stGraylistThreshold           = -10000
  , stAcceptPXThreshold           = 100
  , stOpportunisticGraftThreshold = 1
  }

-- Message cache types

-- | A cached message entry.
data CacheEntry = CacheEntry
  { ceMessageId :: !MessageId
  , ceMessage   :: !PubSubMessage
  , ceTopic     :: !Topic
  } deriving (Show, Eq)

-- | Sliding-window message cache.
data MessageCache = MessageCache
  { mcWindows :: !(Seq [CacheEntry])          -- ^ Circular windows, newest first (index 0)
  , mcIndex   :: !(Map MessageId CacheEntry)  -- ^ Fast lookup by message ID
  , mcLen     :: !Int                         -- ^ Total number of windows
  , mcGossip  :: !Int                         -- ^ Number of windows for gossip (IHAVE)
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
    -- Scoring (Phase 9b)
  , gsScoreParams :: !PeerScoreParams
  , gsThresholds  :: !ScoreThresholds
  , gsIPPeerCount :: !(TVar (Map ByteString (Set PeerId)))
    -- ^ IP address → peers sharing that IP (for P6)
    -- Message cache (Phase 9c)
  , gsMessageCache  :: !(TVar MessageCache)
    -- ^ Sliding-window message cache for IWANT and IHAVE
  , gsHeartbeatCount :: !(TVar Int)
    -- ^ Heartbeat counter (for opportunistic graft timing)
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
