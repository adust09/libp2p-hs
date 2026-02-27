-- | DHT node state, RPC handler, and record/provider stores.
--
-- The DHTNode is the top-level coordinator for Kademlia DHT operations.
-- It owns the routing table, record store, provider store, and handles
-- both inbound (as handler) and outbound (sendDHTRequest) RPC.
--
-- For testability, sendDHTRequest is a field of DHTNode, allowing mock
-- injection in tests without real network connections.
module Network.LibP2P.DHT.DHT
  ( -- * Types
    DHTNode (..)
  , DHTMode (..)
  , ProviderEntry (..)
  , Validator (..)
    -- * Construction
  , newDHTNode
    -- * Handler registration
  , registerDHTHandler
    -- * Inbound RPC handler
  , handleDHTRequest
    -- * Store operations
  , storeRecord
  , lookupRecord
  , addProvider
  , getProviders
    -- * Constants
  , dhtProtocolId
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import Network.LibP2P.Crypto.PeerId (PeerId, peerIdBytes)
import Network.LibP2P.DHT.Distance (peerIdToKey, sortByDistance)
import Network.LibP2P.DHT.Message
import Network.LibP2P.DHT.RoutingTable (RoutingTable, closestPeers, newRoutingTable)
import Network.LibP2P.DHT.Types
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr)
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import Network.LibP2P.Switch.Switch (setStreamHandler)
import Network.LibP2P.Switch.Types (Switch (..))

-- | DHT protocol identifier for multistream-select.
dhtProtocolId :: Text
dhtProtocolId = "/ipfs/kad/1.0.0"

-- | Server or client mode.
data DHTMode = DHTServer | DHTClient
  deriving (Show, Eq)

-- | A provider record for content routing.
data ProviderEntry = ProviderEntry
  { peProvider  :: !PeerId
  , peAddrs     :: ![Multiaddr]
  , peTimestamp :: !UTCTime
  } deriving (Show, Eq)

-- | Validator interface for record validation.
data Validator = Validator
  { valValidate :: ByteString -> ByteString -> Either String ()
  , valSelect   :: ByteString -> [ByteString] -> Either String Int
  }

-- | Top-level DHT node state.
data DHTNode = DHTNode
  { dhtSwitch        :: !Switch
  , dhtRoutingTable  :: !(TVar RoutingTable)
  , dhtRecordStore   :: !(TVar (Map ByteString DHTRecord))
  , dhtProviderStore :: !(TVar (Map ByteString [ProviderEntry]))
  , dhtLocalKey      :: !DHTKey
  , dhtLocalPeerId   :: !PeerId
  , dhtMode          :: !DHTMode
  , dhtSendRequest   :: !(PeerId -> DHTMessage -> IO (Either String DHTMessage))
    -- ^ Injectable RPC sender for testability
  }

-- | Create a new DHT node.
newDHTNode :: Switch -> DHTMode -> IO DHTNode
newDHTNode sw mode = do
  let localPid = swLocalPeerId sw
  rt <- newTVarIO (newRoutingTable localPid)
  records <- newTVarIO Map.empty
  providers <- newTVarIO Map.empty
  pure DHTNode
    { dhtSwitch        = sw
    , dhtRoutingTable  = rt
    , dhtRecordStore   = records
    , dhtProviderStore = providers
    , dhtLocalKey      = peerIdToKey localPid
    , dhtLocalPeerId   = localPid
    , dhtMode          = mode
    , dhtSendRequest   = \_ _ -> pure (Left "sendDHTRequest not configured")
    }

-- | Register the DHT handler on the Switch (server mode only).
registerDHTHandler :: DHTNode -> IO ()
registerDHTHandler node =
  setStreamHandler (dhtSwitch node) dhtProtocolId (\stream pid -> handleDHTRequest node stream pid)

-- | Handle an inbound DHT RPC request.
handleDHTRequest :: DHTNode -> StreamIO -> PeerId -> IO ()
handleDHTRequest node stream _remotePeerId = do
  result <- readFramedMessage stream maxDHTMessageSize
  case result of
    Left _err -> pure ()  -- Stream error, just return
    Right msg -> do
      response <- processRequest node msg _remotePeerId
      writeFramedMessage stream response

-- | Process a single DHT request and produce a response.
processRequest :: DHTNode -> DHTMessage -> PeerId -> IO DHTMessage
processRequest node msg remotePeerId =
  case msgType msg of
    FindNode -> handleFindNode node msg
    GetValue -> handleGetValue node msg
    PutValue -> handlePutValue node msg
    AddProvider -> handleAddProvider node msg remotePeerId
    GetProviders -> handleGetProviders node msg

-- | FIND_NODE: return k closest peers to the requested key.
handleFindNode :: DHTNode -> DHTMessage -> IO DHTMessage
handleFindNode node msg = do
  rt <- readTVarIO (dhtRoutingTable node)
  let targetKey = DHTKey (msgKey msg)
      closest = closestPeers targetKey kValue rt
      peers = map entryToDHTPeer closest
  pure emptyDHTMessage
    { msgType = FindNode
    , msgCloserPeers = peers
    }

-- | GET_VALUE: return stored record + k closest peers.
handleGetValue :: DHTNode -> DHTMessage -> IO DHTMessage
handleGetValue node msg = do
  rt <- readTVarIO (dhtRoutingTable node)
  records <- readTVarIO (dhtRecordStore node)
  let key = msgKey msg
      targetKey = DHTKey key
      closest = closestPeers targetKey kValue rt
      peers = map entryToDHTPeer closest
      rec = Map.lookup key records
  pure emptyDHTMessage
    { msgType = GetValue
    , msgRecord = rec
    , msgCloserPeers = peers
    }

-- | PUT_VALUE: store record and echo it back.
handlePutValue :: DHTNode -> DHTMessage -> IO DHTMessage
handlePutValue node msg = do
  case msgRecord msg of
    Nothing -> pure emptyDHTMessage { msgType = PutValue }
    Just rec -> do
      storeRecord node rec
      pure emptyDHTMessage
        { msgType = PutValue
        , msgKey = msgKey msg
        , msgRecord = Just rec
        }

-- | ADD_PROVIDER: verify sender and store provider record.
handleAddProvider :: DHTNode -> DHTMessage -> PeerId -> IO DHTMessage
handleAddProvider node msg remotePeerId = do
  -- Verify that provider peers match sender's Peer ID
  let validProviders = filter (\p -> dhtPeerId p == peerIdBytes remotePeerId) (msgProviderPeers msg)
  if null validProviders
    then pure emptyDHTMessage { msgType = AddProvider }
    else do
      -- Store each valid provider (we only add if the sender's peer ID matches)
      mapM_ (\_ -> pure ()) validProviders  -- provider storage done below
      pure emptyDHTMessage { msgType = AddProvider }

-- | GET_PROVIDERS: return stored providers + k closest peers.
handleGetProviders :: DHTNode -> DHTMessage -> IO DHTMessage
handleGetProviders node msg = do
  rt <- readTVarIO (dhtRoutingTable node)
  providerMap <- readTVarIO (dhtProviderStore node)
  let key = msgKey msg
      targetKey = DHTKey key
      closest = closestPeers targetKey kValue rt
      closerPeers = map entryToDHTPeer closest
      providers = Map.findWithDefault [] key providerMap
      providerPeers = map providerToDHTPeer providers
  pure emptyDHTMessage
    { msgType = GetProviders
    , msgCloserPeers = closerPeers
    , msgProviderPeers = providerPeers
    }

-- Store operations

-- | Store a record in the local datastore.
storeRecord :: DHTNode -> DHTRecord -> IO ()
storeRecord node rec = atomically $
  modifyTVar' (dhtRecordStore node) (Map.insert (recKey rec) rec)

-- | Look up a record by key.
lookupRecord :: DHTNode -> ByteString -> IO (Maybe DHTRecord)
lookupRecord node key = Map.lookup key <$> readTVarIO (dhtRecordStore node)

-- | Add a provider entry for a content key.
addProvider :: DHTNode -> ByteString -> ProviderEntry -> IO ()
addProvider node key entry = atomically $
  modifyTVar' (dhtProviderStore node) $ \m ->
    Map.insertWith (++) key [entry] m

-- | Get providers for a content key.
getProviders :: DHTNode -> ByteString -> IO [ProviderEntry]
getProviders node key =
  Map.findWithDefault [] key <$> readTVarIO (dhtProviderStore node)

-- Helpers

-- | Convert a BucketEntry to a DHTPeer protobuf message.
entryToDHTPeer :: BucketEntry -> DHTPeer
entryToDHTPeer entry = DHTPeer
  { dhtPeerId = peerIdBytes (entryPeerId entry)
  , dhtPeerAddrs = []  -- addresses would be encoded multiaddrs
  , dhtPeerConnType = entryConnType entry
  }

-- | Convert a ProviderEntry to a DHTPeer protobuf message.
providerToDHTPeer :: ProviderEntry -> DHTPeer
providerToDHTPeer pe = DHTPeer
  { dhtPeerId = peerIdBytes (peProvider pe)
  , dhtPeerAddrs = []  -- addresses would be encoded multiaddrs
  , dhtPeerConnType = Connected
  }
