-- | DHT node state, RPC handler, and record/provider stores.
--
-- The DHTNode is the top-level coordinator for Kademlia DHT operations.
-- It owns the routing table, record store, provider store, and handles
-- both inbound (as handler) and outbound (dhtSendRequest) RPC.
--
-- The outbound sender is wired to the Switch by 'newDHTNode'; it remains
-- a record field so tests can inject mocks without a real network.
module LibP2P.DHT
  ( -- * Types
    DHTNode (..)
  , DHTMode (..)
  , ProviderEntry (..)
  , Validator (..)
    -- * Validators
  , defaultValidator
  , namespacedValidator
  , pkValidator
    -- * Construction
  , newDHTNode
    -- * Handler registration
  , registerDHTHandler
    -- * Inbound RPC handler
  , handleDHTRequest
    -- * Routing table maintenance
  , addPeerToTable
    -- * Store operations
  , storeRecord
  , lookupRecord
  , addProvider
  , getProviders
    -- * Wire helpers
  , decodePeerAddrs
    -- * Constants
  , dhtProtocolId
  , providerRecordTTL
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException, catch, try)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import LibP2P.Crypto.PeerId (PeerId (..), peerIdBytes)
import LibP2P.DHT.Distance (keyToDHTKey, peerIdToKey)
import LibP2P.DHT.Message
import LibP2P.DHT.RoutingTable
  ( RoutingTable
  , allPeers
  , closestPeers
  , insertPeer
  , newRoutingTable
  , removePeer
  )
import LibP2P.DHT.Types
import LibP2P.DHT.Validator
  ( Validator (..)
  , defaultValidator
  , namespacedValidator
  , pkValidator
  )
import LibP2P.Multiaddr (Multiaddr, fromBytes, toBytes)
import LibP2P.MultistreamSelect.Negotiation
  ( NegotiationResult (..)
  , StreamIO (..)
  , negotiateInitiator
  )
import LibP2P.Switch (setStreamHandler)
import LibP2P.Switch.ConnPool (lookupConn)
import LibP2P.Switch.Types (Connection (..), MuxerSession (..), Switch (..))

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

-- | Provider record expiration interval, per specs/kad-dht (48 hours).
-- Expired entries are pruned on read in 'getProviders'.
providerRecordTTL :: NominalDiffTime
providerRecordTTL = 48 * 3600

-- | Top-level DHT node state.
data DHTNode = DHTNode
  { dhtSwitch        :: !Switch
  , dhtRoutingTable  :: !(TVar RoutingTable)
  , dhtRecordStore   :: !(TVar (Map ByteString DHTRecord))
  , dhtProviderStore :: !(TVar (Map ByteString [ProviderEntry]))
  , dhtLocalKey      :: !DHTKey
  , dhtLocalPeerId   :: !PeerId
  , dhtMode          :: !DHTMode
  , dhtValidator     :: !Validator
    -- ^ Record validator applied to PUT_VALUE records before storage
    -- (and available to GET_VALUE conflict resolution). Defaults to
    -- 'defaultValidator' (the @/pk/@ namespace).
  , dhtStreams       :: !(TVar (Map PeerId StreamIO))
    -- ^ Cached outbound @/ipfs/kad/1.0.0@ streams, one per peer
    -- (go-libp2p reuses a single long-lived stream per peer)
  , dhtSendRequest   :: !(PeerId -> DHTMessage -> IO (Either String DHTMessage))
    -- ^ Outbound RPC sender. Wired to the Switch by 'newDHTNode';
    -- kept as a field so tests can inject mocks.
  }

-- | Create a new DHT node with the outbound sender wired to the Switch.
newDHTNode :: Switch -> DHTMode -> IO DHTNode
newDHTNode sw mode = do
  let localPid = swLocalPeerId sw
  rt <- newTVarIO (newRoutingTable localPid)
  records <- newTVarIO Map.empty
  providers <- newTVarIO Map.empty
  streams <- newTVarIO Map.empty
  pure DHTNode
    { dhtSwitch        = sw
    , dhtRoutingTable  = rt
    , dhtRecordStore   = records
    , dhtProviderStore = providers
    , dhtLocalKey      = peerIdToKey localPid
    , dhtLocalPeerId   = localPid
    , dhtMode          = mode
    , dhtValidator     = defaultValidator
    , dhtStreams       = streams
    , dhtSendRequest   = sendRequestViaSwitch sw streams
    }

-- | Register the DHT handler on the Switch.
--
-- Per specs/kad-dht (client and server mode), nodes operating in client
-- mode do not offer the Kademlia protocol identifier for incoming
-- streams, so this is a no-op for 'DHTClient' nodes: they keep issuing
-- outbound queries via 'dhtSendRequest' but never serve inbound RPC.
registerDHTHandler :: DHTNode -> IO ()
registerDHTHandler node = case dhtMode node of
  DHTClient -> pure ()
  DHTServer ->
    setStreamHandler (dhtSwitch node) dhtProtocolId
      (\conn stream -> handleDHTRequest node stream (connPeerId conn))

-- | Handle an inbound DHT stream.
--
-- Per specs/kad-dht, implementations must handle additional RPC request
-- messages on the same incoming stream: go-libp2p keeps one long-lived
-- stream per peer and pipelines requests over it. Loop until the stream
-- errors, is reset, or reaches EOF.
handleDHTRequest :: DHTNode -> StreamIO -> PeerId -> IO ()
handleDHTRequest node stream remotePeerId = loop
  where
    loop = do
      result <- try $ do
        readResult <- readFramedMessage stream maxDHTMessageSize
        case readResult of
          Left err -> pure (Left err)
          Right msg -> do
            response <- processRequest node msg remotePeerId
            writeFramedMessage stream response
            -- Routing-table growth: a peer speaking the DHT protocol to
            -- us is a live contact; insert (or refresh) it. Note this
            -- cannot distinguish client-mode senders (the spec would
            -- exclude them) without identify-provided protocol lists.
            now <- getCurrentTime
            _ <- addPeerToTable node BucketEntry
              { entryPeerId   = remotePeerId
              , entryKey      = peerIdToKey remotePeerId
              , entryAddrs    = []
              , entryLastSeen = now
              , entryConnType = Connected
              }
            pure (Right ())
      case result of
        Left (_ :: SomeException) -> pure ()  -- Stream closed or reset
        Right (Left _err) -> pure ()          -- Framing/decode error: stop serving
        Right (Right ()) -> loop

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
  -- The wire key is raw (a binary peer ID); the spec distance metric is
  -- XOR over SHA-256 digests, so hash before comparing.
  let targetKey = keyToDHTKey (msgKey msg)
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
      -- Store lookup uses the raw key; distance uses its SHA-256.
      targetKey = keyToDHTKey key
      closest = closestPeers targetKey kValue rt
      peers = map entryToDHTPeer closest
      rec = Map.lookup key records
  pure emptyDHTMessage
    { msgType = GetValue
    , msgRecord = rec
    , msgCloserPeers = peers
    }

-- | PUT_VALUE: validate, store with a receiver-set timestamp, and echo.
--
-- Per specs/kad-dht (Entry validation), incoming records are validated
-- before being stored: the record key must match the message key and
-- the configured 'dhtValidator' must accept the key/value binding
-- (e.g. @/pk/@ records must carry the public key hashing to the key's
-- multihash). Rejected records are neither stored nor echoed back.
--
-- The stored record's @timeReceived@ is set by the receiver (Record
-- field 5: "Time the record was received, set by receiver"), never
-- taken from the sender's claim.
handlePutValue :: DHTNode -> DHTMessage -> IO DHTMessage
handlePutValue node msg = do
  case msgRecord msg of
    Nothing -> pure rejected
    Just rec
      | recKey rec /= msgKey msg -> pure rejected
      | otherwise ->
          case valValidate (dhtValidator node) (recKey rec) (recValue rec) of
            Left _err -> pure rejected
            Right () -> do
              now <- getCurrentTime
              storeRecord node rec { recTimeReceived = T.pack (iso8601Show now) }
              pure emptyDHTMessage
                { msgType = PutValue
                , msgKey = msgKey msg
                , msgRecord = Just rec
                }
  where
    rejected = emptyDHTMessage { msgType = PutValue }

-- | ADD_PROVIDER: verify sender and store provider record.
handleAddProvider :: DHTNode -> DHTMessage -> PeerId -> IO DHTMessage
handleAddProvider node msg remotePeerId = do
  now <- getCurrentTime
  -- Verify that provider peers match sender's Peer ID
  let validProviders = filter (\p -> dhtPeerId p == peerIdBytes remotePeerId) (msgProviderPeers msg)
  -- Store each valid provider keyed by msgKey
  mapM_ (\p -> addProvider node (msgKey msg) (dhtPeerToProvider p now)) validProviders
  pure emptyDHTMessage { msgType = AddProvider }

-- | GET_PROVIDERS: return stored providers + k closest peers.
handleGetProviders :: DHTNode -> DHTMessage -> IO DHTMessage
handleGetProviders node msg = do
  rt <- readTVarIO (dhtRoutingTable node)
  let key = msgKey msg
      -- Store lookup uses the raw key; distance uses its SHA-256.
      targetKey = keyToDHTKey key
      closest = closestPeers targetKey kValue rt
      closerPeers = map entryToDHTPeer closest
  -- getProviders prunes entries past the 48h provider TTL on read.
  providers <- getProviders node key
  let providerPeers = map providerToDHTPeer providers
  pure emptyDHTMessage
    { msgType = GetProviders
    , msgCloserPeers = closerPeers
    , msgProviderPeers = providerPeers
    }

-- Routing table maintenance

-- | Insert a peer into the routing table, applying the Kademlia
-- full-bucket eviction policy.
--
-- When the target bucket is full, the least-recently-seen peer is
-- probed with a FIND_NODE request (the DHT liveness check; our
-- 'MessageType' has no PING, and go-libp2p likewise treats any
-- successful RPC as proof of liveness):
--
-- * if the LRS peer answers, it is kept (refreshed to
--   most-recently-seen) and the new peer is dropped ('BucketFull');
-- * if it does not answer, it is evicted and the new peer takes its
--   place ('Inserted').
addPeerToTable :: DHTNode -> BucketEntry -> IO InsertResult
addPeerToTable node entry = do
  result <- atomically $ do
    rt <- readTVar (dhtRoutingTable node)
    let (rt', res) = insertPeer entry rt
    writeTVar (dhtRoutingTable node) rt'
    pure res
  case result of
    BucketFull lrs -> evictOrKeep lrs
    other -> pure other
  where
    evictOrKeep lrs = do
      let ping = emptyDHTMessage { msgType = FindNode, msgKey = peerIdBytes lrs }
      response <- dhtSendRequest node lrs ping
        `catch` \(e :: SomeException) -> pure (Left (show e))
      case response of
        Right _ -> do
          -- LRS peer is alive: move it to most-recently-seen and drop
          -- the newcomer.
          now <- getCurrentTime
          atomically $ modifyTVar' (dhtRoutingTable node) (refreshPeer lrs now)
          pure (BucketFull lrs)
        Left _ -> do
          -- LRS peer is dead: evict it and insert the newcomer.
          atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
            fst (insertPeer entry (removePeer lrs rt))
          pure Inserted

-- | Move an existing peer to the most-recently-seen position of its
-- bucket with a fresh last-seen timestamp. No-op if the peer is gone.
refreshPeer :: PeerId -> UTCTime -> RoutingTable -> RoutingTable
refreshPeer pid now rt =
  case filter ((== pid) . entryPeerId) (allPeers rt) of
    (e : _) -> fst (insertPeer e { entryLastSeen = now } rt)
    [] -> rt

-- Outbound RPC

-- | Send a DHT request to a peer over the Switch.
--
-- Reuses a cached @/ipfs/kad/1.0.0@ stream per peer when one exists
-- (go-libp2p pipelines all requests to a peer over one stream); otherwise
-- opens a new muxer stream on an existing connection and negotiates the
-- protocol. A failed exchange on a cached stream evicts it and retries
-- once on a fresh stream.
sendRequestViaSwitch
  :: Switch
  -> TVar (Map PeerId StreamIO)
  -> PeerId
  -> DHTMessage
  -> IO (Either String DHTMessage)
sendRequestViaSwitch sw streamsVar pid request = do
  mCached <- Map.lookup pid <$> readTVarIO streamsVar
  case mCached of
    Nothing -> openAndExchange
    Just stream -> do
      result <- exchangeFramed stream request
      case result of
        Right resp -> pure (Right resp)
        Left _ -> do
          -- Cached stream is dead: evict it and retry on a fresh one.
          atomically $ modifyTVar' streamsVar (Map.delete pid)
          openAndExchange
  where
    openAndExchange = do
      opened <- openDHTStream sw pid
      case opened of
        Left err -> pure (Left err)
        Right stream -> do
          atomically $ modifyTVar' streamsVar (Map.insert pid stream)
          result <- exchangeFramed stream request
          case result of
            Left err -> do
              atomically $ modifyTVar' streamsVar (Map.delete pid)
              pure (Left err)
            ok -> pure ok

-- | Write a framed request and read the framed response, capturing IO errors.
exchangeFramed :: StreamIO -> DHTMessage -> IO (Either String DHTMessage)
exchangeFramed stream request = do
  result <- try $ do
    writeFramedMessage stream request
    readFramedMessage stream maxDHTMessageSize
  pure $ case result of
    Left (e :: SomeException) -> Left ("DHT stream I/O failed: " ++ show e)
    Right r -> r

-- | Open a new muxer stream to the peer and negotiate @/ipfs/kad/1.0.0@.
openDHTStream :: Switch -> PeerId -> IO (Either String StreamIO)
openDHTStream sw pid = do
  mConn <- atomically $ lookupConn (swConnPool sw) pid
  case mConn of
    Nothing -> pure (Left "no open connection to peer")
    Just conn -> do
      result <- try $ do
        stream <- muxOpenStream (connSession conn)
        negotiated <- negotiateInitiator stream [dhtProtocolId]
        pure (stream, negotiated)
      pure $ case result of
        Left (e :: SomeException) -> Left ("failed to open DHT stream: " ++ show e)
        Right (stream, Accepted _) -> Right stream
        Right (_, NoProtocol) -> Left "peer does not support /ipfs/kad/1.0.0"

-- Store operations

-- | Store a record in the local datastore.
storeRecord :: DHTNode -> DHTRecord -> IO ()
storeRecord node rec = atomically $
  modifyTVar' (dhtRecordStore node) (Map.insert (recKey rec) rec)

-- | Look up a record by key.
lookupRecord :: DHTNode -> ByteString -> IO (Maybe DHTRecord)
lookupRecord node key = Map.lookup key <$> readTVarIO (dhtRecordStore node)

-- | Add a provider entry for a content key.
--
-- A provider republishing on schedule replaces its previous entry
-- (deduplicated by peer ID) instead of appending a duplicate, so the
-- entry's timestamp is refreshed and GET_PROVIDERS responses stay
-- bounded.
addProvider :: DHTNode -> ByteString -> ProviderEntry -> IO ()
addProvider node key entry = atomically $
  modifyTVar' (dhtProviderStore node) $ \m ->
    Map.insertWith merge key [entry] m
  where
    merge new old = new ++ filter (\e -> peProvider e /= peProvider entry) old

-- | Get providers for a content key, pruning entries older than
-- 'providerRecordTTL' (48h expiration interval per specs/kad-dht).
getProviders :: DHTNode -> ByteString -> IO [ProviderEntry]
getProviders node key = do
  now <- getCurrentTime
  atomically $ do
    m <- readTVar (dhtProviderStore node)
    let live = filter (\e -> diffUTCTime now (peTimestamp e) < providerRecordTTL)
                      (Map.findWithDefault [] key m)
        m' = if null live then Map.delete key m else Map.insert key live m
    writeTVar (dhtProviderStore node) m'
    pure live

-- Helpers

-- | Convert a BucketEntry to a DHTPeer protobuf message.
-- Per specs/kad-dht, Peer records carry the peer's known multiaddrs so
-- the requester can dial them (go-libp2p filters address-less peers).
entryToDHTPeer :: BucketEntry -> DHTPeer
entryToDHTPeer entry = DHTPeer
  { dhtPeerId = peerIdBytes (entryPeerId entry)
  , dhtPeerAddrs = map toBytes (entryAddrs entry)
  , dhtPeerConnType = entryConnType entry
  }

-- | Convert a DHTPeer from ADD_PROVIDER into a ProviderEntry.
dhtPeerToProvider :: DHTPeer -> UTCTime -> ProviderEntry
dhtPeerToProvider peer now = ProviderEntry
  { peProvider  = PeerId (dhtPeerId peer)
  , peAddrs     = decodePeerAddrs (dhtPeerAddrs peer)
  , peTimestamp = now
  }

-- | Convert a ProviderEntry to a DHTPeer protobuf message.
providerToDHTPeer :: ProviderEntry -> DHTPeer
providerToDHTPeer pe = DHTPeer
  { dhtPeerId = peerIdBytes (peProvider pe)
  , dhtPeerAddrs = map toBytes (peAddrs pe)
  , dhtPeerConnType = Connected
  }

-- | Decode raw wire multiaddrs from a Peer record, dropping any that fail
-- to parse: a malformed address from a remote peer must not poison the
-- rest of the record.
decodePeerAddrs :: [ByteString] -> [Multiaddr]
decodePeerAddrs raw = [addr | Right addr <- map fromBytes raw]
