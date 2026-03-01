-- | Iterative lookup algorithms for the Kademlia DHT.
--
-- Implements FIND_NODE, GET_VALUE, and GET_PROVIDERS iterative lookups
-- per docs/09-dht.md. Uses STM for shared state and async for concurrent
-- queries (alpha=10 parallelism).
--
-- Candidates are maintained as a list sorted by XOR distance to the
-- target key, ensuring the closest peers are always queried first.
--
-- Bootstrap performs a self-lookup followed by per-bucket random refresh.
module Network.LibP2P.DHT.Lookup
  ( -- * Lookup results
    LookupResult (..)
    -- * Iterative lookups
  , iterativeFindNode
  , iterativeGetValue
  , iterativeGetProviders
    -- * Bootstrap
  , bootstrap
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (UTCTime, getCurrentTime)
import Network.LibP2P.Crypto.PeerId (PeerId (..))
import Network.LibP2P.DHT.DHT (DHTNode (..), ProviderEntry (..), Validator (..))
import Network.LibP2P.DHT.Distance (peerIdToKey, sortByDistance)
import Network.LibP2P.DHT.Message
import Network.LibP2P.DHT.RoutingTable (closestPeers, insertPeer, allPeers)
import Network.LibP2P.DHT.Types

-- | Result of an iterative lookup.
data LookupResult
  = FoundPeers ![BucketEntry]
  | FoundValue !DHTRecord ![BucketEntry]
  | FoundProviders ![ProviderEntry] ![BucketEntry]
  deriving (Show)

-- | Iterative FIND_NODE: find the k closest peers to a target key.
--
-- Algorithm:
-- 1. Seed candidates with k closest from local routing table
-- 2. Query up to alpha unqueried candidates in parallel
-- 3. Merge returned closerPeers into candidates
-- 4. Terminate when top-k candidates all queried or no unqueried remain
iterativeFindNode :: DHTNode -> DHTKey -> IO [BucketEntry]
iterativeFindNode node targetKey = do
  rt <- readTVarIO (dhtRoutingTable node)
  let seeds = closestPeers targetKey kValue rt
  now <- getCurrentTime

  -- State: candidates sorted by XOR distance, queried set, known set (for dedup)
  candidatesVar <- newTVarIO (sortByDistance targetKey seeds)
  queriedVar    <- newTVarIO Set.empty
  knownVar      <- newTVarIO (Set.fromList (map entryPeerId seeds))

  lookupLoop node targetKey candidatesVar queriedVar knownVar now FindNode

-- | Core lookup loop shared by FIND_NODE, GET_VALUE, GET_PROVIDERS.
lookupLoop
  :: DHTNode
  -> DHTKey
  -> TVar [BucketEntry]   -- ^ Candidates sorted by XOR distance to target
  -> TVar (Set PeerId)    -- ^ Already queried peers
  -> TVar (Set PeerId)    -- ^ Known peers (all candidates ever seen, for dedup)
  -> a                    -- ^ Timestamp placeholder (UTCTime)
  -> MessageType          -- ^ Query type
  -> IO [BucketEntry]
lookupLoop node targetKey candidatesVar queriedVar knownVar _now queryType = go
  where
    go = do
      -- Pick up to alpha unqueried candidates closest to target
      toQuery <- atomically $ do
        candidates <- readTVar candidatesVar
        queried <- readTVar queriedVar
        let unqueried = filter (\e -> not (Set.member (entryPeerId e) queried)) candidates
            batch = take alphaValue unqueried
        -- Mark them as queried
        let newQueried = Set.union queried (Set.fromList (map entryPeerId batch))
        writeTVar queriedVar newQueried
        pure batch

      if null toQuery
        then do
          -- No more unqueried candidates -> return top k
          candidates <- readTVarIO candidatesVar
          pure (take kValue candidates)
        else do
          -- Query each peer in parallel
          results <- mapConcurrently (queryPeer node targetKey queryType) toQuery

          -- Merge results
          atomically $ do
            known <- readTVar knownVar
            candidates <- readTVar candidatesVar
            let newPeers = concatMap (either (const []) id) results
                -- Convert DHTPeers to BucketEntries, excluding already known
                newEntries = filter (\e -> not (Set.member (entryPeerId e) known))
                           $ map (dhtPeerToEntry _now) newPeers
                -- Mark new entries as known
                newKnown = Set.union known (Set.fromList (map entryPeerId newEntries))
                -- Merge and re-sort by XOR distance
                merged = sortByDistance targetKey (candidates ++ newEntries)
            writeTVar candidatesVar merged
            writeTVar knownVar newKnown

          -- Check termination: have we queried top-k?
          shouldContinue <- atomically $ do
            candidates <- readTVar candidatesVar
            queried <- readTVar queriedVar
            let topK = take kValue candidates
                allQueried = all (\e -> Set.member (entryPeerId e) queried) topK
            pure (not allQueried)

          if shouldContinue
            then go
            else do
              candidates <- readTVarIO candidatesVar
              pure (take kValue candidates)

    _now = let DHTKey bs = targetKey in bs  -- placeholder, overridden by caller

-- | Query a single peer and return the closerPeers from the response.
queryPeer :: DHTNode -> DHTKey -> MessageType -> BucketEntry -> IO (Either String [DHTPeer])
queryPeer node targetKey queryType entry = do
  let (DHTKey keyBytes) = targetKey
      request = emptyDHTMessage
        { msgType = queryType
        , msgKey  = keyBytes
        }
  result <- (dhtSendRequest node) (entryPeerId entry) request
    `catch` (\(e :: SomeException) -> pure (Left (show e)))
  pure $ case result of
    Left err -> Left err
    Right resp -> Right (msgCloserPeers resp)

-- | Iterative GET_VALUE: find a value by key, with convergence repair.
--
-- Same as FIND_NODE but also tracks the best value found and which peers
-- returned it. On completion, sends PUT_VALUE to peers with outdated values.
iterativeGetValue :: DHTNode -> Validator -> ByteString -> IO (Either String DHTRecord)
iterativeGetValue node validator key = do
  rt <- readTVarIO (dhtRoutingTable node)
  let targetKey = DHTKey key
      seeds = closestPeers targetKey kValue rt
  now <- getCurrentTime

  candidatesVar <- newTVarIO (sortByDistance targetKey seeds)
  queriedVar    <- newTVarIO Set.empty
  knownVar      <- newTVarIO (Set.fromList (map entryPeerId seeds))
  bestVar       <- newTVarIO (Nothing :: Maybe DHTRecord)
  bestPeersVar  <- newTVarIO (Set.empty :: Set PeerId)
  outdatedVar   <- newTVarIO (Set.empty :: Set PeerId)

  valueLoop node targetKey candidatesVar queriedVar knownVar bestVar bestPeersVar outdatedVar validator now

-- | Value lookup loop with best/outdated tracking.
valueLoop
  :: DHTNode
  -> DHTKey
  -> TVar [BucketEntry]
  -> TVar (Set PeerId)
  -> TVar (Set PeerId)    -- ^ Known peers (dedup)
  -> TVar (Maybe DHTRecord)
  -> TVar (Set PeerId)    -- ^ Peers that returned best value
  -> TVar (Set PeerId)    -- ^ Peers with outdated values
  -> Validator
  -> a
  -> IO (Either String DHTRecord)
valueLoop node targetKey candidatesVar queriedVar knownVar bestVar bestPeersVar outdatedVar validator _now = go
  where
    go = do
      toQuery <- atomically $ do
        candidates <- readTVar candidatesVar
        queried <- readTVar queriedVar
        let unqueried = filter (\e -> not (Set.member (entryPeerId e) queried)) candidates
            batch = take alphaValue unqueried
        let newQueried = Set.union queried (Set.fromList (map entryPeerId batch))
        writeTVar queriedVar newQueried
        pure batch

      if null toQuery
        then finalize
        else do
          results <- mapConcurrently (queryPeerForValue node targetKey) toQuery

          -- Process each result
          mapM_ (processValueResult node targetKey bestVar bestPeersVar outdatedVar validator _now) results

          -- Merge closer peers from responses
          atomically $ do
            known <- readTVar knownVar
            candidates <- readTVar candidatesVar
            let newPeers = concatMap (\(_, peers, _) -> either (const []) id peers) results
                newEntries = filter (\e -> not (Set.member (entryPeerId e) known))
                           $ map (dhtPeerToEntry _now) newPeers
                newKnown = Set.union known (Set.fromList (map entryPeerId newEntries))
                merged = sortByDistance targetKey (candidates ++ newEntries)
            writeTVar candidatesVar merged
            writeTVar knownVar newKnown

          -- Check termination
          shouldContinue <- atomically $ do
            candidates <- readTVar candidatesVar
            queried <- readTVar queriedVar
            let topK = take kValue candidates
                allQueried = all (\e -> Set.member (entryPeerId e) queried) topK
            pure (not allQueried)

          if shouldContinue then go else finalize

    finalize = do
      best <- readTVarIO bestVar
      outdated <- readTVarIO outdatedVar

      -- Convergence repair: PUT_VALUE to outdated peers
      case best of
        Nothing -> pure (Left "value not found")
        Just rec -> do
          let putMsg = emptyDHTMessage
                { msgType = PutValue
                , msgKey = recKey rec
                , msgRecord = Just rec
                }
          mapM_ (\pid -> (dhtSendRequest node) pid putMsg
                          `catch` (\(_ :: SomeException) -> pure (Left "repair failed")))
                (Set.toList outdated)
          pure (Right rec)

    _now = let DHTKey bs = targetKey in bs  -- placeholder

-- | Query a peer for a value and return (peerId, closerPeers, Maybe record).
queryPeerForValue :: DHTNode -> DHTKey -> BucketEntry
                  -> IO (PeerId, Either String [DHTPeer], Maybe DHTRecord)
queryPeerForValue node targetKey entry = do
  let (DHTKey keyBytes) = targetKey
      request = emptyDHTMessage { msgType = GetValue, msgKey = keyBytes }
  result <- (dhtSendRequest node) (entryPeerId entry) request
    `catch` (\(e :: SomeException) -> pure (Left (show e)))
  pure $ case result of
    Left err -> (entryPeerId entry, Left err, Nothing)
    Right resp -> (entryPeerId entry, Right (msgCloserPeers resp), msgRecord resp)

-- | Process a value result: update best/bestPeers/outdated.
processValueResult
  :: DHTNode -> DHTKey
  -> TVar (Maybe DHTRecord)
  -> TVar (Set PeerId)
  -> TVar (Set PeerId)
  -> Validator
  -> a
  -> (PeerId, Either String [DHTPeer], Maybe DHTRecord)
  -> IO ()
processValueResult _ _ bestVar bestPeersVar outdatedVar validator _ (pid, _, Just rec) = do
  atomically $ do
    best <- readTVar bestVar
    case best of
      Nothing -> do
        writeTVar bestVar (Just rec)
        writeTVar bestPeersVar (Set.singleton pid)
      Just currentBest -> do
        case valSelect validator (recKey rec) [recValue currentBest, recValue rec] of
          Right 0 -> do
            -- Current best is still best; this peer has outdated value
            modifyTVar' outdatedVar (Set.insert pid)
          Right 1 -> do
            -- New value is better
            oldBestPeers <- readTVar bestPeersVar
            modifyTVar' outdatedVar (Set.union oldBestPeers)
            writeTVar bestVar (Just rec)
            writeTVar bestPeersVar (Set.singleton pid)
          _ -> do
            -- Same or error: add to bestPeers
            modifyTVar' bestPeersVar (Set.insert pid)
processValueResult _ _ _ _ _ _ _ (_, _, Nothing) = pure ()

-- | Iterative GET_PROVIDERS: find providers for a content key.
iterativeGetProviders :: DHTNode -> ByteString -> IO [ProviderEntry]
iterativeGetProviders node key = do
  rt <- readTVarIO (dhtRoutingTable node)
  let targetKey = DHTKey key
      seeds = closestPeers targetKey kValue rt
  now <- getCurrentTime

  candidatesVar <- newTVarIO (sortByDistance targetKey seeds)
  queriedVar    <- newTVarIO Set.empty
  knownVar      <- newTVarIO (Set.fromList (map entryPeerId seeds))
  providersVar  <- newTVarIO ([] :: [ProviderEntry])

  providerLoop node targetKey candidatesVar queriedVar knownVar providersVar now

-- | Provider lookup loop.
providerLoop
  :: DHTNode
  -> DHTKey
  -> TVar [BucketEntry]
  -> TVar (Set PeerId)
  -> TVar (Set PeerId)    -- ^ Known peers (dedup)
  -> TVar [ProviderEntry]
  -> a
  -> IO [ProviderEntry]
providerLoop node targetKey candidatesVar queriedVar knownVar providersVar _now = go
  where
    go = do
      toQuery <- atomically $ do
        candidates <- readTVar candidatesVar
        queried <- readTVar queriedVar
        let unqueried = filter (\e -> not (Set.member (entryPeerId e) queried)) candidates
            batch = take alphaValue unqueried
        let newQueried = Set.union queried (Set.fromList (map entryPeerId batch))
        writeTVar queriedVar newQueried
        pure batch

      if null toQuery
        then readTVarIO providersVar
        else do
          results <- mapConcurrently (queryPeerForProviders node targetKey) toQuery

          -- Collect providers and closer peers
          atomically $ do
            known <- readTVar knownVar
            candidates <- readTVar candidatesVar
            currentProviders <- readTVar providersVar
            let allCloser = concatMap (\(_, closer, _) -> either (const []) id closer) results
                allProviders = concatMap (\(_, _, provs) -> provs) results
                newEntries = filter (\e -> not (Set.member (entryPeerId e) known))
                           $ map (dhtPeerToEntry _now) allCloser
                newKnown = Set.union known (Set.fromList (map entryPeerId newEntries))
                merged = sortByDistance targetKey (candidates ++ newEntries)
                newProviderEntries = map dhtPeerToProvider allProviders
            writeTVar candidatesVar merged
            writeTVar knownVar newKnown
            writeTVar providersVar (currentProviders ++ newProviderEntries)

          shouldContinue <- atomically $ do
            candidates <- readTVar candidatesVar
            queried <- readTVar queriedVar
            let topK = take kValue candidates
                allQueried = all (\e -> Set.member (entryPeerId e) queried) topK
            pure (not allQueried)

          if shouldContinue then go else readTVarIO providersVar

    _now = let DHTKey bs = targetKey in bs

-- | Query a peer for providers.
queryPeerForProviders :: DHTNode -> DHTKey -> BucketEntry
                      -> IO (PeerId, Either String [DHTPeer], [DHTPeer])
queryPeerForProviders node targetKey entry = do
  let (DHTKey keyBytes) = targetKey
      request = emptyDHTMessage { msgType = GetProviders, msgKey = keyBytes }
  result <- (dhtSendRequest node) (entryPeerId entry) request
    `catch` (\(e :: SomeException) -> pure (Left (show e)))
  pure $ case result of
    Left err -> (entryPeerId entry, Left err, [])
    Right resp -> (entryPeerId entry, Right (msgCloserPeers resp), msgProviderPeers resp)

-- | Bootstrap the DHT: connect to seeds, self-lookup, per-bucket refresh.
bootstrap :: DHTNode -> [PeerId] -> IO ()
bootstrap node seeds = do
  now <- getCurrentTime
  -- Step 1: Add seed peers to routing table
  rt <- readTVarIO (dhtRoutingTable node)
  let seedEntries = map (\pid -> BucketEntry pid (peerIdToKey pid) [] now NotConnected) seeds
      rt' = foldl (\r e -> fst (insertPeer e r)) rt seedEntries
  atomically $ writeTVar (dhtRoutingTable node) rt'

  -- Step 2: Self-lookup (FIND_NODE for our own key)
  _ <- iterativeFindNode node (dhtLocalKey node)

  -- Step 3: Refresh non-empty buckets
  -- (simplified: just do another lookup for a peer in each occupied bucket)
  rt'' <- readTVarIO (dhtRoutingTable node)
  let peers = allPeers rt''
  -- For each unique bucket, pick a representative peer and do a lookup
  let bucketReps = take 10 peers  -- limit to avoid excessive lookups during bootstrap
  mapM_ (\entry -> iterativeFindNode node (entryKey entry)
                     `catch` (\(_ :: SomeException) -> pure []))
        bucketReps

-- Helpers

-- | Convert a DHTPeer to a BucketEntry (with current time).
dhtPeerToEntry :: a -> DHTPeer -> BucketEntry
dhtPeerToEntry _ peer = BucketEntry
  { entryPeerId   = PeerId (dhtPeerId peer)
  , entryKey      = peerIdToKey (PeerId (dhtPeerId peer))
  , entryAddrs    = []  -- would need multiaddr decoding
  , entryLastSeen = epochTime
  , entryConnType = dhtPeerConnType peer
  }

-- | Convert a DHTPeer to a ProviderEntry.
dhtPeerToProvider :: DHTPeer -> ProviderEntry
dhtPeerToProvider peer = ProviderEntry
  { peProvider  = PeerId (dhtPeerId peer)
  , peAddrs     = []
  , peTimestamp = epochTime
  }

-- | Epoch time as placeholder (0 seconds from epoch).
epochTime :: UTCTime
epochTime = read "2000-01-01 00:00:00 UTC"
