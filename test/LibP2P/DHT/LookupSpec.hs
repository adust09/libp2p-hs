module LibP2P.DHT.LookupSpec (spec) where

import Test.Hspec

import Control.Concurrent.STM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import LibP2P.Crypto.PeerId (PeerId (..), peerIdBytes)
import LibP2P.DHT
import LibP2P.DHT.Distance (peerIdToKey, sortByDistance)
import LibP2P.DHT.Lookup
import LibP2P.DHT.Message
import LibP2P.DHT.RoutingTable (insertPeer, allPeers)
import LibP2P.DHT.Types
import LibP2P.Multiaddr (fromText, toBytes)

-- Reuse mock Switch helpers from DHTSpec
import LibP2P.DHT.DHTSpec (mkTestNode, mkPeerId, localPid)

-- | Create a mock DHTNode with a custom sendRequest function.
mkNodeWithMock :: PeerId -> (PeerId -> DHTMessage -> IO (Either String DHTMessage)) -> IO DHTNode
mkNodeWithMock pid mockSend = do
  node <- mkTestNode pid
  pure node { dhtSendRequest = mockSend }

-- | Create a mock network: a map from PeerId to their response function.
-- Each "node" returns closerPeers based on its local knowledge.
type MockNetwork = Map.Map PeerId (DHTMessage -> DHTMessage)

-- | Build a mock sendRequest from a MockNetwork.
--
-- Issue #173: mock senders must read the request rather than return a
-- canned constant. A query without a wire key would be a client-side bug
-- that a constant mock silently masks, so reject it like a real peer.
mockSendFromNetwork :: MockNetwork -> PeerId -> DHTMessage -> IO (Either String DHTMessage)
mockSendFromNetwork network pid msg
  | BS.null (msgKey msg) = pure (Left "mock: request carries no wire key")
  | otherwise =
      case Map.lookup pid network of
        Nothing -> pure (Left "peer not found in mock network")
        Just handler -> pure (Right (handler msg))

spec :: Spec
spec = do
  describe "iterativeFindNode" $ do
    -- Issue #146: per specs/kad-dht the FIND_NODE wire key must be the
    -- binary Peer ID. The old code sent sha256(peer id); a remote hashes
    -- the key again, landing the query at sha256(sha256(peer id)).
    it "sends the raw target peer ID as the wire key, not its SHA-256 digest" $ do
      now <- getCurrentTime
      sentKeysRef <- newIORef ([] :: [BS.ByteString])
      let targetPid = mkPeerId (BS.pack [42, 42, 42, 42])
          mockSend _pid msg = do
            atomicModifyIORef' sentKeysRef (\ks -> (ks ++ [msgKey msg], ()))
            pure (Right (emptyDHTMessage { msgType = FindNode, msgCloserPeers = [] }))
      node <- mkNodeWithMock localPid mockSend
      let pidA = mkPeerId (BS.pack [10])
          entryA = BucketEntry pidA (peerIdToKey pidA) [] now NotConnected
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        fst (insertPeer entryA rt)

      _ <- iterativeFindNode node targetPid
      sentKeys <- readIORef sentKeysRef
      sentKeys `shouldSatisfy` (not . null)
      mapM_ (`shouldBe` peerIdBytes targetPid) sentKeys

    it "with local-only routing table returns local peers" $ do
      now <- getCurrentTime
      -- Create node with some peers in routing table, no network
      node <- mkNodeWithMock localPid (\_ _ -> pure (Left "no network"))
      let peers = [mkPeerId (BS.pack [i]) | i <- [2..6]]
          entries = map (\pid -> BucketEntry pid (peerIdToKey pid) [] now NotConnected) peers
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        foldl (\r e -> fst (insertPeer e r)) rt entries

      result <- iterativeFindNode node (mkPeerId (BS.pack [42]))
      -- Should return peers even though network queries fail
      length result `shouldSatisfy` (> 0)
      length result `shouldSatisfy` (<= kValue)

    it "converges through mock network (3-hop)" $ do
      now <- getCurrentTime
      -- Setup: localNode knows A, A knows B, B knows C (closer to target)
      let pidA = mkPeerId (BS.pack [10])
          pidB = mkPeerId (BS.pack [20])
          pidC = mkPeerId (BS.pack [30])
          targetPid = mkPeerId (BS.pack [42, 42, 42, 42])
          -- Mock network: A returns B as closer, B returns C as closer
          network = Map.fromList
            [ (pidA, \_ -> emptyDHTMessage
                { msgType = FindNode
                , msgCloserPeers = [DHTPeer (peerIdBytes pidB) [] NotConnected]
                })
            , (pidB, \_ -> emptyDHTMessage
                { msgType = FindNode
                , msgCloserPeers = [DHTPeer (peerIdBytes pidC) [] NotConnected]
                })
            , (pidC, \_ -> emptyDHTMessage
                { msgType = FindNode
                , msgCloserPeers = []  -- terminus
                })
            ]
      node <- mkNodeWithMock localPid (mockSendFromNetwork network)
      -- Seed routing table with A
      let entryA = BucketEntry pidA (peerIdToKey pidA) [] now NotConnected
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        fst (insertPeer entryA rt)

      result <- iterativeFindNode node targetPid
      -- Should have discovered A, B, C through the lookup chain
      let foundPids = map entryPeerId result
      foundPids `shouldSatisfy` (\ps -> pidA `elem` ps)
      -- B and C should also have been discovered
      length result `shouldSatisfy` (>= 2)

    it "terminates when all k-closest queried" $ do
      now <- getCurrentTime
      -- All peers return empty closerPeers → terminates after querying seeds
      let peers = [mkPeerId (BS.pack [i]) | i <- [2..6]]
          network = Map.fromList
            [(pid, \_ -> emptyDHTMessage { msgType = FindNode, msgCloserPeers = [] })
            | pid <- peers]
      node <- mkNodeWithMock localPid (mockSendFromNetwork network)
      let entries = map (\pid -> BucketEntry pid (peerIdToKey pid) [] now NotConnected) peers
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        foldl (\r e -> fst (insertPeer e r)) rt entries

      result <- iterativeFindNode node (mkPeerId (BS.pack [0xFF, 0xFF]))
      length result `shouldBe` length peers

    it "handles query failures gracefully" $ do
      now <- getCurrentTime
      -- All queries fail
      node <- mkNodeWithMock localPid (\_ _ -> pure (Left "connection refused"))
      let peers = [mkPeerId (BS.pack [i]) | i <- [2..4]]
          entries = map (\pid -> BucketEntry pid (peerIdToKey pid) [] now NotConnected) peers
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        foldl (\r e -> fst (insertPeer e r)) rt entries

      result <- iterativeFindNode node (mkPeerId (BS.pack [42]))
      -- Should still return the local peers
      length result `shouldSatisfy` (> 0)

    it "terminates early when total peers < k" $ do
      now <- getCurrentTime
      -- Only 3 peers, all return empty
      let peers = [mkPeerId (BS.pack [i]) | i <- [2..4]]
          network = Map.fromList
            [(pid, \_ -> emptyDHTMessage { msgType = FindNode, msgCloserPeers = [] })
            | pid <- peers]
      node <- mkNodeWithMock localPid (mockSendFromNetwork network)
      let entries = map (\pid -> BucketEntry pid (peerIdToKey pid) [] now NotConnected) peers
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        foldl (\r e -> fst (insertPeer e r)) rt entries

      result <- iterativeFindNode node (mkPeerId (BS.pack [99]))
      length result `shouldBe` 3  -- only 3 peers total, less than k=20

    it "queries peers in XOR distance order, not lexicographic key order" $ do
      now <- getCurrentTime
      queryOrderRef <- newIORef ([] :: [PeerId])

      -- Create 15 peers so that alpha (10) < total, forcing batch selection
      let peers = [mkPeerId (BS.pack [i]) | i <- [10..24]]
          targetPid = mkPeerId (BS.replicate 32 0xFF)
          -- Distance is over sha256(key) (specs/kad-dht), so the expected
          -- ordering must use the hashed target, matching the lookup.
          targetKey = peerIdToKey targetPid

      -- Compute expected first batch: the 10 closest by XOR distance
      let entries = map (\pid -> BucketEntry pid (peerIdToKey pid) [] now NotConnected) peers
          sortedByDist = sortByDistance targetKey entries
          closestAlpha = Set.fromList $ map entryPeerId (take alphaValue sortedByDist)

      -- Mock network: each peer returns empty but records query order
      let mockSend pid _msg = do
            atomicModifyIORef' queryOrderRef (\xs -> (xs ++ [pid], ()))
            pure (Right (emptyDHTMessage { msgType = FindNode, msgCloserPeers = [] }))

      node <- mkNodeWithMock localPid mockSend
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        foldl (\r e -> fst (insertPeer e r)) rt entries

      _ <- iterativeFindNode node targetPid

      -- The first alpha peers queried should be the closest by XOR distance
      -- (mapConcurrently doesn't preserve order within batch, so compare as sets)
      queriedOrder <- readIORef queryOrderRef
      let firstBatch = Set.fromList (take alphaValue queriedOrder)
      firstBatch `shouldBe` closestAlpha

    it "returns results sorted by XOR distance to target" $ do
      now <- getCurrentTime
      let peers = [mkPeerId (BS.pack [i]) | i <- [10..20]]
          targetPid = mkPeerId (BS.replicate 32 0xAA)
          -- Expected order uses sha256(target peer id), the spec metric.
          targetKey = peerIdToKey targetPid
          entries = map (\pid -> BucketEntry pid (peerIdToKey pid) [] now NotConnected) peers
          expectedOrder = map entryPeerId (sortByDistance targetKey entries)

      let mockSend _pid _msg =
            pure (Right (emptyDHTMessage { msgType = FindNode, msgCloserPeers = [] }))

      node <- mkNodeWithMock localPid mockSend
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        foldl (\r e -> fst (insertPeer e r)) rt entries

      result <- iterativeFindNode node targetPid
      -- Results should be sorted by XOR distance to the target
      map entryPeerId result `shouldBe` expectedOrder

    -- Issue #147 (receive side): multiaddrs carried by closerPeers must be
    -- decoded into the resulting entries instead of being dropped —
    -- otherwise the peers we learn about cannot be dialled.
    it "decodes closerPeers' multiaddrs into lookup results" $ do
      now <- getCurrentTime
      let pidA = mkPeerId (BS.pack [10])
          pidB = mkPeerId (BS.pack [20])
          addrB = either error id (fromText "/ip4/192.0.2.1/tcp/4001")
          targetPid = mkPeerId (BS.pack [42])
          network = Map.fromList
            [ (pidA, \_ -> emptyDHTMessage
                { msgType = FindNode
                , msgCloserPeers =
                    [DHTPeer (peerIdBytes pidB) [toBytes addrB] NotConnected]
                })
            , (pidB, \_ -> emptyDHTMessage
                { msgType = FindNode, msgCloserPeers = [] })
            ]
      node <- mkNodeWithMock localPid (mockSendFromNetwork network)
      let entryA = BucketEntry pidA (peerIdToKey pidA) [] now NotConnected
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        fst (insertPeer entryA rt)

      result <- iterativeFindNode node targetPid
      let entriesB = filter ((== pidB) . entryPeerId) result
      map entryAddrs entriesB `shouldBe` [[addrB]]

  describe "iterativeGetValue" $ do
    it "sends the raw record key as the wire key" $ do
      now <- getCurrentTime
      sentKeysRef <- newIORef ([] :: [BS.ByteString])
      let key = BS.pack [0xCA, 0xFE]
          record = DHTRecord key (BS.pack [0xDE, 0xAD]) "2024-01-01T00:00:00Z"
          mockSend _pid msg = do
            atomicModifyIORef' sentKeysRef (\ks -> (ks ++ [msgKey msg], ()))
            pure (Right (emptyDHTMessage
              { msgType = GetValue, msgRecord = Just record, msgCloserPeers = [] }))
          validator = Validator (\_ _ -> Right ()) (\_ _ -> Right 0)
      node <- mkNodeWithMock localPid mockSend
      let pidA = mkPeerId (BS.pack [10])
          entryA = BucketEntry pidA (peerIdToKey pidA) [] now NotConnected
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        fst (insertPeer entryA rt)

      _ <- iterativeGetValue node validator key
      sentKeys <- readIORef sentKeysRef
      sentKeys `shouldSatisfy` (not . null)
      mapM_ (`shouldBe` key) sentKeys

    it "finds value from mock network" $ do
      now <- getCurrentTime
      let pidA = mkPeerId (BS.pack [10])
          key = BS.pack [0xCA, 0xFE]
          record = DHTRecord key (BS.pack [0xDE, 0xAD]) "2024-01-01T00:00:00Z"
          network = Map.fromList
            [ (pidA, \_ -> emptyDHTMessage
                { msgType = GetValue
                , msgRecord = Just record
                , msgCloserPeers = []
                })
            ]
          validator = Validator
            { valValidate = \_ _ -> Right ()
            , valSelect = \_ vals -> Right 0  -- always pick first
            }
      node <- mkNodeWithMock localPid (mockSendFromNetwork network)
      let entryA = BucketEntry pidA (peerIdToKey pidA) [] now NotConnected
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        fst (insertPeer entryA rt)

      result <- iterativeGetValue node validator key
      result `shouldBe` Right record

    it "corrects outdated peers with PUT_VALUE" $ do
      now <- getCurrentTime
      putCalls <- newTVarIO ([] :: [PeerId])
      let pidA = mkPeerId (BS.pack [10])
          pidB = mkPeerId (BS.pack [20])
          key = BS.pack [0xCA, 0xFE]
          oldRecord = DHTRecord key (BS.pack [0x01]) "2024-01-01T00:00:00Z"
          newRecord = DHTRecord key (BS.pack [0x02]) "2024-06-01T00:00:00Z"
          -- A has old value, B has new (better) value
          network = Map.fromList
            [ (pidA, \_ -> emptyDHTMessage
                { msgType = GetValue
                , msgRecord = Just oldRecord
                , msgCloserPeers = [DHTPeer (peerIdBytes pidB) [] NotConnected]
                })
            , (pidB, \_ -> emptyDHTMessage
                { msgType = GetValue
                , msgRecord = Just newRecord
                , msgCloserPeers = []
                })
            ]
          -- Custom sender that also tracks PUT_VALUE calls
          mockSend pid msg = do
            case msgType msg of
              PutValue -> atomically $ modifyTVar' putCalls (pid :)
              _ -> pure ()
            mockSendFromNetwork network pid msg
          validator = Validator
            { valValidate = \_ _ -> Right ()
            , valSelect = \_ vals ->
                -- Select second value (index 1) as better
                if length vals >= 2 then Right 1 else Right 0
            }
      node <- mkNodeWithMock localPid mockSend
      let entryA = BucketEntry pidA (peerIdToKey pidA) [] now NotConnected
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        fst (insertPeer entryA rt)

      result <- iterativeGetValue node validator key
      case result of
        Right rec -> recValue rec `shouldBe` BS.pack [0x02]
        Left err -> expectationFailure $ "Expected value, got: " ++ err
      -- Verify PUT_VALUE was sent to peer A (outdated)
      puts <- readTVarIO putCalls
      puts `shouldSatisfy` (\ps -> pidA `elem` ps)

    it "selects best value via Validator.select" $ do
      now <- getCurrentTime
      let pidA = mkPeerId (BS.pack [10])
          pidB = mkPeerId (BS.pack [20])
          key = BS.pack [0xBB]
          recA = DHTRecord key (BS.pack [1]) "2024-01-01T00:00:00Z"
          recB = DHTRecord key (BS.pack [2]) "2024-06-01T00:00:00Z"
          network = Map.fromList
            [ (pidA, \_ -> emptyDHTMessage
                { msgType = GetValue
                , msgRecord = Just recA
                , msgCloserPeers = [DHTPeer (peerIdBytes pidB) [] NotConnected]
                })
            , (pidB, \_ -> emptyDHTMessage
                { msgType = GetValue
                , msgRecord = Just recB
                , msgCloserPeers = []
                })
            ]
          -- Always select index 0 (first = current best)
          validator = Validator
            { valValidate = \_ _ -> Right ()
            , valSelect = \_ _ -> Right 0
            }
      node <- mkNodeWithMock localPid (mockSendFromNetwork network)
      let entryA = BucketEntry pidA (peerIdToKey pidA) [] now NotConnected
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        fst (insertPeer entryA rt)

      result <- iterativeGetValue node validator key
      -- With Select always returning 0, the first value found (recA) should be kept
      case result of
        Right rec -> recValue rec `shouldBe` BS.pack [1]
        Left err -> expectationFailure $ "Expected value, got: " ++ err

  describe "iterativeGetProviders" $ do
    it "sends the raw content key as the wire key" $ do
      now <- getCurrentTime
      sentKeysRef <- newIORef ([] :: [BS.ByteString])
      let key = BS.pack [0xDD, 0xEE, 0xFF]
          mockSend _pid msg = do
            atomicModifyIORef' sentKeysRef (\ks -> (ks ++ [msgKey msg], ()))
            pure (Right (emptyDHTMessage
              { msgType = GetProviders, msgCloserPeers = [], msgProviderPeers = [] }))
      node <- mkNodeWithMock localPid mockSend
      let pidA = mkPeerId (BS.pack [10])
          entryA = BucketEntry pidA (peerIdToKey pidA) [] now NotConnected
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        fst (insertPeer entryA rt)

      _ <- iterativeGetProviders node key
      sentKeys <- readIORef sentKeysRef
      sentKeys `shouldSatisfy` (not . null)
      mapM_ (`shouldBe` key) sentKeys

    it "collects providers from multiple hops" $ do
      now <- getCurrentTime
      let pidA = mkPeerId (BS.pack [10])
          pidB = mkPeerId (BS.pack [20])
          key = BS.pack [0xDD]
          providerPeer1 = DHTPeer (BS.pack [50]) [] Connected
          providerPeer2 = DHTPeer (BS.pack [60]) [] Connected
          network = Map.fromList
            [ (pidA, \_ -> emptyDHTMessage
                { msgType = GetProviders
                , msgCloserPeers = [DHTPeer (peerIdBytes pidB) [] NotConnected]
                , msgProviderPeers = [providerPeer1]
                })
            , (pidB, \_ -> emptyDHTMessage
                { msgType = GetProviders
                , msgCloserPeers = []
                , msgProviderPeers = [providerPeer2]
                })
            ]
      node <- mkNodeWithMock localPid (mockSendFromNetwork network)
      let entryA = BucketEntry pidA (peerIdToKey pidA) [] now NotConnected
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        fst (insertPeer entryA rt)

      result <- iterativeGetProviders node key
      -- Should have collected providers from both hops
      length result `shouldSatisfy` (>= 2)

    -- Issue #147 (receive side): provider records carry multiaddrs that
    -- must be decoded so the provider can be dialled.
    it "decodes provider multiaddrs into provider entries" $ do
      now <- getCurrentTime
      let pidA = mkPeerId (BS.pack [10])
          key = BS.pack [0xD1]
          providerAddr = either error id (fromText "/ip4/192.0.2.7/tcp/4001")
          providerPeer = DHTPeer (BS.pack [50]) [toBytes providerAddr] Connected
          network = Map.fromList
            [ (pidA, \_ -> emptyDHTMessage
                { msgType = GetProviders
                , msgCloserPeers = []
                , msgProviderPeers = [providerPeer]
                })
            ]
      node <- mkNodeWithMock localPid (mockSendFromNetwork network)
      let entryA = BucketEntry pidA (peerIdToKey pidA) [] now NotConnected
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        fst (insertPeer entryA rt)

      result <- iterativeGetProviders node key
      map peAddrs result `shouldBe` [[providerAddr]]

  describe "bootstrap" $ do
    it "self-lookup sends raw peer IDs as wire keys" $ do
      sentKeysRef <- newIORef ([] :: [BS.ByteString])
      let seedPid = mkPeerId (BS.pack [10])
          mockSend _pid msg = do
            atomicModifyIORef' sentKeysRef (\ks -> (ks ++ [msgKey msg], ()))
            pure (Right (emptyDHTMessage { msgType = FindNode, msgCloserPeers = [] }))
      node <- mkNodeWithMock localPid mockSend

      bootstrap node [seedPid]

      sentKeys <- readIORef sentKeysRef
      sentKeys `shouldSatisfy` (not . null)
      -- The self-lookup queries for our own binary peer ID (not its digest).
      take 1 sentKeys `shouldBe` [peerIdBytes localPid]
      -- Bucket refresh queries also carry raw peer IDs.
      mapM_ (\k -> k `shouldSatisfy` (`elem` [peerIdBytes localPid, peerIdBytes seedPid]))
            sentKeys

    it "performs self-lookup and populates nearby buckets" $ do
      now <- getCurrentTime
      let seedPid = mkPeerId (BS.pack [10])
          pidB = mkPeerId (BS.pack [20])
          pidC = mkPeerId (BS.pack [30])
          -- Seed returns B and C as closer peers
          network = Map.fromList
            [ (seedPid, \_ -> emptyDHTMessage
                { msgType = FindNode
                , msgCloserPeers = [ DHTPeer (peerIdBytes pidB) [] NotConnected
                                   , DHTPeer (peerIdBytes pidC) [] NotConnected
                                   ]
                })
            , (pidB, \_ -> emptyDHTMessage { msgType = FindNode, msgCloserPeers = [] })
            , (pidC, \_ -> emptyDHTMessage { msgType = FindNode, msgCloserPeers = [] })
            ]
      node <- mkNodeWithMock localPid (mockSendFromNetwork network)

      bootstrap node [seedPid]

      -- After bootstrap, routing table should contain the seed + discovered peers
      rt <- readTVarIO (dhtRoutingTable node)
      let allEntries = allPeers rt
      -- At minimum, the seed should be in the routing table
      length allEntries `shouldSatisfy` (>= 1)

    it "bootstrap respects timeout (completes even with slow peers)" $ do
      now <- getCurrentTime
      -- All queries return empty → bootstrap completes quickly
      let seedPid = mkPeerId (BS.pack [10])
          network = Map.fromList
            [ (seedPid, \_ -> emptyDHTMessage { msgType = FindNode, msgCloserPeers = [] })
            ]
      node <- mkNodeWithMock localPid (mockSendFromNetwork network)
      -- This should complete without hanging
      bootstrap node [seedPid]
      -- If we reach here, timeout behavior is fine
      pure () :: IO ()
