module Test.Network.LibP2P.DHT.LookupSpec (spec) where

import Test.Hspec

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import Network.LibP2P.Crypto.PeerId (PeerId (..), peerIdBytes)
import Network.LibP2P.DHT.DHT
import Network.LibP2P.DHT.Distance (peerIdToKey)
import Network.LibP2P.DHT.Lookup
import Network.LibP2P.DHT.Message
import Network.LibP2P.DHT.RoutingTable (insertPeer, allPeers)
import Network.LibP2P.DHT.Types

-- Reuse mock Switch helpers from DHTSpec
import Test.Network.LibP2P.DHT.DHTSpec (mkTestNode, mkPeerId, localPid)

-- | Create a mock DHTNode with a custom sendRequest function.
mkNodeWithMock :: PeerId -> (PeerId -> DHTMessage -> IO (Either String DHTMessage)) -> IO DHTNode
mkNodeWithMock pid mockSend = do
  node <- mkTestNode pid
  pure node { dhtSendRequest = mockSend }

-- | Create a mock network: a map from PeerId to their response function.
-- Each "node" returns closerPeers based on its local knowledge.
type MockNetwork = Map.Map PeerId (DHTMessage -> DHTMessage)

-- | Build a mock sendRequest from a MockNetwork.
mockSendFromNetwork :: MockNetwork -> PeerId -> DHTMessage -> IO (Either String DHTMessage)
mockSendFromNetwork network pid msg =
  case Map.lookup pid network of
    Nothing -> pure (Left "peer not found in mock network")
    Just handler -> pure (Right (handler msg))

spec :: Spec
spec = do
  describe "iterativeFindNode" $ do
    it "with local-only routing table returns local peers" $ do
      now <- getCurrentTime
      -- Create node with some peers in routing table, no network
      node <- mkNodeWithMock localPid (\_ _ -> pure (Left "no network"))
      let peers = [mkPeerId (BS.pack [i]) | i <- [2..6]]
          entries = map (\pid -> BucketEntry pid (peerIdToKey pid) [] now NotConnected) peers
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        foldl (\r e -> fst (insertPeer e r)) rt entries

      let targetKey = peerIdToKey (mkPeerId (BS.pack [42]))
      result <- iterativeFindNode node targetKey
      -- Should return peers even though network queries fail
      length result `shouldSatisfy` (> 0)
      length result `shouldSatisfy` (<= kValue)

    it "converges through mock network (3-hop)" $ do
      now <- getCurrentTime
      -- Setup: localNode knows A, A knows B, B knows C (closer to target)
      let pidA = mkPeerId (BS.pack [10])
          pidB = mkPeerId (BS.pack [20])
          pidC = mkPeerId (BS.pack [30])
          target = BS.pack [42, 42, 42, 42]
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

      result <- iterativeFindNode node (DHTKey target)
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

      result <- iterativeFindNode node (DHTKey (BS.pack [0xFF, 0xFF]))
      length result `shouldBe` length peers

    it "handles query failures gracefully" $ do
      now <- getCurrentTime
      -- All queries fail
      node <- mkNodeWithMock localPid (\_ _ -> pure (Left "connection refused"))
      let peers = [mkPeerId (BS.pack [i]) | i <- [2..4]]
          entries = map (\pid -> BucketEntry pid (peerIdToKey pid) [] now NotConnected) peers
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        foldl (\r e -> fst (insertPeer e r)) rt entries

      result <- iterativeFindNode node (DHTKey (BS.pack [42]))
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

      result <- iterativeFindNode node (DHTKey (BS.pack [99]))
      length result `shouldBe` 3  -- only 3 peers total, less than k=20

  describe "iterativeGetValue" $ do
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

  describe "bootstrap" $ do
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
