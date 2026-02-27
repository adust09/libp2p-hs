module Test.Network.LibP2P.DHT.RoutingTableSpec (spec) where

import Test.Hspec

import Data.Bits (xor, testBit)
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Data.Word (Word8)
import Network.LibP2P.Crypto.PeerId (PeerId (..))
import Network.LibP2P.DHT.Distance (peerIdToKey)
import Network.LibP2P.DHT.RoutingTable
  ( newRoutingTable
  , insertPeer
  , removePeer
  , closestPeers
  , bucketForPeer
  , bucketSize
  , allPeers
  )
import Network.LibP2P.DHT.Types
  ( BucketEntry (..)
  , ConnectionType (..)
  , DHTKey (..)
  , InsertResult (..)
  , kValue
  )

-- | Helper: create a PeerId from raw bytes.
mkPeerId :: BS.ByteString -> PeerId
mkPeerId = PeerId

-- | Helper: create a BucketEntry.
mkEntry :: PeerId -> UTCTime -> BucketEntry
mkEntry pid t = BucketEntry
  { entryPeerId   = pid
  , entryKey      = peerIdToKey pid
  , entryAddrs    = []
  , entryLastSeen = t
  , entryConnType = NotConnected
  }

-- | Generate a list of unique PeerIds for filling buckets.
genPeerIds :: Int -> [PeerId]
genPeerIds n = [mkPeerId (BS.pack [fromIntegral i]) | i <- [1..n]]

-- | The local peer used for the routing table.
localPeerId :: PeerId
localPeerId = mkPeerId (BS.pack [0])

spec :: Spec
spec = do
  describe "newRoutingTable" $ do
    it "creates empty table with correct self key" $ do
      let rt = newRoutingTable localPeerId
      allPeers rt `shouldBe` []

  describe "insertPeer" $ do
    it "into empty bucket → Inserted" $ do
      now <- getCurrentTime
      let rt = newRoutingTable localPeerId
          peer = mkEntry (mkPeerId (BS.pack [1])) now
          (rt', result) = insertPeer peer rt
      result `shouldBe` Inserted
      length (allPeers rt') `shouldBe` 1

    it "existing peer → Updated (moved to tail)" $ do
      now <- getCurrentTime
      let later = addUTCTime 10 now
          rt = newRoutingTable localPeerId
          peer1 = mkEntry (mkPeerId (BS.pack [1])) now
          peer2 = mkEntry (mkPeerId (BS.pack [2])) now
          (rt1, _) = insertPeer peer1 rt
          (rt2, _) = insertPeer peer2 rt1
          peer1Updated = mkEntry (mkPeerId (BS.pack [1])) later
          (rt3, result) = insertPeer peer1Updated rt2
      result `shouldBe` Updated
      let peers = allPeers rt3
      length peers `shouldBe` 2
      entryPeerId (last peers) `shouldBe` mkPeerId (BS.pack [1])

    it "self → rejected (not inserted)" $ do
      now <- getCurrentTime
      let rt = newRoutingTable localPeerId
          selfEntry = mkEntry localPeerId now
          (rt', result) = insertPeer selfEntry rt
      result `shouldBe` Updated
      allPeers rt' `shouldBe` []

    it "fills bucket to k → all Inserted" $ do
      now <- getCurrentTime
      let rt0 = newRoutingTable localPeerId
          allPids = genPeerIds 200
          entries = map (\pid -> mkEntry pid now) allPids
          selfKey = peerIdToKey localPeerId
          bucketed = groupByBucket selfKey entries
      case filter (\(_, es) -> length es >= kValue) bucketed of
        [] -> pendingWith "Not enough peers in any single bucket (probabilistic test)"
        ((bucketIdx, bucketEntries'):_) -> do
          let toInsert = take kValue bucketEntries'
              (rtFinal, results) = foldl
                (\(rt, rs) e -> let (rt', r) = insertPeer e rt in (rt', rs ++ [r]))
                (rt0, [])
                toInsert
          all (== Inserted) results `shouldBe` True
          bucketSize bucketIdx rtFinal `shouldBe` kValue

    it "into full bucket → BucketFull with LRS peer ID" $ do
      now <- getCurrentTime
      let rt0 = newRoutingTable localPeerId
          selfKey = peerIdToKey localPeerId
          allPids = genPeerIds 200
          entries = map (\pid -> mkEntry pid now) allPids
          bucketed = groupByBucket selfKey entries
      case filter (\(_, es) -> length es > kValue) bucketed of
        [] -> pendingWith "Not enough peers in any single bucket (probabilistic test)"
        ((_, bucketEntries'):_) -> do
          let toInsert = take (kValue + 1) bucketEntries'
              firstK = take kValue toInsert
              extra = toInsert !! kValue
              (rtFull, _) = foldl
                (\(rt, rs) e -> let (rt', r) = insertPeer e rt in (rt', rs ++ [r]))
                (rt0, [])
                firstK
              (_, result) = insertPeer extra rtFull
          case result of
            BucketFull lrsPid ->
              case firstK of
                (firstEntry:_) -> lrsPid `shouldBe` entryPeerId firstEntry
                [] -> expectationFailure "firstK should not be empty"
            other -> expectationFailure $ "Expected BucketFull, got " ++ show other

  describe "removePeer" $ do
    it "existing → peer gone from table" $ do
      now <- getCurrentTime
      let rt = newRoutingTable localPeerId
          pid = mkPeerId (BS.pack [1])
          entry = mkEntry pid now
          (rt1, _) = insertPeer entry rt
          rt2 = removePeer pid rt1
      allPeers rt2 `shouldBe` []

    it "non-existent → no-op" $ do
      now <- getCurrentTime
      let rt = newRoutingTable localPeerId
          pid = mkPeerId (BS.pack [1])
          entry = mkEntry pid now
          (rt1, _) = insertPeer entry rt
          rt2 = removePeer (mkPeerId (BS.pack [99])) rt1
      length (allPeers rt2) `shouldBe` 1

  describe "closestPeers" $ do
    it "returns up to n nearest peers sorted by distance" $ do
      now <- getCurrentTime
      let rt0 = newRoutingTable localPeerId
          pids = genPeerIds 10
          entries = map (\pid -> mkEntry pid now) pids
          (rtFinal, _) = foldl
            (\(rt, rs) e -> let (rt', r) = insertPeer e rt in (rt', rs ++ [r]))
            (rt0, [])
            entries
          target = peerIdToKey (mkPeerId (BS.pack [42]))
          closest = closestPeers target 5 rtFinal
      length closest `shouldBe` 5
      let dists = map (\e -> xorDist target (entryKey e)) closest
      dists `shouldBe` sort dists

    it "from empty table → empty list" $ do
      let rt = newRoutingTable localPeerId
          target = peerIdToKey (mkPeerId (BS.pack [1]))
      closestPeers target 10 rt `shouldBe` []

    it "spans multiple buckets when needed" $ do
      now <- getCurrentTime
      let rt0 = newRoutingTable localPeerId
          pids = genPeerIds 50
          entries = map (\pid -> mkEntry pid now) pids
          (rtFinal, _) = foldl
            (\(rt, rs) e -> let (rt', r) = insertPeer e rt in (rt', rs ++ [r]))
            (rt0, [])
            entries
          target = peerIdToKey (mkPeerId (BS.pack [200]))
          closest = closestPeers target 20 rtFinal
      length closest `shouldSatisfy` (> 0)
      length closest `shouldSatisfy` (<= 20)
      let dists = map (\e -> xorDist target (entryKey e)) closest
      dists `shouldBe` sort dists

  describe "bucketForPeer" $ do
    it "computes correct bucket index" $ do
      let rt = newRoutingTable localPeerId
          peerKey = peerIdToKey (mkPeerId (BS.pack [1]))
          idx = bucketForPeer peerKey rt
      idx `shouldSatisfy` (>= 0)
      idx `shouldSatisfy` (< 256)

  describe "bucket placement" $ do
    it "peers go to correct buckets based on prefix length" $ do
      now <- getCurrentTime
      let rt0 = newRoutingTable localPeerId
          pids = genPeerIds 30
          entries = map (\pid -> mkEntry pid now) pids
          (rtFinal, _) = foldl
            (\(rt, rs) e -> let (rt', r) = insertPeer e rt in (rt', rs ++ [r]))
            (rt0, [])
            entries
      mapM_ (\e -> do
        let expectedIdx = bucketForPeer (entryKey e) rtFinal
            sz = bucketSize expectedIdx rtFinal
        sz `shouldSatisfy` (> 0)
        ) entries

  describe "allPeers" $ do
    it "returns all entries across all buckets" $ do
      now <- getCurrentTime
      let rt0 = newRoutingTable localPeerId
          pids = genPeerIds 15
          entries = map (\pid -> mkEntry pid now) pids
          (rtFinal, _) = foldl
            (\(rt, rs) e -> let (rt', r) = insertPeer e rt in (rt', rs ++ [r]))
            (rt0, [])
            entries
      length (allPeers rtFinal) `shouldBe` 15

-- Helpers

-- | XOR distance as raw ByteString for sorting comparison.
xorDist :: DHTKey -> DHTKey -> BS.ByteString
xorDist (DHTKey a) (DHTKey b) = BS.pack (BS.zipWith xor a b)

-- | Group entries by their bucket index relative to a self key.
groupByBucket :: DHTKey -> [BucketEntry] -> [(Int, [BucketEntry])]
groupByBucket selfKey entries =
  let indexed = map (\e -> (computeBucketIdx selfKey (entryKey e), e)) entries
  in foldl (\acc (idx, e) ->
       case lookup idx acc of
         Nothing -> acc ++ [(idx, [e])]
         Just _  -> map (\(i, es) -> if i == idx then (i, es ++ [e]) else (i, es)) acc
     ) [] indexed

-- | Compute bucket index (common prefix length clamped to [0,255]).
computeBucketIdx :: DHTKey -> DHTKey -> Int
computeBucketIdx (DHTKey a) (DHTKey b) =
  let d = BS.pack (BS.zipWith xor a b)
  in min (countLeadingZeroBits d) 255

countLeadingZeroBits :: BS.ByteString -> Int
countLeadingZeroBits bs = go 0
  where
    len = BS.length bs
    go i
      | i >= len  = i * 8
      | byte == 0 = go (i + 1)
      | otherwise = i * 8 + clzByte byte
      where byte = BS.index bs i

clzByte :: Word8 -> Int
clzByte 0 = 8
clzByte w = go' 7
  where
    go' (-1) = 8
    go' bit
      | testBit w bit = 7 - bit
      | otherwise     = go' (bit - 1)
