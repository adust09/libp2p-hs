module Test.Network.LibP2P.DHT.DistanceSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import Network.LibP2P.Crypto.PeerId (PeerId (..))
import Network.LibP2P.DHT.Distance
  ( peerIdToKey
  , xorDistance
  , commonPrefixLength
  , compareDistance
  , sortByDistance
  )
import Network.LibP2P.DHT.Types (BucketEntry (..), ConnectionType (..), DHTKey (..))
import Data.Time (UTCTime, getCurrentTime)

-- | Helper: create a PeerId from raw bytes (for test determinism).
mkPeerId :: BS.ByteString -> PeerId
mkPeerId = PeerId

-- | Helper: create a BucketEntry for testing.
mkEntry :: PeerId -> DHTKey -> UTCTime -> BucketEntry
mkEntry pid key t = BucketEntry
  { entryPeerId   = pid
  , entryKey      = key
  , entryAddrs    = []
  , entryLastSeen = t
  , entryConnType = NotConnected
  }

-- | Zero key (all zero bytes).
zeroKey :: DHTKey
zeroKey = DHTKey (BS.replicate 32 0)


spec :: Spec
spec = do
  describe "peerIdToKey" $ do
    it "produces 32-byte output" $ do
      let pid = mkPeerId (BS.pack [1, 2, 3, 4])
          (DHTKey keyBytes) = peerIdToKey pid
      BS.length keyBytes `shouldBe` 32

    it "is deterministic (same input → same output)" $ do
      let pid = mkPeerId (BS.pack [10, 20, 30])
          key1 = peerIdToKey pid
          key2 = peerIdToKey pid
      key1 `shouldBe` key2

  describe "xorDistance" $ do
    it "identity: d(x, x) = 0" $ do
      let key = peerIdToKey (mkPeerId (BS.pack [42]))
          dist = xorDistance key key
      dist `shouldBe` zeroKey

    it "symmetry: d(x, y) = d(y, x)" $ do
      let keyA = peerIdToKey (mkPeerId (BS.pack [1]))
          keyB = peerIdToKey (mkPeerId (BS.pack [2]))
      xorDistance keyA keyB `shouldBe` xorDistance keyB keyA

    it "produces 32-byte output" $ do
      let keyA = peerIdToKey (mkPeerId (BS.pack [1]))
          keyB = peerIdToKey (mkPeerId (BS.pack [2]))
          (DHTKey distBytes) = xorDistance keyA keyB
      BS.length distBytes `shouldBe` 32

  describe "commonPrefixLength" $ do
    it "same key → 256" $ do
      let key = peerIdToKey (mkPeerId (BS.pack [99]))
      commonPrefixLength key key `shouldBe` 256

    it "different keys → correct bit position" $ do
      -- Construct keys that differ at a known bit position
      -- key1 = 0x00 0x00 ... (32 bytes of 0)
      -- key2 = 0x00 0x01 ... (byte 1 has bit 0 set → differ at bit 15)
      let key1 = DHTKey (BS.replicate 32 0)
          key2 = DHTKey (BS.pack (0 : 1 : replicate 30 0))
      -- XOR = 0x00 0x01 0x00... → 8 zero bits + 7 zero bits in byte 1 = 15
      commonPrefixLength key1 key2 `shouldBe` 15

    it "max-distance keys (first bit differs) → 0" $ do
      -- key1 starts with 0x00, key2 starts with 0x80 → first bit differs
      let key1 = DHTKey (BS.replicate 32 0)
          key2 = DHTKey (BS.pack (0x80 : replicate 31 0))
      commonPrefixLength key1 key2 `shouldBe` 0

  describe "compareDistance" $ do
    it "orders correctly (closer < farther)" $ do
      -- Use constructed keys where we control distance to zeroKey
      let closer = DHTKey (BS.pack (0 : 1 : replicate 30 0))  -- small XOR to zero
          farther = DHTKey (BS.pack (0xFF : replicate 31 0))   -- large XOR to zero
          target = zeroKey
      -- d(0, closer) = 0x0001... < d(0, farther) = 0xFF00...
      compareDistance target closer farther `shouldBe` LT
      compareDistance target farther closer `shouldBe` GT
      compareDistance target closer closer  `shouldBe` EQ

  describe "sortByDistance" $ do
    it "sorts ascending by XOR distance" $ do
      now <- getCurrentTime
      -- Use constructed keys with known distances to zeroKey
      let target = zeroKey
          -- Create entries with keys at known distances
          nearKey  = DHTKey (BS.pack (0 : 0 : 1 : replicate 29 0))   -- distance 0x000001...
          midKey   = DHTKey (BS.pack (0 : 1 : replicate 30 0))       -- distance 0x0001...
          farKey   = DHTKey (BS.pack (1 : replicate 31 0))            -- distance 0x01...
          nearPid  = mkPeerId (BS.pack [10])
          midPid   = mkPeerId (BS.pack [20])
          farPid   = mkPeerId (BS.pack [30])
          entries = [ mkEntry farPid farKey now
                    , mkEntry nearPid nearKey now
                    , mkEntry midPid midKey now
                    ]
          sorted = sortByDistance target entries
      map (entryPeerId) sorted `shouldBe` [nearPid, midPid, farPid]
