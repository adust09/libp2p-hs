module Test.Network.LibP2P.Protocol.GossipSub.MessageCacheSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import Network.LibP2P.Protocol.GossipSub.Types (Topic, MessageId, PubSubMessage (..), MessageCache (..))
import Network.LibP2P.Protocol.GossipSub.MessageCache

-- | Helper to create a simple PubSubMessage.
mkMsg :: Int -> Topic -> PubSubMessage
mkMsg n topic = PubSubMessage
  { msgFrom      = Just (BS.pack [fromIntegral n])
  , msgData      = BS.pack [fromIntegral n]
  , msgSeqNo     = Just (BS.pack [0, 0, 0, 0, 0, 0, 0, fromIntegral n])
  , msgTopic     = topic
  , msgSignature = Nothing
  , msgKey       = Nothing
  }

-- | Helper to create a message ID from an int.
mkMid :: Int -> MessageId
mkMid n = BS.pack [fromIntegral n]

spec :: Spec
spec = do
  describe "GossipSub.MessageCache" $ do

    describe "newMessageCache" $ do
      it "creates cache with correct number of empty windows" $ do
        let mc = newMessageCache 5 3
        mcLen mc `shouldBe` 5
        mcGossip mc `shouldBe` 3

    describe "cachePut" $ do
      it "adds entry to current window" $ do
        let mc = newMessageCache 5 3
            mc' = cachePut (mkMid 1) (mkMsg 1 "t") mc
        cacheGet (mkMid 1) mc' `shouldBe` Just (mkMsg 1 "t")

      it "supports multiple entries" $ do
        let mc = newMessageCache 5 3
            mc' = cachePut (mkMid 1) (mkMsg 1 "t") $
                  cachePut (mkMid 2) (mkMsg 2 "t") mc
        cacheGet (mkMid 1) mc' `shouldBe` Just (mkMsg 1 "t")
        cacheGet (mkMid 2) mc' `shouldBe` Just (mkMsg 2 "t")

    describe "cacheGet" $ do
      it "returns Nothing for unknown message ID" $ do
        let mc = newMessageCache 5 3
        cacheGet (mkMid 99) mc `shouldBe` Nothing

      it "retrieves entry after shift" $ do
        let mc = newMessageCache 5 3
            mc' = cacheShift $ cachePut (mkMid 1) (mkMsg 1 "t") mc
        -- Message should still be accessible after one shift
        cacheGet (mkMid 1) mc' `shouldBe` Just (mkMsg 1 "t")

    describe "cacheGetGossipIds" $ do
      it "returns IDs from gossip windows only" $ do
        let mc = newMessageCache 5 3
            mc' = cachePut (mkMid 1) (mkMsg 1 "blocks") mc
        -- mcGossip=3, message in window 0 (newest), within gossip range
        cacheGetGossipIds "blocks" mc' `shouldBe` [mkMid 1]

      it "filters by topic" $ do
        let mc = newMessageCache 5 3
            mc' = cachePut (mkMid 1) (mkMsg 1 "blocks") $
                  cachePut (mkMid 2) (mkMsg 2 "tx") mc
        cacheGetGossipIds "blocks" mc' `shouldBe` [mkMid 1]
        cacheGetGossipIds "tx" mc' `shouldBe` [mkMid 2]

      it "excludes messages outside gossip window" $ do
        -- mcGossip=2, shift 3 times → message should be outside gossip range
        let mc = newMessageCache 5 2
            mc' = cacheShift $ cacheShift $ cacheShift $
                  cachePut (mkMid 1) (mkMsg 1 "t") mc
        -- Message is in window 3, but gossip only covers windows 0-1
        cacheGetGossipIds "t" mc' `shouldBe` []
        -- But cacheGet should still find it (within mcLen=5)
        cacheGet (mkMid 1) mc' `shouldBe` Just (mkMsg 1 "t")

    describe "cacheShift" $ do
      it "rotates windows and drops oldest when full" $ do
        -- mcLen=3: after 3 shifts, oldest window is dropped
        let mc = newMessageCache 3 2
            mc' = cachePut (mkMid 1) (mkMsg 1 "t") mc
            mc'' = cacheShift $ cacheShift $ cacheShift mc'
        -- Message was in the first window, 3 shifts = evicted
        cacheGet (mkMid 1) mc'' `shouldBe` Nothing

      it "preserves entries within window range" $ do
        -- mcLen=5: after 2 shifts, message still accessible
        let mc = newMessageCache 5 3
            mc' = cacheShift $ cacheShift $
                  cachePut (mkMid 1) (mkMsg 1 "t") mc
        cacheGet (mkMid 1) mc' `shouldBe` Just (mkMsg 1 "t")

    describe "lifecycle" $ do
      it "put + shift + gossipIds + get" $ do
        let mc = newMessageCache 3 2
            -- Put messages in different windows
            mc1 = cachePut (mkMid 1) (mkMsg 1 "t") mc
            mc2 = cacheShift mc1
            mc3 = cachePut (mkMid 2) (mkMsg 2 "t") mc2
            mc4 = cacheShift mc3
            mc5 = cachePut (mkMid 3) (mkMsg 3 "t") mc4
        -- All three should be accessible
        cacheGet (mkMid 1) mc5 `shouldBe` Just (mkMsg 1 "t")
        cacheGet (mkMid 2) mc5 `shouldBe` Just (mkMsg 2 "t")
        cacheGet (mkMid 3) mc5 `shouldBe` Just (mkMsg 3 "t")
        -- Gossip (2 windows) should only include mid2 and mid3
        let gossipIds = cacheGetGossipIds "t" mc5
        mkMid 3 `elem` gossipIds `shouldBe` True
        mkMid 2 `elem` gossipIds `shouldBe` True
        mkMid 1 `elem` gossipIds `shouldBe` False
        -- One more shift should evict mid1
        let mc6 = cacheShift mc5
        cacheGet (mkMid 1) mc6 `shouldBe` Nothing
        cacheGet (mkMid 2) mc6 `shouldBe` Just (mkMsg 2 "t")
