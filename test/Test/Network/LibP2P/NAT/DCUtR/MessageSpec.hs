module Test.Network.LibP2P.NAT.DCUtR.MessageSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import Control.Concurrent.STM (newTQueueIO, atomically, writeTQueue, readTQueue, TQueue)
import Data.Word (Word8)
import Network.LibP2P.NAT.DCUtR.Message
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))

-- | Create an in-memory stream pair for testing.
mkStreamPair :: IO (StreamIO, StreamIO)
mkStreamPair = do
  q1 <- newTQueueIO :: IO (TQueue Word8)
  q2 <- newTQueueIO :: IO (TQueue Word8)
  let streamA = StreamIO
        { streamWrite = \bs -> mapM_ (\b -> atomically (writeTQueue q1 b)) (BS.unpack bs)
        , streamReadByte = atomically (readTQueue q2)
        , streamClose = pure ()
        }
      streamB = StreamIO
        { streamWrite = \bs -> mapM_ (\b -> atomically (writeTQueue q2 b)) (BS.unpack bs)
        , streamReadByte = atomically (readTQueue q1)
        , streamClose = pure ()
        }
  pure (streamA, streamB)

-- Test data

connectMsg :: HolePunchMessage
connectMsg = HolePunchMessage
  { hpType    = HPConnect
  , hpObsAddrs = [ BS.pack [4, 203, 0, 113, 5, 6, 0x10, 0x01]   -- /ip4/203.0.113.5/tcp/4097
                  , BS.pack [4, 10, 0, 0, 1, 6, 0x0F, 0xA1]      -- /ip4/10.0.0.1/tcp/4001
                  ]
  }

syncMsg :: HolePunchMessage
syncMsg = HolePunchMessage
  { hpType    = HPSync
  , hpObsAddrs = []
  }

connectNoAddrs :: HolePunchMessage
connectNoAddrs = HolePunchMessage
  { hpType    = HPConnect
  , hpObsAddrs = []
  }

connectManyAddrs :: HolePunchMessage
connectManyAddrs = HolePunchMessage
  { hpType    = HPConnect
  , hpObsAddrs = [ BS.pack [4, 127, 0, 0, 1, 6, 0x10, 0x01]
                  , BS.pack [4, 10, 0, 0, 1, 6, 0x0F, 0xA1]
                  , BS.pack [4, 192, 168, 1, 1, 6, 0x1F, 0x90]
                  , BS.pack [4, 172, 16, 0, 1, 6, 0x00, 0x50]
                  ]
  }

spec :: Spec
spec = do
  describe "DCUtR Message encoding/decoding" $ do
    it "encode → decode round-trip: CONNECT with addresses" $ do
      let encoded = encodeHolePunchMessage connectMsg
          decoded = decodeHolePunchMessage encoded
      decoded `shouldBe` Right connectMsg

    it "encode → decode round-trip: SYNC (no addresses)" $ do
      let encoded = encodeHolePunchMessage syncMsg
          decoded = decodeHolePunchMessage encoded
      decoded `shouldBe` Right syncMsg

    it "encode → decode round-trip: CONNECT with no addresses" $ do
      let encoded = encodeHolePunchMessage connectNoAddrs
          decoded = decodeHolePunchMessage encoded
      decoded `shouldBe` Right connectNoAddrs

    it "encode → decode round-trip: CONNECT with many addresses" $ do
      let encoded = encodeHolePunchMessage connectManyAddrs
          decoded = decodeHolePunchMessage encoded
      decoded `shouldBe` Right connectManyAddrs

  describe "DCUtR Message type wire values" $ do
    it "CONNECT maps to wire value 100" $ do
      holePunchTypeToWord HPConnect `shouldBe` 100

    it "SYNC maps to wire value 300" $ do
      holePunchTypeToWord HPSync `shouldBe` 300

    it "wire value 100 maps to CONNECT" $ do
      wordToHolePunchType 100 `shouldBe` Just HPConnect

    it "wire value 300 maps to SYNC" $ do
      wordToHolePunchType 300 `shouldBe` Just HPSync

    it "unknown wire value returns Nothing" $ do
      wordToHolePunchType 0 `shouldBe` Nothing
      wordToHolePunchType 200 `shouldBe` Nothing
      wordToHolePunchType 999 `shouldBe` Nothing

  describe "DCUtR Message wire framing" $ do
    it "encodeHolePunchFramed → decodeHolePunchFramed round-trip" $ do
      let framed = encodeHolePunchFramed connectMsg
      case decodeHolePunchFramed maxDCUtRMessageSize framed of
        Right decoded -> decoded `shouldBe` connectMsg
        Left err -> expectationFailure $ "Round-trip failed: " ++ err

    it "rejects message exceeding 4 KiB limit" $ do
      let fakeLen = BS.pack [0xFF, 0x3F]  -- varint for ~8191, above 4096
          fakePayload = BS.replicate 10 0x00
          oversized = fakeLen <> fakePayload
      case decodeHolePunchFramed maxDCUtRMessageSize oversized of
        Left _ -> pure ()  -- expected rejection
        Right _ -> expectationFailure "Should have rejected oversized message"

    it "writeHolePunchMessage + readHolePunchMessage over StreamIO pair" $ do
      (streamA, streamB) <- mkStreamPair
      writeHolePunchMessage streamA connectMsg
      result <- readHolePunchMessage streamB maxDCUtRMessageSize
      result `shouldBe` Right connectMsg

    it "multiple messages over same stream" $ do
      (streamA, streamB) <- mkStreamPair
      writeHolePunchMessage streamA connectMsg
      writeHolePunchMessage streamA syncMsg
      r1 <- readHolePunchMessage streamB maxDCUtRMessageSize
      r2 <- readHolePunchMessage streamB maxDCUtRMessageSize
      r1 `shouldBe` Right connectMsg
      r2 `shouldBe` Right syncMsg
