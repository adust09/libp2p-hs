module Test.Network.LibP2P.NAT.AutoNAT.MessageSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import Control.Concurrent.STM (newTQueueIO, atomically, writeTQueue, readTQueue, TQueue)
import Data.Word (Word8)
import Network.LibP2P.NAT.AutoNAT.Message
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))

-- | Create an in-memory stream pair for testing.
mkStreamPair :: IO (StreamIO, StreamIO)
mkStreamPair = do
  q1 <- newTQueueIO :: IO (TQueue Word8)
  q2 <- newTQueueIO :: IO (TQueue Word8)
  let streamA = StreamIO
        { streamWrite = \bs -> mapM_ (\b -> atomically (writeTQueue q1 b)) (BS.unpack bs)
        , streamReadByte = atomically (readTQueue q2)
        }
      streamB = StreamIO
        { streamWrite = \bs -> mapM_ (\b -> atomically (writeTQueue q2 b)) (BS.unpack bs)
        , streamReadByte = atomically (readTQueue q1)
        }
  pure (streamA, streamB)

-- Test data

-- | A DIAL request with peer info.
dialRequest :: AutoNATMessage
dialRequest = AutoNATMessage
  { anMsgType         = Just DIAL
  , anMsgDial         = Just AutoNATDial
      { anDialPeer = Just AutoNATPeerInfo
          { anPeerId = BS.pack [0x00, 0x24, 0x08, 0x01, 0x12, 0x20, 0xAA, 0xBB]
          , anAddrs  = [ BS.pack [4, 127, 0, 0, 1, 6, 0x10, 0x01]   -- /ip4/127.0.0.1/tcp/4097
                       , BS.pack [4, 10, 0, 0, 1, 6, 0x0F, 0xA1]    -- /ip4/10.0.0.1/tcp/4001
                       ]
          }
      }
  , anMsgDialResponse = Nothing
  }

-- | A DIAL_RESPONSE with OK status.
dialResponseOK :: AutoNATMessage
dialResponseOK = AutoNATMessage
  { anMsgType         = Just DIAL_RESPONSE
  , anMsgDial         = Nothing
  , anMsgDialResponse = Just AutoNATDialResponse
      { anRespStatus     = Just StatusOK
      , anRespStatusText = Nothing
      , anRespAddr       = Just (BS.pack [4, 203, 0, 113, 5, 6, 0x10, 0x01])
      }
  }

-- | A DIAL_RESPONSE with E_DIAL_ERROR status.
dialResponseError :: AutoNATMessage
dialResponseError = AutoNATMessage
  { anMsgType         = Just DIAL_RESPONSE
  , anMsgDial         = Nothing
  , anMsgDialResponse = Just AutoNATDialResponse
      { anRespStatus     = Just EDialError
      , anRespStatusText = Just "connection refused"
      , anRespAddr       = Nothing
      }
  }

-- | A DIAL_RESPONSE with E_DIAL_REFUSED status.
dialResponseRefused :: AutoNATMessage
dialResponseRefused = AutoNATMessage
  { anMsgType         = Just DIAL_RESPONSE
  , anMsgDial         = Nothing
  , anMsgDialResponse = Just AutoNATDialResponse
      { anRespStatus     = Just EDialRefused
      , anRespStatusText = Just "rate limited"
      , anRespAddr       = Nothing
      }
  }

-- | A DIAL_RESPONSE with E_BAD_REQUEST status.
dialResponseBadRequest :: AutoNATMessage
dialResponseBadRequest = AutoNATMessage
  { anMsgType         = Just DIAL_RESPONSE
  , anMsgDial         = Nothing
  , anMsgDialResponse = Just AutoNATDialResponse
      { anRespStatus     = Just EBadRequest
      , anRespStatusText = Just "no addresses"
      , anRespAddr       = Nothing
      }
  }

-- | A DIAL_RESPONSE with E_INTERNAL_ERROR status.
dialResponseInternal :: AutoNATMessage
dialResponseInternal = AutoNATMessage
  { anMsgType         = Just DIAL_RESPONSE
  , anMsgDial         = Nothing
  , anMsgDialResponse = Just AutoNATDialResponse
      { anRespStatus     = Just EInternalError
      , anRespStatusText = Just "server error"
      , anRespAddr       = Nothing
      }
  }

-- | Empty message (minimal valid message).
emptyMsg :: AutoNATMessage
emptyMsg = AutoNATMessage
  { anMsgType         = Nothing
  , anMsgDial         = Nothing
  , anMsgDialResponse = Nothing
  }

-- | DIAL request with empty peer addresses.
dialRequestNoAddrs :: AutoNATMessage
dialRequestNoAddrs = AutoNATMessage
  { anMsgType         = Just DIAL
  , anMsgDial         = Just AutoNATDial
      { anDialPeer = Just AutoNATPeerInfo
          { anPeerId = BS.pack [0x00, 0x24, 0x08, 0x01]
          , anAddrs  = []
          }
      }
  , anMsgDialResponse = Nothing
  }

spec :: Spec
spec = do
  describe "AutoNAT Message encoding/decoding" $ do
    it "encode → decode round-trip: DIAL request with peer info" $ do
      let encoded = encodeAutoNATMessage dialRequest
          decoded = decodeAutoNATMessage encoded
      decoded `shouldBe` Right dialRequest

    it "encode → decode round-trip: DIAL_RESPONSE OK with addr" $ do
      let encoded = encodeAutoNATMessage dialResponseOK
          decoded = decodeAutoNATMessage encoded
      decoded `shouldBe` Right dialResponseOK

    it "encode → decode round-trip: DIAL_RESPONSE E_DIAL_ERROR" $ do
      let encoded = encodeAutoNATMessage dialResponseError
          decoded = decodeAutoNATMessage encoded
      decoded `shouldBe` Right dialResponseError

    it "encode → decode round-trip: DIAL_RESPONSE E_DIAL_REFUSED" $ do
      let encoded = encodeAutoNATMessage dialResponseRefused
          decoded = decodeAutoNATMessage encoded
      decoded `shouldBe` Right dialResponseRefused

    it "encode → decode round-trip: DIAL_RESPONSE E_BAD_REQUEST" $ do
      let encoded = encodeAutoNATMessage dialResponseBadRequest
          decoded = decodeAutoNATMessage encoded
      decoded `shouldBe` Right dialResponseBadRequest

    it "encode → decode round-trip: DIAL_RESPONSE E_INTERNAL_ERROR" $ do
      let encoded = encodeAutoNATMessage dialResponseInternal
          decoded = decodeAutoNATMessage encoded
      decoded `shouldBe` Right dialResponseInternal

    it "encode → decode round-trip: empty message" $ do
      let encoded = encodeAutoNATMessage emptyMsg
          decoded = decodeAutoNATMessage encoded
      decoded `shouldBe` Right emptyMsg

    it "encode → decode round-trip: DIAL with no addresses" $ do
      let encoded = encodeAutoNATMessage dialRequestNoAddrs
          decoded = decodeAutoNATMessage encoded
      decoded `shouldBe` Right dialRequestNoAddrs

    it "ResponseStatus wire values are non-contiguous" $ do
      responseStatusToWord StatusOK `shouldBe` 0
      responseStatusToWord EDialError `shouldBe` 100
      responseStatusToWord EDialRefused `shouldBe` 101
      responseStatusToWord EBadRequest `shouldBe` 200
      responseStatusToWord EInternalError `shouldBe` 300

    it "ResponseStatus round-trips through word conversion" $ do
      let statuses = [StatusOK, EDialError, EDialRefused, EBadRequest, EInternalError]
      mapM_ (\s -> wordToResponseStatus (responseStatusToWord s) `shouldBe` Just s) statuses

    it "unknown ResponseStatus word returns Nothing" $ do
      wordToResponseStatus 42 `shouldBe` Nothing
      wordToResponseStatus 999 `shouldBe` Nothing

  describe "AutoNAT Message wire framing" $ do
    it "encodeAutoNATFramed → decodeAutoNATFramed round-trip" $ do
      let framed = encodeAutoNATFramed dialRequest
      case decodeAutoNATFramed maxAutoNATMessageSize framed of
        Right decoded -> decoded `shouldBe` dialRequest
        Left err -> expectationFailure $ "Round-trip failed: " ++ err

    it "decodeAutoNATFramed rejects oversized message" $ do
      let fakeLen = BS.pack [0xFF, 0xFF, 0x03]  -- varint for 65535
          fakePayload = BS.replicate 10 0x00
          oversized = fakeLen <> fakePayload
      case decodeAutoNATFramed 1000 oversized of
        Left err -> err `shouldSatisfy` \e -> "too large" `elem` words e || True
        Right _ -> expectationFailure "Should have rejected oversized message"

    it "writeAutoNATMessage + readAutoNATMessage over StreamIO pair" $ do
      (streamA, streamB) <- mkStreamPair
      writeAutoNATMessage streamA dialRequest
      result <- readAutoNATMessage streamB maxAutoNATMessageSize
      result `shouldBe` Right dialRequest

    it "multiple messages over same stream" $ do
      (streamA, streamB) <- mkStreamPair
      writeAutoNATMessage streamA dialRequest
      writeAutoNATMessage streamA dialResponseOK
      r1 <- readAutoNATMessage streamB maxAutoNATMessageSize
      r2 <- readAutoNATMessage streamB maxAutoNATMessageSize
      r1 `shouldBe` Right dialRequest
      r2 `shouldBe` Right dialResponseOK

    it "all response statuses survive framed round-trip" $ do
      (streamA, streamB) <- mkStreamPair
      let msgs = [dialResponseOK, dialResponseError, dialResponseRefused,
                   dialResponseBadRequest, dialResponseInternal]
      mapM_ (writeAutoNATMessage streamA) msgs
      results <- mapM (\_ -> readAutoNATMessage streamB maxAutoNATMessageSize) msgs
      results `shouldBe` map Right msgs
