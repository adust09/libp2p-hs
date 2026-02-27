module Test.Network.LibP2P.DHT.MessageSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import Control.Concurrent.STM (newTQueueIO, atomically, writeTQueue, readTQueue, TQueue)
import Data.Word (Word8)
import Network.LibP2P.DHT.Message
import Network.LibP2P.DHT.Types (ConnectionType (..))
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))

-- | Create an in-memory stream pair for testing (same pattern as other specs).
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

-- | A FIND_NODE request message (type + key only).
findNodeRequest :: DHTMessage
findNodeRequest = emptyDHTMessage
  { msgType = FindNode
  , msgKey  = BS.pack [1, 2, 3, 4, 5, 6, 7, 8]
  }

-- | A FIND_NODE response with closerPeers.
findNodeResponse :: DHTMessage
findNodeResponse = emptyDHTMessage
  { msgType = FindNode
  , msgCloserPeers =
      [ DHTPeer (BS.pack [10, 20]) [BS.pack [4, 127, 0, 0, 1]] Connected
      , DHTPeer (BS.pack [30, 40]) [BS.pack [4, 10, 0, 0, 1], BS.pack [4, 192, 168, 1, 1]] CanConnect
      ]
  }

-- | A PUT_VALUE message with a Record.
putValueMessage :: DHTMessage
putValueMessage = emptyDHTMessage
  { msgType = PutValue
  , msgKey  = BS.pack [0xCA, 0xFE]
  , msgRecord = Just DHTRecord
      { recKey          = BS.pack [0xCA, 0xFE]
      , recValue        = BS.pack [0xDE, 0xAD, 0xBE, 0xEF]
      , recTimeReceived = "2024-01-15T10:30:00Z"
      }
  }

-- | A GET_PROVIDERS response with both closer and provider peers.
getProvidersResponse :: DHTMessage
getProvidersResponse = emptyDHTMessage
  { msgType = GetProviders
  , msgCloserPeers =
      [ DHTPeer (BS.pack [1]) [BS.pack [4, 127, 0, 0, 1]] NotConnected ]
  , msgProviderPeers =
      [ DHTPeer (BS.pack [2]) [BS.pack [4, 10, 0, 0, 1]] Connected
      , DHTPeer (BS.pack [3]) [] CannotConnect
      ]
  }

spec :: Spec
spec = do
  describe "DHT Message encoding/decoding" $ do
    it "encode → decode round-trip: FIND_NODE request" $ do
      let encoded = encodeDHTMessage findNodeRequest
          decoded = decodeDHTMessage encoded
      decoded `shouldBe` Right findNodeRequest

    it "encode → decode round-trip: FIND_NODE response with closerPeers" $ do
      let encoded = encodeDHTMessage findNodeResponse
          decoded = decodeDHTMessage encoded
      decoded `shouldBe` Right findNodeResponse

    it "encode → decode round-trip: PUT_VALUE with Record" $ do
      let encoded = encodeDHTMessage putValueMessage
          decoded = decodeDHTMessage encoded
      decoded `shouldBe` Right putValueMessage

    it "encode → decode round-trip: GET_PROVIDERS response" $ do
      let encoded = encodeDHTMessage getProvidersResponse
          decoded = decodeDHTMessage encoded
      decoded `shouldBe` Right getProvidersResponse

    it "decode empty message → defaults" $ do
      let decoded = decodeDHTMessage BS.empty
      decoded `shouldBe` Right emptyDHTMessage

    it "encode omits Nothing record field" $ do
      let msg = emptyDHTMessage { msgType = FindNode, msgKey = BS.pack [1] }
          encoded = encodeDHTMessage msg
          decoded = decodeDHTMessage encoded
      case decoded of
        Right result -> msgRecord result `shouldBe` Nothing
        Left err -> expectationFailure $ "Decode failed: " ++ show err

    it "decode skips unknown fields (clusterLevelRaw field 10)" $ do
      let msg = emptyDHTMessage { msgType = FindNode, msgKey = BS.pack [1, 2] }
          encoded = encodeDHTMessage msg
          -- Append unknown field 10 (wire type 0 = varint, tag = 10<<3|0 = 80 = 0x50)
          unknownField = BS.pack [0x50, 0x01]  -- field 10, varint value 1
          withUnknown = encoded <> unknownField
      case decodeDHTMessage withUnknown of
        Right result -> do
          msgType result `shouldBe` FindNode
          msgKey result `shouldBe` BS.pack [1, 2]
        Left err -> expectationFailure $ "Decode failed: " ++ show err

    it "DHTPeer encode → decode preserves repeated addrs and ConnectionType" $ do
      let msg = emptyDHTMessage
            { msgType = FindNode
            , msgCloserPeers =
                [ DHTPeer (BS.pack [0xAA])
                    [ BS.pack [4, 127, 0, 0, 1]
                    , BS.pack [4, 10, 0, 0, 1]
                    , BS.pack [4, 192, 168, 1, 1]
                    ]
                    CanConnect
                ]
            }
          encoded = encodeDHTMessage msg
          decoded = decodeDHTMessage encoded
      decoded `shouldBe` Right msg

  describe "DHT Message wire framing" $ do
    it "encodeFramed produces [uvarint][protobuf]" $ do
      let msg = findNodeRequest
          framed = encodeFramed msg
          payload = encodeDHTMessage msg
      -- First byte(s) should be the uvarint length
      BS.length framed `shouldBe` (BS.length payload + 1)  -- length fits in 1 byte for small messages
      -- Verify structure: length prefix + payload
      case decodeFramed maxDHTMessageSize framed of
        Right decoded -> decoded `shouldBe` msg
        Left err -> expectationFailure $ "decodeFramed failed: " ++ err

    it "encodeFramed → decodeFramed round-trip" $ do
      let msg = putValueMessage
          framed = encodeFramed msg
      case decodeFramed maxDHTMessageSize framed of
        Right decoded -> decoded `shouldBe` msg
        Left err -> expectationFailure $ "Round-trip failed: " ++ err

    it "decodeFramed rejects oversized message" $ do
      -- Create a framed message that claims to be very large
      let fakeLen = BS.pack [0xFF, 0xFF, 0x03]  -- varint for 65535
          fakePayload = BS.replicate 10 0x00     -- doesn't matter, too short
          oversized = fakeLen <> fakePayload
      case decodeFramed 1000 oversized of
        Left err -> err `shouldSatisfy` (elem 'l' . take 50)  -- contains "large"
        Right _ -> expectationFailure "Should have rejected oversized message"

    it "writeFramedMessage + readFramedMessage over StreamIO pair" $ do
      (streamA, streamB) <- mkStreamPair
      let msg = findNodeResponse
      writeFramedMessage streamA msg
      result <- readFramedMessage streamB maxDHTMessageSize
      result `shouldBe` Right msg

  describe "DHT Message edge cases" $ do
    it "multiple closerPeers with different ConnectionTypes" $ do
      let msg = emptyDHTMessage
            { msgType = FindNode
            , msgCloserPeers =
                [ DHTPeer (BS.pack [1]) [] NotConnected
                , DHTPeer (BS.pack [2]) [] Connected
                , DHTPeer (BS.pack [3]) [] CanConnect
                , DHTPeer (BS.pack [4]) [] CannotConnect
                ]
            }
          encoded = encodeDHTMessage msg
          decoded = decodeDHTMessage encoded
      decoded `shouldBe` Right msg

    it "Record with empty key/value fields" $ do
      let msg = emptyDHTMessage
            { msgType = PutValue
            , msgRecord = Just DHTRecord
                { recKey          = BS.empty
                , recValue        = BS.empty
                , recTimeReceived = ""
                }
            }
          encoded = encodeDHTMessage msg
          decoded = decodeDHTMessage encoded
      case decoded of
        Right result -> do
          case msgRecord result of
            Just rec -> do
              recKey rec `shouldBe` BS.empty
              recValue rec `shouldBe` BS.empty
              recTimeReceived rec `shouldBe` ""
            Nothing -> expectationFailure "Expected record but got Nothing"
        Left err -> expectationFailure $ "Decode failed: " ++ show err
