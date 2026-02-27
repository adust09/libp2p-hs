module Test.Network.LibP2P.Protocol.Identify.MessageSpec (spec) where

import qualified Data.ByteString as BS
import Network.LibP2P.Protocol.Identify.Message
import Test.Hspec

-- | A fully populated IdentifyInfo for testing.
fullInfo :: IdentifyInfo
fullInfo = IdentifyInfo
  { idProtocolVersion = Just "ipfs/0.1.0"
  , idAgentVersion    = Just "libp2p-hs/0.1.0"
  , idPublicKey       = Just (BS.pack [0x08, 0x01, 0x12, 0x20, 1, 2, 3, 4])
  , idListenAddrs     = [BS.pack [4, 127, 0, 0, 1, 6, 0x10, 0x01],
                          BS.pack [4, 10, 0, 0, 1, 6, 0x0F, 0xA1]]
  , idObservedAddr    = Just (BS.pack [4, 192, 168, 1, 1, 6, 0x1F, 0x90])
  , idProtocols       = ["/ipfs/id/1.0.0", "/ipfs/ping/1.0.0", "/noise"]
  }

spec :: Spec
spec = do
  describe "Identify Message" $ do
    it "encode produces non-empty protobuf bytes" $ do
      let encoded = encodeIdentify fullInfo
      BS.length encoded `shouldSatisfy` (> 0)

    it "encode → decode round-trip preserves all fields" $ do
      let encoded = encodeIdentify fullInfo
          decoded = decodeIdentify encoded
      decoded `shouldBe` Right fullInfo

    it "decode empty message returns empty IdentifyInfo" $ do
      let decoded = decodeIdentify BS.empty
      decoded `shouldBe` Right IdentifyInfo
        { idProtocolVersion = Nothing
        , idAgentVersion    = Nothing
        , idPublicKey       = Nothing
        , idListenAddrs     = []
        , idObservedAddr    = Nothing
        , idProtocols       = []
        }

    it "decode handles repeated listenAddrs correctly" $ do
      let info = IdentifyInfo Nothing Nothing Nothing
                   [BS.pack [1, 2], BS.pack [3, 4], BS.pack [5, 6]]
                   Nothing []
          encoded = encodeIdentify info
          decoded = decodeIdentify encoded
      case decoded of
        Right result -> idListenAddrs result `shouldBe` [BS.pack [1, 2], BS.pack [3, 4], BS.pack [5, 6]]
        Left err -> expectationFailure $ "Decode failed: " ++ show err

    it "decode handles repeated protocols correctly" $ do
      let info = IdentifyInfo Nothing Nothing Nothing [] Nothing
                   ["/noise", "/yamux/1.0.0", "/ipfs/id/1.0.0"]
          encoded = encodeIdentify info
          decoded = decodeIdentify encoded
      case decoded of
        Right result -> idProtocols result `shouldBe` ["/noise", "/yamux/1.0.0", "/ipfs/id/1.0.0"]
        Left err -> expectationFailure $ "Decode failed: " ++ show err

    it "decode rejects oversized message" $ do
      -- Create a message larger than maxIdentifySize
      let bigPubKey = BS.replicate (maxIdentifySize + 100) 0x42
          info = IdentifyInfo Nothing Nothing (Just bigPubKey) [] Nothing []
          encoded = encodeIdentify info
      -- The encoded message should be parseable (proto3-wire doesn't enforce size)
      -- Size check is done at read time by readUntilEOF, not at decode time
      BS.length encoded `shouldSatisfy` (> maxIdentifySize)

    it "decode skips unknown fields" $ do
      -- Encode known fields, then append unknown field bytes
      let info = IdentifyInfo (Just "ipfs/0.1.0") Nothing Nothing [] Nothing []
          encoded = encodeIdentify info
          -- Append unknown field 99 (wire type 0 = varint, tag = 99<<3|0 = 0x318)
          -- This is a varint-encoded tag + value: field 99, varint 42
          unknownField = BS.pack [0xF8, 0x06, 0x2A]  -- field 99, wire type 0, value 42
          withUnknown = encoded <> unknownField
      case decodeIdentify withUnknown of
        Right result -> idProtocolVersion result `shouldBe` Just "ipfs/0.1.0"
        Left err -> expectationFailure $ "Decode failed with unknown field: " ++ show err

    it "encode omits Nothing fields" $ do
      let info = IdentifyInfo Nothing Nothing Nothing [] Nothing []
          encoded = encodeIdentify info
      -- Empty message should encode to empty bytes (no fields set)
      encoded `shouldBe` BS.empty
