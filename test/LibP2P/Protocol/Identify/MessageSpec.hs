module LibP2P.Protocol.Identify.MessageSpec (spec) where

import qualified Data.ByteString as BS
import LibP2P.Protocol.Identify.Message
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

    it "encodes each field with the spec's field number and wire type" $ do
      -- specs/identify/README.md protobuf: publicKey = 1, listenAddrs = 2,
      -- protocols = 3, observedAddr = 4, protocolVersion = 5,
      -- agentVersion = 6 — all length-delimited (wire type 2), so the
      -- first tag byte of a single-field message is (field << 3) | 2.
      let empty = IdentifyInfo Nothing Nothing Nothing [] Nothing []
          firstByte info = BS.head (encodeIdentify info)
      firstByte empty { idPublicKey = Just (BS.pack [1]) }   `shouldBe` 0x0a
      firstByte empty { idListenAddrs = [BS.pack [1]] }      `shouldBe` 0x12
      firstByte empty { idProtocols = ["/x/1.0.0"] }         `shouldBe` 0x1a
      firstByte empty { idObservedAddr = Just (BS.pack [1]) } `shouldBe` 0x22
      firstByte empty { idProtocolVersion = Just "v" }       `shouldBe` 0x2a
      firstByte empty { idAgentVersion = Just "v" }          `shouldBe` 0x32

    it "decodes a go-libp2p-shaped message (hand-written golden vector)" $ do
      -- Hand-constructed per the spec protobuf, deliberately NOT produced
      -- by encodeIdentify, so encoder and decoder cannot agree on a
      -- convention that is internally consistent but externally wrong.
      let golden = BS.concat
            [ -- listenAddrs = 2 (bytes): /ip4/127.0.0.1/tcp/4001
              BS.pack [0x12, 0x08, 0x04, 0x7f, 0x00, 0x00, 0x01, 0x06, 0x0f, 0xa1]
              -- protocols = 3 (string): "/ipfs/id/1.0.0"
            , BS.pack [0x1a, 0x0e], "/ipfs/id/1.0.0"
              -- protocols = 3 (string): "/ipfs/ping/1.0.0"
            , BS.pack [0x1a, 0x10], "/ipfs/ping/1.0.0"
              -- protocolVersion = 5 (string): "ipfs/0.1.0"
            , BS.pack [0x2a, 0x0a], "ipfs/0.1.0"
              -- agentVersion = 6 (string): "go-libp2p/0.36.4"
            , BS.pack [0x32, 0x10], "go-libp2p/0.36.4"
            ]
      case decodeIdentify golden of
        Left err -> expectationFailure $ "Decode failed: " ++ show err
        Right info -> do
          idAgentVersion info `shouldBe` Just "go-libp2p/0.36.4"
          idProtocolVersion info `shouldBe` Just "ipfs/0.1.0"
          idListenAddrs info `shouldBe`
            [BS.pack [0x04, 0x7f, 0x00, 0x00, 0x01, 0x06, 0x0f, 0xa1]]
          idProtocols info `shouldBe` ["/ipfs/id/1.0.0", "/ipfs/ping/1.0.0"]

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
