module Test.Network.LibP2P.MultistreamSelect.NegotiationSpec (spec) where

import Control.Concurrent.Async (concurrently, race, withAsync)
import qualified Data.ByteString as BS
import Network.LibP2P.MultistreamSelect.Negotiation
import Network.LibP2P.MultistreamSelect.Wire
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
  describe "Wire encoding" $ do
    it "encodes /multistream/1.0.0 correctly" $ do
      -- "/multistream/1.0.0" = 18 chars + '\n' = 19 bytes payload
      let encoded = encodeMessage "/multistream/1.0.0"
      BS.head encoded `shouldBe` 0x13 -- varint(19)
      BS.last encoded `shouldBe` 0x0a
      BS.length encoded `shouldBe` 20 -- 1 (varint) + 18 (text) + 1 (\n)

    it "encodes /noise correctly" $ do
      let encoded = encodeMessage "/noise"
      encoded `shouldBe` BS.pack [0x07, 0x2f, 0x6e, 0x6f, 0x69, 0x73, 0x65, 0x0a]

    it "encodes na correctly" $ do
      let encoded = encodeMessage "na"
      encoded `shouldBe` BS.pack [0x03, 0x6e, 0x61, 0x0a]

  describe "Wire decoding" $ do
    it "decodes /noise from bytes" $ do
      let bytes = BS.pack [0x07, 0x2f, 0x6e, 0x6f, 0x69, 0x73, 0x65, 0x0a]
      decodeMessage bytes `shouldBe` Right ("/noise", BS.empty)

    it "decodes with remaining bytes" $ do
      let bytes = BS.pack [0x03, 0x6e, 0x61, 0x0a, 0xff, 0xfe]
      decodeMessage bytes `shouldBe` Right ("na", BS.pack [0xff, 0xfe])

    it "fails on empty input" $
      decodeMessage BS.empty `shouldSatisfy` isLeft

  describe "Wire round-trip" $ do
    it "decode(encode(msg)) == msg" $ do
      let msg = "/ipfs/id/1.0.0"
      case decodeMessage (encodeMessage msg) of
        Right (decoded, remaining) -> do
          decoded `shouldBe` msg
          remaining `shouldBe` BS.empty
        Left err -> expectationFailure err

  describe "Negotiation - first protocol accepted" $ do
    it "negotiates /noise successfully" $ do
      (streamA, streamB) <- mkMemoryStreamPair
      (initResult, respResult) <-
        concurrently
          (negotiateInitiator streamA ["/noise"])
          (negotiateResponder streamB ["/noise", "/yamux/1.0.0"])
      initResult `shouldBe` Accepted "/noise"
      respResult `shouldBe` Accepted "/noise"

  describe "Negotiation - fallback to second protocol" $ do
    it "rejects /tls then accepts /noise" $ do
      (streamA, streamB) <- mkMemoryStreamPair
      (initResult, respResult) <-
        concurrently
          (negotiateInitiator streamA ["/tls/1.0.0", "/noise"])
          (negotiateResponder streamB ["/noise"])
      initResult `shouldBe` Accepted "/noise"
      respResult `shouldBe` Accepted "/noise"

  describe "Negotiation - no common protocol" $ do
    it "initiator returns NoProtocol when nothing matches" $ do
      (streamA, streamB) <- mkMemoryStreamPair
      -- Use withAsync so we can cancel the responder when initiator finishes
      withAsync (negotiateResponder streamB ["/bar"]) $ \_ -> do
        initResult <- negotiateInitiator streamA ["/foo"]
        initResult `shouldBe` NoProtocol

  describe "Negotiation - yamux" $ do
    it "negotiates muxer protocol" $ do
      (streamA, streamB) <- mkMemoryStreamPair
      (initResult, respResult) <-
        concurrently
          (negotiateInitiator streamA ["/yamux/1.0.0"])
          (negotiateResponder streamB ["/yamux/1.0.0"])
      initResult `shouldBe` Accepted "/yamux/1.0.0"
      respResult `shouldBe` Accepted "/yamux/1.0.0"

  describe "Wire decoding safety" $ do
    it "decodeMessage returns Left on invalid UTF-8 bytes" $ do
      -- Construct a framed message with invalid UTF-8: 0xFF 0xFE followed by '\n'
      let invalidUtf8 = BS.pack [0xFF, 0xFE, 0x0a]
          -- varint length = 3, then payload
          framed = BS.pack [0x03] <> invalidUtf8
      decodeMessage framed `shouldSatisfy` isLeft

    it "decodeMessage returns Left on truncated UTF-8 sequence" $ do
      -- 0xC0 starts a 2-byte UTF-8 sequence but is followed by '\n' instead of continuation
      let truncated = BS.pack [0xC0, 0x0a]
          framed = BS.pack [0x02] <> truncated
      decodeMessage framed `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
