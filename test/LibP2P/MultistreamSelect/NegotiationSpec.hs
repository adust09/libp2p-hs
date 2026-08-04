module LibP2P.MultistreamSelect.NegotiationSpec (spec) where

import Control.Concurrent.Async (concurrently, withAsync)
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Word (Word8)
import LibP2P.MultistreamSelect.Negotiation
import LibP2P.MultistreamSelect.Wire
import System.Timeout (timeout)
import Test.Hspec

-- | Read exactly n raw bytes from a StreamIO (test-side helper for
-- inspecting the wire without going through the message decoder).
readRawBytes :: StreamIO -> Int -> IO ByteString
readRawBytes s n = BS.pack <$> replicateM n (streamReadByte s)

-- | The exact wire encoding of the "/multistream/1.0.0" header:
-- varint(19), the 18 ASCII bytes of the protocol id, trailing newline.
-- Transcribed from the multistream-select spec, not computed — a typo in
-- the header constant must not be able to satisfy this test.
multistreamHeaderBytes :: ByteString
multistreamHeaderBytes = BS.pack
  [ 0x13
  , 0x2f, 0x6d, 0x75, 0x6c, 0x74, 0x69, 0x73, 0x74, 0x72, 0x65
  , 0x61, 0x6d, 0x2f, 0x31, 0x2e, 0x30, 0x2e, 0x30
  , 0x0a
  ]

spec :: Spec
spec = do
  describe "Wire encoding" $ do
    it "encodes /multistream/1.0.0 to the exact spec bytes" $
      encodeMessage "/multistream/1.0.0" `shouldBe` multistreamHeaderBytes

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

  describe "Negotiation - differing protocol tables" $ do
    it "negotiates the only shared protocol after multiple na rounds" $ do
      (streamA, streamB) <- mkMemoryStreamPair
      (initResult, respResult) <-
        concurrently
          (negotiateInitiator streamA ["/tls/1.0.0", "/mplex/6.7.0", "/noise"])
          (negotiateResponder streamB ["/yamux/1.0.0", "/noise"])
      initResult `shouldBe` Accepted "/noise"
      respResult `shouldBe` Accepted "/noise"

    it "responder answers an unsupported proposal with the exact na bytes" $ do
      (streamA, streamB) <- mkMemoryStreamPair
      -- Hand-rolled initiator: instead of pairing the responder with our
      -- own initiator (which by construction agrees on every byte), speak
      -- the protocol manually and compare the responder's raw wire output
      -- against the bytes the spec mandates.
      withAsync (negotiateResponder streamB ["/bar"]) $ \_ -> do
        streamWrite streamA (encodeMessage multistreamHeader)
        headerEcho <- readRawBytes streamA 20
        headerEcho `shouldBe` multistreamHeaderBytes
        streamWrite streamA (encodeMessage "/foo")
        naBytes <- readRawBytes streamA 4
        naBytes `shouldBe` BS.pack [0x03, 0x6e, 0x61, 0x0a]

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

  describe "Negotiation - pre-handshake read limits" $ do
    -- go-multistream rejects messages over 1024 bytes ("incoming message
    -- was too large"). Without this cap an unauthenticated peer can declare
    -- a huge length and make us allocate/read without bound.
    it "rejects an oversized declared message length before reading the payload" $ do
      (streamA, streamB) <- mkMemoryStreamPair
      -- Declare a (2^32 - 1)-byte message; only the varint prefix is sent.
      -- The responder must reject the length instead of blocking to read 4 GiB.
      streamWrite streamA (BS.pack [0xff, 0xff, 0xff, 0xff, 0x0f])
      result <- timeout 1000000 (negotiateResponder streamB ["/noise"])
      result `shouldBe` Just NoProtocol

    it "rejects a message length just above the 1024-byte cap" $ do
      (streamA, streamB) <- mkMemoryStreamPair
      -- varint(1025) = 0x81 0x08; no payload follows
      streamWrite streamA (BS.pack [0x81, 0x08])
      result <- timeout 1000000 (negotiateResponder streamB ["/noise"])
      result `shouldBe` Just NoProtocol

    -- The varint read loop itself must be bounded: the unsigned-varint spec
    -- caps varints at 9 bytes, so a stream of continuation bytes (0x80) must
    -- be aborted instead of being accumulated indefinitely.
    it "aborts an unterminated varint prefix after the 9-byte spec limit" $ do
      (streamA, streamB) <- mkMemoryStreamPair
      streamWrite streamA (BS.pack (replicate 64 0x80))
      result <- timeout 1000000 (negotiateResponder streamB ["/noise"])
      result `shouldBe` Just NoProtocol

  describe "readExactBounded" $ do
    it "reads exactly n bytes when n is within the bound" $ do
      (streamA, streamB) <- mkMemoryStreamPair
      let payload = BS.pack [1 .. 10]
      streamWrite streamA payload
      result <- readExactBounded streamB 1024 10
      result `shouldBe` Right payload

    it "reads zero bytes as the empty string without touching the stream" $ do
      (_, streamB) <- mkMemoryStreamPair
      result <- readExactBounded streamB 1024 0
      result `shouldBe` Right BS.empty

    it "accepts n equal to the maximum" $ do
      (streamA, streamB) <- mkMemoryStreamPair
      let payload = BS.replicate 16 0xab
      streamWrite streamA payload
      result <- readExactBounded streamB 16 16
      result `shouldBe` Right payload

    it "rejects n above the maximum before reading any byte" $ do
      (streamA, streamB) <- mkMemoryStreamPair
      streamWrite streamA (BS.pack [1, 2, 3])
      result <- readExactBounded streamB 16 17
      result `shouldSatisfy` isLeft
      -- No byte was consumed: the buffered bytes are still readable.
      after' <- readExactBounded streamB 16 3
      after' `shouldBe` Right (BS.pack [1, 2, 3])

    it "rejects a negative length" $ do
      (_, streamB) <- mkMemoryStreamPair
      result <- readExactBounded streamB 16 (-1)
      result `shouldSatisfy` isLeft

    it "returns Left instead of throwing when the stream fails mid-read" $ do
      -- A stream that yields 5 bytes and then fails (EOF).
      remaining <- newIORef (5 :: Int)
      let failingStream = StreamIO
            { streamWrite = \_ -> pure ()
            , streamReadByte = do
                left <- atomicModifyIORef' remaining (\k -> (k - 1, k))
                if left > 0
                  then pure (0x2a :: Word8)
                  else fail "connection reset"
            , streamClose = pure ()
            }
      result <- readExactBounded failingStream 1024 10
      case result of
        Left err -> err `shouldContain` "read failed"
        Right bs -> expectationFailure ("expected Left, got " ++ show bs)

    it "assembles reads larger than one chunk correctly" $ do
      -- 70000 bytes spans three 32 KiB chunks; the reassembled bytes
      -- must be identical and in order.
      (streamA, streamB) <- mkMemoryStreamPair
      let payload = BS.pack (map fromIntegral [(0 :: Int) .. 69999])
      -- The in-memory pair is backed by an unbounded queue, so the
      -- whole payload can be written before reading it back.
      streamWrite streamA payload
      result <- readExactBounded streamB 131072 70000
      result `shouldBe` Right payload

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
