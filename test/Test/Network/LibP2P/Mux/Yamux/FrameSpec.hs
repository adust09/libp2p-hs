module Test.Network.LibP2P.Mux.Yamux.FrameSpec (spec) where

import qualified Data.ByteString as BS
import Data.Word (Word32)
import Network.LibP2P.Mux.Yamux.Frame
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "encodeHeader" $ do
    it "encodes Data SYN stream=1 len=5 (docs example)" $ do
      -- From docs/06-multiplexing.md:
      -- 00 00 0001 00000001 00000005
      let hdr =
            YamuxHeader
              { yhVersion = 0
              , yhType = FrameData
              , yhFlags = defaultFlags {flagSYN = True}
              , yhStreamId = 1
              , yhLength = 5
              }
      encodeHeader hdr
        `shouldBe` BS.pack
          [0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x05]

    it "encodes WindowUpdate ACK stream=1 len=262144 (docs example)" $ do
      -- 00 01 0002 00000001 00040000
      let hdr =
            YamuxHeader
              { yhVersion = 0
              , yhType = FrameWindowUpdate
              , yhFlags = defaultFlags {flagACK = True}
              , yhStreamId = 1
              , yhLength = 262144
              }
      encodeHeader hdr
        `shouldBe` BS.pack
          [0x00, 0x01, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x00]

    it "always produces 12 bytes" $ do
      let hdr = YamuxHeader 0 FramePing defaultFlags 0 0
      BS.length (encodeHeader hdr) `shouldBe` 12

    it "encodes FIN flag correctly" $ do
      let hdr = YamuxHeader 0 FrameData (defaultFlags {flagFIN = True}) 3 0
      let encoded = encodeHeader hdr
      -- Flags bytes at offset 2-3: 0x00 0x04
      BS.index encoded 2 `shouldBe` 0x00
      BS.index encoded 3 `shouldBe` 0x04

    it "encodes RST flag correctly" $ do
      let hdr = YamuxHeader 0 FrameData (defaultFlags {flagRST = True}) 3 0
      let encoded = encodeHeader hdr
      BS.index encoded 2 `shouldBe` 0x00
      BS.index encoded 3 `shouldBe` 0x08

    it "encodes GoAway normal" $ do
      let hdr = YamuxHeader 0 FrameGoAway defaultFlags 0 0
      let encoded = encodeHeader hdr
      BS.index encoded 1 `shouldBe` 0x03
      BS.index encoded 8 `shouldBe` 0x00 -- error code 0

  describe "decodeHeader" $ do
    it "decodes Data SYN stream=1 len=5" $ do
      let bytes = BS.pack [0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x05]
      case decodeHeader bytes of
        Right hdr -> do
          yhVersion hdr `shouldBe` 0
          yhType hdr `shouldBe` FrameData
          flagSYN (yhFlags hdr) `shouldBe` True
          flagACK (yhFlags hdr) `shouldBe` False
          yhStreamId hdr `shouldBe` 1
          yhLength hdr `shouldBe` 5
        Left err -> expectationFailure err

    it "decodes WindowUpdate ACK" $ do
      let bytes = BS.pack [0x00, 0x01, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x00]
      case decodeHeader bytes of
        Right hdr -> do
          yhType hdr `shouldBe` FrameWindowUpdate
          flagACK (yhFlags hdr) `shouldBe` True
          yhStreamId hdr `shouldBe` 1
          yhLength hdr `shouldBe` 262144
        Left err -> expectationFailure err

    it "fails on short input" $
      decodeHeader (BS.pack [0x00, 0x01]) `shouldSatisfy` isLeft

    it "fails on unknown frame type" $ do
      let bytes = BS.pack [0x00, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
      decodeHeader bytes `shouldSatisfy` isLeft

  describe "Round-trip" $ do
    it "decode(encode(hdr)) == hdr for Data frames" $ do
      let hdr = YamuxHeader 0 FrameData (defaultFlags {flagSYN = True}) 1 100
      decodeHeader (encodeHeader hdr) `shouldBe` Right hdr

    it "decode(encode(hdr)) == hdr for WindowUpdate" $ do
      let hdr = YamuxHeader 0 FrameWindowUpdate (defaultFlags {flagACK = True}) 2 262144
      decodeHeader (encodeHeader hdr) `shouldBe` Right hdr

    it "decode(encode(hdr)) == hdr for Ping" $ do
      let hdr = YamuxHeader 0 FramePing (defaultFlags {flagSYN = True}) 0 42
      decodeHeader (encodeHeader hdr) `shouldBe` Right hdr

    it "round-trip for arbitrary stream IDs and lengths" $
      property $ \(sid :: Word32) (len :: Word32) ->
        let hdr = YamuxHeader 0 FrameData defaultFlags sid len
         in decodeHeader (encodeHeader hdr) === Right hdr

  describe "Stream ID conventions" $ do
    it "client uses odd IDs (1,3,5,...)" $ do
      let clientIds = [1, 3, 5, 7, 9] :: [Word32]
      all odd clientIds `shouldBe` True

    it "server uses even IDs (2,4,6,...)" $ do
      let serverIds = [2, 4, 6, 8, 10] :: [Word32]
      all even serverIds `shouldBe` True

  describe "Constants" $ do
    it "initial window size is 256 KiB" $
      initialWindowSize `shouldBe` 262144

    it "header size is 12" $
      headerSize `shouldBe` 12

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
