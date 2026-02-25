module Test.Network.LibP2P.Core.VarintSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import Network.LibP2P.Core.Varint
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "encodeUvarint" $ do
    it "encodes 0 as 0x00" $
      encodeUvarint 0 `shouldBe` BS.pack [0x00]

    it "encodes 1 as 0x01" $
      encodeUvarint 1 `shouldBe` BS.pack [0x01]

    it "encodes 127 as 0x7f" $
      encodeUvarint 127 `shouldBe` BS.pack [0x7f]

    it "encodes 128 as 0x80 0x01" $
      encodeUvarint 128 `shouldBe` BS.pack [0x80, 0x01]

    it "encodes 300 as 0xac 0x02" $
      encodeUvarint 300 `shouldBe` BS.pack [0xac, 0x02]

    it "encodes 421 as 0xa5 0x03 (p2p protocol code)" $
      encodeUvarint 421 `shouldBe` BS.pack [0xa5, 0x03]

    it "encodes maxBound (2^64-1)" $
      -- 2^64-1 requires 10 bytes in LEB128
      BS.length (encodeUvarint maxBound) `shouldBe` 10

  describe "decodeUvarint" $ do
    it "decodes 0x00 as 0" $
      decodeUvarint (BS.pack [0x00]) `shouldBe` Right (0, BS.empty)

    it "decodes 0x01 as 1" $
      decodeUvarint (BS.pack [0x01]) `shouldBe` Right (1, BS.empty)

    it "decodes 0x7f as 127" $
      decodeUvarint (BS.pack [0x7f]) `shouldBe` Right (127, BS.empty)

    it "decodes 0x80 0x01 as 128" $
      decodeUvarint (BS.pack [0x80, 0x01]) `shouldBe` Right (128, BS.empty)

    it "decodes 0xac 0x02 as 300" $
      decodeUvarint (BS.pack [0xac, 0x02]) `shouldBe` Right (300, BS.empty)

    it "returns remaining bytes after varint" $
      decodeUvarint (BS.pack [0x01, 0xff, 0xfe])
        `shouldBe` Right (1, BS.pack [0xff, 0xfe])

    it "fails on empty input" $
      decodeUvarint BS.empty `shouldSatisfy` isLeft

    it "fails on unterminated varint (all continuation bits)" $
      decodeUvarint (BS.pack [0x80, 0x80]) `shouldSatisfy` isLeft

    it "fails on overlong varint (>10 bytes)" $
      let overlong = BS.pack (replicate 11 0x80)
       in decodeUvarint overlong `shouldSatisfy` isLeft

  describe "round-trip property" $ do
    it "decode(encode(x)) == x for all Word64" $
      property $ \(w :: Word64) ->
        decodeUvarint (encodeUvarint w) === Right (w, BS.empty)

-- | Helper to check if an Either is Left.
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
