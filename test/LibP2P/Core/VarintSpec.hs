module LibP2P.Core.VarintSpec (spec) where

import Control.Exception (evaluate)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import LibP2P.Core.Varint
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

    -- multiformats unsigned-varint spec: max 9 bytes / 63 bits.
    -- 2^63-1 is the largest encodable value and takes exactly 9 bytes.
    it "encodes 2^63-1 (spec maximum) as 9 bytes" $
      encodeUvarint (2 ^ (63 :: Int) - 1)
        `shouldBe` BS.pack (replicate 8 0xff ++ [0x7f])

    it "rejects values >= 2^63 (would need a 10-byte encoding)" $
      evaluate (encodeUvarint maxBound) `shouldThrow` anyErrorCall

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

    -- multiformats unsigned-varint spec: "Implementations MUST restrict
    -- the size of the varint to a max of 9 bytes (63 bits)."
    it "fails on 10-byte varint (spec max is 9 bytes / 63 bits)" $
      let tenBytes = BS.pack (replicate 9 0x80 ++ [0x01])
       in decodeUvarint tenBytes `shouldSatisfy` isLeft

    it "accepts a 9-byte varint at the 63-bit maximum" $
      decodeUvarint (BS.pack (replicate 8 0xff ++ [0x7f]))
        `shouldBe` Right (2 ^ (63 :: Int) - 1, BS.empty)

    -- multiformats unsigned-varint spec: "Leading zeros must be trimmed
    -- when encoding and must be rejected when decoding." The only number
    -- that can end in a 0x00 byte is 0.
    it "rejects non-minimal encoding 0x81 0x00 (padded 1)" $
      decodeUvarint (BS.pack [0x81, 0x00]) `shouldSatisfy` isLeft

    it "rejects non-minimal encoding 0x80 0x00 (padded 0)" $
      decodeUvarint (BS.pack [0x80, 0x00]) `shouldSatisfy` isLeft

    it "rejects non-minimal encoding 0xff 0xff 0x00" $
      decodeUvarint (BS.pack [0xff, 0xff, 0x00]) `shouldSatisfy` isLeft

  describe "round-trip property" $ do
    -- Restricted to 63-bit values: the unsigned-varint spec caps varints
    -- at 9 bytes (63 bits), so values >= 2^63 are not encodable.
    it "decode(encode(x)) == x for all 63-bit values" $
      property $
        forAll (choose (0, 2 ^ (63 :: Int) - 1)) $ \(w :: Word64) ->
          decodeUvarint (encodeUvarint w) === Right (w, BS.empty)

-- | Helper to check if an Either is Left.
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
