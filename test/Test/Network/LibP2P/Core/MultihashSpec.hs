module Test.Network.LibP2P.Core.MultihashSpec (spec) where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Network.LibP2P.Core.Multihash
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "encodeMultihash" $ do
    it "encodes identity multihash: 0x00 + varint(len) + data" $ do
      let input = BS.pack [0x08, 0x01, 0x12, 0x20] -- 4 bytes
      let result = encodeMultihash Identity input
      -- 0x00 (identity) + 0x04 (length=4) + data
      result `shouldBe` BS.pack ([0x00, 0x04] <> [0x08, 0x01, 0x12, 0x20])

    it "encodes SHA-256 multihash: 0x12 0x20 + 32-byte digest" $ do
      let input = BS.replicate 100 0xAB -- arbitrary data to hash
      let result = encodeMultihash SHA256 input
      -- First two bytes: 0x12 (sha2-256 code) + 0x20 (32 = digest length)
      BS.take 2 result `shouldBe` BS.pack [0x12, 0x20]
      -- Total length: 2 + 32 = 34
      BS.length result `shouldBe` 34

    it "identity multihash preserves original data" $ do
      let input = BS.pack [0x01, 0x02, 0x03]
      let result = encodeMultihash Identity input
      -- 0x00 + 0x03 + original data
      result `shouldBe` BS.pack [0x00, 0x03, 0x01, 0x02, 0x03]

    it "SHA-256 of empty input produces valid multihash" $ do
      let result = encodeMultihash SHA256 BS.empty
      BS.take 2 result `shouldBe` BS.pack [0x12, 0x20]
      BS.length result `shouldBe` 34

  describe "decodeMultihash" $ do
    it "decodes identity multihash" $ do
      let encoded = BS.pack [0x00, 0x03, 0xAA, 0xBB, 0xCC]
      decodeMultihash encoded `shouldBe` Right (Identity, BS.pack [0xAA, 0xBB, 0xCC])

    it "decodes SHA-256 multihash" $ do
      let digest = BS.replicate 32 0x42
      let encoded = BS.pack [0x12, 0x20] <> digest
      decodeMultihash encoded `shouldBe` Right (SHA256, digest)

    it "fails on empty input" $
      decodeMultihash BS.empty `shouldSatisfy` isLeft

    it "fails on unknown hash function code" $ do
      let encoded = BS.pack [0xFF, 0x01, 0x00]
      decodeMultihash encoded `shouldSatisfy` isLeft

    it "fails when digest length mismatches" $ do
      -- Claims 5 bytes but only 2 available
      let encoded = BS.pack [0x00, 0x05, 0xAA, 0xBB]
      decodeMultihash encoded `shouldSatisfy` isLeft

  describe "round-trip property" $ do
    it "decode(encode(Identity, data)) == (Identity, data)" $
      property $ \(bs :: [Word8]) ->
        let input = BS.pack bs
            encoded = encodeMultihash Identity input
         in decodeMultihash encoded === Right (Identity, input)

    it "decode(encode(SHA256, data)) produces 32-byte digest" $
      property $ \(bs :: [Word8]) ->
        let input = BS.pack bs
            encoded = encodeMultihash SHA256 input
         in case decodeMultihash encoded of
              Right (SHA256, digest) -> BS.length digest === 32
              other -> counterexample (show other) False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
