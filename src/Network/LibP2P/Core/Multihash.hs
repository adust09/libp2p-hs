-- | Multihash encoding/decoding.
--
-- Multihash is a self-describing hash format: <hash-function-code><digest-size><digest>
-- Used for Peer ID derivation and content addressing.
module Network.LibP2P.Core.Multihash
  ( HashFunction (..)
  , encodeMultihash
  , decodeMultihash
  ) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import Network.LibP2P.Core.Varint (decodeUvarint, encodeUvarint)

-- | Supported hash functions for multihash encoding.
data HashFunction
  = Identity -- ^ Code 0x00: raw bytes, no hashing
  | SHA256   -- ^ Code 0x12: SHA-256 (32-byte digest)
  deriving (Show, Eq)

-- | Multicodec code for each hash function.
hashCode :: HashFunction -> Word64
hashCode Identity = 0x00
hashCode SHA256 = 0x12

-- | Lookup hash function from multicodec code.
fromHashCode :: Word64 -> Either String HashFunction
fromHashCode 0x00 = Right Identity
fromHashCode 0x12 = Right SHA256
fromHashCode c = Left $ "decodeMultihash: unknown hash function code 0x" <> showHex c
  where
    showHex :: Word64 -> String
    showHex n
      | n < 16 = "0" <> [hexDigit n]
      | otherwise = map hexDigit [n `div` 16, n `mod` 16]
    hexDigit :: Word64 -> Char
    hexDigit d
      | d < 10 = toEnum (fromIntegral d + fromEnum '0')
      | otherwise = toEnum (fromIntegral d - 10 + fromEnum 'a')

-- | Encode data as a multihash.
-- For Identity: stores raw bytes. For SHA256: hashes first, stores digest.
encodeMultihash :: HashFunction -> ByteString -> ByteString
encodeMultihash hf input =
  let digest = case hf of
        Identity -> input
        SHA256 -> convert (hash input :: Digest SHA256)
   in encodeUvarint (hashCode hf)
        <> encodeUvarint (fromIntegral (BS.length digest))
        <> digest

-- | Decode a multihash into its hash function and digest/data.
decodeMultihash :: ByteString -> Either String (HashFunction, ByteString)
decodeMultihash bs = do
  (code, rest1) <- decodeUvarint bs
  hf <- fromHashCode code
  (len, rest2) <- decodeUvarint rest1
  let digestLen = fromIntegral len :: Int
  if BS.length rest2 < digestLen
    then Left $ "decodeMultihash: expected " <> show digestLen <> " bytes but got " <> show (BS.length rest2)
    else Right (hf, BS.take digestLen rest2)
