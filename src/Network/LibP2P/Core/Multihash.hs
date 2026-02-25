-- | Multihash encoding/decoding.
--
-- Multihash is a self-describing hash format: <hash-function-code><digest-size><digest>
-- Used for Peer ID derivation and content addressing.
module Network.LibP2P.Core.Multihash
  ( HashFunction (..)
  , encodeMultihash
  , decodeMultihash
  ) where

import Data.ByteString (ByteString)

-- | Supported hash functions for multihash encoding.
data HashFunction
  = Identity -- ^ Code 0x00: raw bytes, no hashing
  | SHA256   -- ^ Code 0x12: SHA-256 (32-byte digest)
  deriving (Show, Eq)

-- | Encode data as a multihash.
-- For Identity: stores raw bytes. For SHA256: hashes first, stores digest.
encodeMultihash :: HashFunction -> ByteString -> ByteString
encodeMultihash = error "Not yet implemented"

-- | Decode a multihash into its hash function and digest/data.
decodeMultihash :: ByteString -> Either String (HashFunction, ByteString)
decodeMultihash = error "Not yet implemented"
