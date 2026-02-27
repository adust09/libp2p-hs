-- | XOR distance metric for the Kademlia DHT.
--
-- All distance computations operate on 256-bit SHA-256 keys.
-- Distance is the XOR of two keys interpreted as a 256-bit unsigned integer.
module Network.LibP2P.DHT.Distance
  ( peerIdToKey
  , xorDistance
  , commonPrefixLength
  , compareDistance
  , sortByDistance
  ) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bits (xor, testBit)
import Data.List (sortBy)
import Data.Word (Word8)
import Network.LibP2P.Crypto.PeerId (PeerId, peerIdBytes)
import Network.LibP2P.DHT.Types (BucketEntry (..), DHTKey (..))

-- | Convert a Peer ID to its DHT key by hashing with SHA-256.
peerIdToKey :: PeerId -> DHTKey
peerIdToKey pid =
  let digest = hash (peerIdBytes pid) :: Digest SHA256
  in DHTKey (convert digest)

-- | Compute XOR distance between two DHT keys.
xorDistance :: DHTKey -> DHTKey -> DHTKey
xorDistance (DHTKey a) (DHTKey b) =
  DHTKey (BS.pack (BS.zipWith xor a b))

-- | Count the number of leading zero bits (common prefix length).
-- Same key → 256. First bit differs → 0.
commonPrefixLength :: DHTKey -> DHTKey -> Int
commonPrefixLength a b =
  let (DHTKey d) = xorDistance a b
  in countLeadingZeros d

-- | Count leading zero bits in a ByteString (big-endian).
countLeadingZeros :: ByteString -> Int
countLeadingZeros bs = go 0
  where
    len = BS.length bs
    go i
      | i >= len  = i * 8
      | byte == 0 = go (i + 1)
      | otherwise = i * 8 + clzByte byte
      where byte = BS.index bs i

-- | Count leading zeros of a single byte (0-8).
clzByte :: Word8 -> Int
clzByte 0 = 8
clzByte w = go 7
  where
    go (-1) = 8
    go bit
      | testBit w bit = 7 - bit
      | otherwise     = go (bit - 1)

-- | Compare two keys by distance to a target.
-- Returns LT if a is closer to target than b, GT if farther, EQ if equal.
compareDistance :: DHTKey -> DHTKey -> DHTKey -> Ordering
compareDistance target a b =
  compare (xorDistance target a) (xorDistance target b)

-- | Sort bucket entries by ascending XOR distance to a target key.
sortByDistance :: DHTKey -> [BucketEntry] -> [BucketEntry]
sortByDistance target =
  sortBy (\a b -> compareDistance target (entryKey a) (entryKey b))
