-- | Unsigned LEB128 varint encoding/decoding.
--
-- Used throughout libp2p for length-prefixed framing, protocol codes,
-- and multiaddr/multihash encoding.
module Network.LibP2P.Core.Varint
  ( encodeUvarint
  , decodeUvarint
  ) where

import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word64, Word8)

-- | Maximum number of bytes for a valid unsigned varint (ceil(64/7) = 10).
maxVarintBytes :: Int
maxVarintBytes = 10

-- | Encode a Word64 as an unsigned LEB128 varint.
encodeUvarint :: Word64 -> ByteString
encodeUvarint = LBS.toStrict . Builder.toLazyByteString . go
  where
    go :: Word64 -> Builder.Builder
    go n
      | n < 0x80 = Builder.word8 (fromIntegral n)
      | otherwise =
          Builder.word8 (fromIntegral (n .&. 0x7f) .|. 0x80)
            <> go (n `shiftR` 7)

-- | Decode an unsigned LEB128 varint from a ByteString.
-- Returns the decoded value and remaining bytes, or an error message.
decodeUvarint :: ByteString -> Either String (Word64, ByteString)
decodeUvarint bs
  | BS.null bs = Left "decodeUvarint: empty input"
  | otherwise = go bs 0 0
  where
    go :: ByteString -> Int -> Word64 -> Either String (Word64, ByteString)
    go input shift acc
      | shift >= maxVarintBytes * 7 =
          Left "decodeUvarint: varint too long (exceeds 10 bytes)"
      | BS.null input =
          Left "decodeUvarint: unexpected end of input"
      | otherwise =
          let byte = BS.head input
              rest = BS.tail input
              val = fromIntegral (byte .&. 0x7f) :: Word64
              acc' = acc .|. (val `shiftL` shift)
           in if byte .&. 0x80 == 0
                then Right (acc', rest)
                else go rest (shift + 7) acc'
