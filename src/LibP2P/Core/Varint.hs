-- | Unsigned LEB128 varint encoding/decoding.
--
-- Used throughout libp2p for length-prefixed framing, protocol codes,
-- and multiaddr/multihash encoding.
--
-- Follows the multiformats unsigned-varint spec
-- (https://github.com/multiformats/unsigned-varint): varints are
-- restricted to a maximum of 9 bytes (63 bits), and non-minimal
-- (zero-padded) encodings are rejected on decode.
module LibP2P.Core.Varint
  ( encodeUvarint
  , decodeUvarint
  , maxVarintBytes
  ) where

import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word64)

-- | Maximum number of bytes for a valid unsigned varint.
-- The spec mandates: "Implementations MUST restrict the size of the
-- varint to a max of 9 bytes (63 bits)."
maxVarintBytes :: Int
maxVarintBytes = 9

-- | Largest value representable in a spec-compliant varint (2^63 - 1).
maxVarintValue :: Word64
maxVarintValue = (1 `shiftL` 63) - 1

-- | Encode a Word64 as an unsigned LEB128 varint.
-- Calls 'error' for values >= 2^63, which the spec makes unrepresentable
-- (go-varint's PutUvarint panics identically).
encodeUvarint :: Word64 -> ByteString
encodeUvarint n
  | n > maxVarintValue =
      error "encodeUvarint: value exceeds 63 bits (unsigned-varint spec maximum)"
  | otherwise = BL.toStrict (Builder.toLazyByteString (go n))
  where
    go :: Word64 -> Builder.Builder
    go m
      | m < 0x80 = Builder.word8 (fromIntegral m)
      | otherwise =
          Builder.word8 (fromIntegral (m .&. 0x7f) .|. 0x80)
            <> go (m `shiftR` 7)

-- | Decode an unsigned LEB128 varint from a ByteString.
-- Returns the decoded value and remaining bytes, or an error message.
-- Rejects varints longer than 9 bytes and non-minimal encodings
-- (a multi-byte varint whose final byte is 0x00).
decodeUvarint :: ByteString -> Either String (Word64, ByteString)
decodeUvarint bs
  | BS.null bs = Left "decodeUvarint: empty input"
  | otherwise = go bs 0 0
  where
    go :: ByteString -> Int -> Word64 -> Either String (Word64, ByteString)
    go input bitShift acc
      | bitShift >= maxVarintBytes * 7 =
          Left "decodeUvarint: varint too long (exceeds 9 bytes / 63 bits)"
      | BS.null input =
          Left "decodeUvarint: unexpected end of input"
      | otherwise =
          let byte = BS.head input
              rest = BS.tail input
              val = fromIntegral (byte .&. 0x7f) :: Word64
              acc' = acc .|. (val `shiftL` bitShift)
           in if byte .&. 0x80 == 0
                then
                  -- Spec: leading zeros "must be rejected when decoding.
                  -- The only number that can end in a 0x00 is 0" — and 0
                  -- is the single-byte encoding 0x00 (bitShift == 0).
                  if byte == 0x00 && bitShift > 0
                    then Left "decodeUvarint: non-minimal encoding (trailing zero byte)"
                    else Right (acc', rest)
                else go rest (bitShift + 7) acc'
