-- | Unsigned LEB128 varint encoding/decoding.
--
-- Used throughout libp2p for length-prefixed framing, protocol codes,
-- and multiaddr/multihash encoding.
module Network.LibP2P.Core.Varint
  ( encodeUvarint
  , decodeUvarint
  ) where

import Data.ByteString (ByteString)
import Data.Word (Word64)

-- | Encode a Word64 as an unsigned LEB128 varint.
encodeUvarint :: Word64 -> ByteString
encodeUvarint = error "Not yet implemented"

-- | Decode an unsigned LEB128 varint from a ByteString.
-- Returns the decoded value and remaining bytes, or an error message.
decodeUvarint :: ByteString -> Either String (Word64, ByteString)
decodeUvarint = error "Not yet implemented"
