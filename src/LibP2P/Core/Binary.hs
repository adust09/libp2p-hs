-- | Big-endian binary encoding/decoding helpers.
--
-- Shared utilities for network byte order (BE) encoding used by
-- Multiaddr, Noise framing, and other wire-format modules.
module LibP2P.Core.Binary
  ( word16BE
  , word32BE
  , word64BE
  , readWord16BE
  , readWord32BE
  , readWord64BE
  ) where

import Data.Binary.Get (getWord16be, getWord32be, getWord64be, runGet)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word16, Word32, Word64)

-- | Encode a Word16 as 2-byte big-endian ByteString.
word16BE :: Word16 -> ByteString
word16BE = BL.toStrict . Builder.toLazyByteString . Builder.word16BE

-- | Encode a Word32 as 4-byte big-endian ByteString.
word32BE :: Word32 -> ByteString
word32BE = BL.toStrict . Builder.toLazyByteString . Builder.word32BE

-- | Encode a Word64 as 8-byte big-endian ByteString.
word64BE :: Word64 -> ByteString
word64BE = BL.toStrict . Builder.toLazyByteString . Builder.word64BE

-- | Read a big-endian Word16 from a ByteString (must be >= 2 bytes).
readWord16BE :: ByteString -> Word16
readWord16BE = runGet getWord16be . BL.fromStrict

-- | Read a big-endian Word32 from a ByteString (must be >= 4 bytes).
readWord32BE :: ByteString -> Word32
readWord32BE = runGet getWord32be . BL.fromStrict

-- | Read a big-endian Word64 from a ByteString (must be >= 8 bytes).
readWord64BE :: ByteString -> Word64
readWord64BE = runGet getWord64be . BL.fromStrict
