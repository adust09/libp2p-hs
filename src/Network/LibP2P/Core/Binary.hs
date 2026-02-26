-- | Big-endian binary encoding/decoding helpers.
--
-- Shared utilities for network byte order (BE) encoding used by
-- Multiaddr, Noise framing, and other wire-format modules.
module Network.LibP2P.Core.Binary
  ( word16BE
  , word32BE
  , readWord16BE
  , readWord32BE
  ) where

import Data.Binary.Get (getWord16be, getWord32be, runGet)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word16, Word32)

-- | Encode a Word16 as 2-byte big-endian ByteString.
word16BE :: Word16 -> ByteString
word16BE = LBS.toStrict . Builder.toLazyByteString . Builder.word16BE

-- | Encode a Word32 as 4-byte big-endian ByteString.
word32BE :: Word32 -> ByteString
word32BE = LBS.toStrict . Builder.toLazyByteString . Builder.word32BE

-- | Read a big-endian Word16 from a ByteString (must be >= 2 bytes).
readWord16BE :: ByteString -> Word16
readWord16BE = runGet getWord16be . LBS.fromStrict

-- | Read a big-endian Word32 from a ByteString (must be >= 4 bytes).
readWord32BE :: ByteString -> Word32
readWord32BE = runGet getWord32be . LBS.fromStrict
