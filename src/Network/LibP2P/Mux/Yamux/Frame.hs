-- | Yamux frame header encoding/decoding.
--
-- Every Yamux frame has a fixed 12-byte header:
-- Version (1) | Type (1) | Flags (2 BE) | StreamID (4 BE) | Length (4 BE)
module Network.LibP2P.Mux.Yamux.Frame
  ( FrameType (..)
  , Flags (..)
  , YamuxHeader (..)
  , GoAwayCode (..)
  , encodeHeader
  , decodeHeader
  , defaultFlags
  , headerSize
  , initialWindowSize
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word16, Word32, Word8)

-- | Yamux header size is always 12 bytes.
headerSize :: Int
headerSize = 12

-- | Default initial window size: 256 KiB (262144 bytes).
initialWindowSize :: Word32
initialWindowSize = 262144

-- | Yamux frame types.
data FrameType
  = FrameData         -- ^ 0x00: Data frame with payload
  | FrameWindowUpdate -- ^ 0x01: Window size increment
  | FramePing         -- ^ 0x02: Keepalive/latency measurement
  | FrameGoAway       -- ^ 0x03: Session termination
  deriving (Show, Eq)

-- | Go Away error codes.
data GoAwayCode
  = GoAwayNormal    -- ^ 0x00: Normal termination
  | GoAwayProtocol  -- ^ 0x01: Protocol error
  | GoAwayInternal  -- ^ 0x02: Internal error
  deriving (Show, Eq)

-- | Frame flags (bitmask).
data Flags = Flags
  { flagSYN :: !Bool -- ^ 0x0001: Open a new stream
  , flagACK :: !Bool -- ^ 0x0002: Acknowledge a new stream
  , flagFIN :: !Bool -- ^ 0x0004: Half-close the stream
  , flagRST :: !Bool -- ^ 0x0008: Reset the stream
  }
  deriving (Show, Eq)

-- | No flags set.
defaultFlags :: Flags
defaultFlags = Flags False False False False

-- | Complete Yamux frame header (12 bytes).
data YamuxHeader = YamuxHeader
  { yhVersion :: !Word8
  , yhType :: !FrameType
  , yhFlags :: !Flags
  , yhStreamId :: !Word32
  , yhLength :: !Word32
  }
  deriving (Show, Eq)

-- | Encode a frame type to its wire value.
frameTypeToWord8 :: FrameType -> Word8
frameTypeToWord8 FrameData = 0x00
frameTypeToWord8 FrameWindowUpdate = 0x01
frameTypeToWord8 FramePing = 0x02
frameTypeToWord8 FrameGoAway = 0x03

-- | Decode a frame type from wire value.
word8ToFrameType :: Word8 -> Either String FrameType
word8ToFrameType 0x00 = Right FrameData
word8ToFrameType 0x01 = Right FrameWindowUpdate
word8ToFrameType 0x02 = Right FramePing
word8ToFrameType 0x03 = Right FrameGoAway
word8ToFrameType n = Left $ "unknown frame type: " <> show n

-- | Encode flags to a big-endian uint16.
flagsToWord16 :: Flags -> Word16
flagsToWord16 (Flags syn ack fin rst) =
  (if syn then 0x0001 else 0)
    .|. (if ack then 0x0002 else 0)
    .|. (if fin then 0x0004 else 0)
    .|. (if rst then 0x0008 else 0)

-- | Decode flags from a big-endian uint16.
word16ToFlags :: Word16 -> Flags
word16ToFlags w =
  Flags
    { flagSYN = w .&. 0x0001 /= 0
    , flagACK = w .&. 0x0002 /= 0
    , flagFIN = w .&. 0x0004 /= 0
    , flagRST = w .&. 0x0008 /= 0
    }

-- | Encode a Yamux header to 12 bytes.
encodeHeader :: YamuxHeader -> ByteString
encodeHeader (YamuxHeader ver typ flags sid len) =
  let f = flagsToWord16 flags
   in BS.pack
        [ ver
        , frameTypeToWord8 typ
        , fromIntegral (f `shiftR` 8)
        , fromIntegral f
        , fromIntegral (sid `shiftR` 24)
        , fromIntegral (sid `shiftR` 16)
        , fromIntegral (sid `shiftR` 8)
        , fromIntegral sid
        , fromIntegral (len `shiftR` 24)
        , fromIntegral (len `shiftR` 16)
        , fromIntegral (len `shiftR` 8)
        , fromIntegral len
        ]

-- | Decode a Yamux header from 12 bytes.
decodeHeader :: ByteString -> Either String YamuxHeader
decodeHeader bs
  | BS.length bs < 12 = Left "decodeHeader: need 12 bytes"
  | otherwise = do
      let ver = BS.index bs 0
      typ <- word8ToFrameType (BS.index bs 1)
      let flags =
            word16ToFlags
              ( (fromIntegral (BS.index bs 2) `shiftL` 8)
                  .|. fromIntegral (BS.index bs 3)
              )
          sid =
            (fromIntegral (BS.index bs 4) `shiftL` 24)
              .|. (fromIntegral (BS.index bs 5) `shiftL` 16)
              .|. (fromIntegral (BS.index bs 6) `shiftL` 8)
              .|. fromIntegral (BS.index bs 7)
          len =
            (fromIntegral (BS.index bs 8) `shiftL` 24)
              .|. (fromIntegral (BS.index bs 9) `shiftL` 16)
              .|. (fromIntegral (BS.index bs 10) `shiftL` 8)
              .|. fromIntegral (BS.index bs 11)
      Right (YamuxHeader ver typ flags sid len)
