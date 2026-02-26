-- | Noise message framing: 2-byte big-endian length prefix.
--
-- All Noise messages (handshake and transport) are framed as:
-- [2 bytes BE length][noise_message]
module Network.LibP2P.Security.Noise.Framing
  ( encodeFrame
  , decodeFrame
  , maxNoiseMessageSize
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.LibP2P.Core.Binary (readWord16BE, word16BE)

-- | Maximum Noise message size (limited by 2-byte length prefix).
maxNoiseMessageSize :: Int
maxNoiseMessageSize = 65535

-- | Encode a Noise message with a 2-byte big-endian length prefix.
encodeFrame :: ByteString -> ByteString
encodeFrame msg =
  let len = BS.length msg
   in word16BE (fromIntegral len) <> msg

-- | Decode a framed Noise message. Returns the message and remaining bytes.
decodeFrame :: ByteString -> Either String (ByteString, ByteString)
decodeFrame bs
  | BS.length bs < 2 = Left "decodeFrame: need at least 2 bytes for length"
  | otherwise =
      let len = fromIntegral (readWord16BE bs) :: Int
          rest = BS.drop 2 bs
       in if BS.length rest < len
            then Left $ "decodeFrame: expected " <> show len <> " bytes but got " <> show (BS.length rest)
            else Right (BS.take len rest, BS.drop len rest)
