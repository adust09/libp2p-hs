-- | Noise message framing: 2-byte big-endian length prefix.
--
-- All Noise messages (handshake and transport) are framed as:
-- [2 bytes BE length][noise_message]
--
-- Per the libp2p Noise spec, a Noise message has a maximum length of
-- 65535 bytes. Plaintext larger than one message allows must be split
-- across multiple Noise transport messages before encryption (see
-- 'chunkPlaintext'); 'encodeFrame' rejects oversized messages instead
-- of silently truncating the length prefix.
module LibP2P.Noise.Framing
  ( encodeFrame
  , decodeFrame
  , chunkPlaintext
  , maxNoiseMessageSize
  , maxNoisePlaintextSize
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import LibP2P.Core.Binary (readWord16BE, word16BE)

-- | Maximum Noise message size (limited by 2-byte length prefix).
maxNoiseMessageSize :: Int
maxNoiseMessageSize = 65535

-- | Maximum plaintext per Noise transport message: the 65535-byte Noise
-- message cap minus the 16-byte ChaChaPoly1305 authentication tag.
-- Matches the chunking threshold used by go-libp2p.
maxNoisePlaintextSize :: Int
maxNoisePlaintextSize = maxNoiseMessageSize - 16

-- | Split plaintext into chunks of at most 'maxNoisePlaintextSize' bytes,
-- so each chunk plus its AEAD tag fits in a single Noise message.
-- Empty input yields a single empty chunk so callers still emit one frame.
chunkPlaintext :: ByteString -> [ByteString]
chunkPlaintext bs
  | BS.length bs <= maxNoisePlaintextSize = [bs]
  | otherwise =
      let (chunk, rest) = BS.splitAt maxNoisePlaintextSize bs
       in chunk : chunkPlaintext rest

-- | Encode a Noise message with a 2-byte big-endian length prefix.
-- Rejects messages larger than 'maxNoiseMessageSize' — the 2-byte prefix
-- cannot represent them, and truncating the length would corrupt every
-- subsequent frame boundary on the connection.
encodeFrame :: ByteString -> Either String ByteString
encodeFrame msg
  | len > maxNoiseMessageSize =
      Left $
        "encodeFrame: message length " <> show len
          <> " exceeds maximum Noise message size " <> show maxNoiseMessageSize
  | otherwise = Right $ word16BE (fromIntegral len) <> msg
  where
    len = BS.length msg

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
