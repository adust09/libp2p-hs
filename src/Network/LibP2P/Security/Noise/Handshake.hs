-- | Noise XX handshake for libp2p secure channels.
--
-- Implements the Noise_XX_25519_ChaChaPoly_SHA256 handshake pattern
-- with libp2p-specific payload injection (identity key + signature).
module Network.LibP2P.Security.Noise.Handshake
  ( HandshakeResult (..)
  , NoisePayload (..)
  , encodeNoisePayload
  , decodeNoisePayload
  , signStaticKey
  , verifyStaticKey
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Network.LibP2P.Core.Varint (decodeUvarint, encodeUvarint)
import Network.LibP2P.Crypto.Key
import Network.LibP2P.Crypto.PeerId (PeerId)

-- | Result of a successful Noise handshake.
data HandshakeResult = HandshakeResult
  { hrRemotePeerId :: !PeerId
  , hrRemotePublicKey :: !PublicKey
  }
  deriving (Show, Eq)

-- | Noise handshake payload (protobuf-encoded in messages 2 and 3).
data NoisePayload = NoisePayload
  { npIdentityKey :: !ByteString -- ^ Serialized PublicKey protobuf
  , npIdentitySig :: !ByteString -- ^ Signature over "noise-libp2p-static-key:" || static_pubkey
  }
  deriving (Show, Eq)

-- | The prefix for the signed data in Noise handshake.
noiseStaticKeyPrefix :: ByteString
noiseStaticKeyPrefix = "noise-libp2p-static-key:"

-- | Sign the Noise static public key with the identity private key.
-- Produces: sign(identity_sk, "noise-libp2p-static-key:" || noise_static_pubkey)
signStaticKey :: PrivateKey -> ByteString -> Either String ByteString
signStaticKey sk noiseStaticPubKey =
  let payload = noiseStaticKeyPrefix <> noiseStaticPubKey
   in sign sk payload

-- | Verify a signature over the Noise static public key.
verifyStaticKey :: PublicKey -> ByteString -> ByteString -> Bool
verifyStaticKey pk noiseStaticPubKey sig =
  let payload = noiseStaticKeyPrefix <> noiseStaticPubKey
   in verify pk payload sig

-- | Encode a NoisePayload as a minimal protobuf message.
--
-- NoiseHandshakePayload {
--   optional bytes identity_key = 1;
--   optional bytes identity_sig = 2;
-- }
encodeNoisePayload :: NoisePayload -> ByteString
encodeNoisePayload (NoisePayload identKey identSig) =
  -- Field 1: tag 0x0a (field 1, wire type 2 = length-delimited)
  BS.singleton 0x0a
    <> encodeUvarint (fromIntegral (BS.length identKey))
    <> identKey
    -- Field 2: tag 0x12 (field 2, wire type 2 = length-delimited)
    <> BS.singleton 0x12
    <> encodeUvarint (fromIntegral (BS.length identSig))
    <> identSig

-- | Decode a NoisePayload from protobuf bytes.
decodeNoisePayload :: ByteString -> Either String NoisePayload
decodeNoisePayload bs = do
  (identKey, rest1) <- decodeField 0x0a bs
  (identSig, _rest2) <- decodeField 0x12 rest1
  Right (NoisePayload identKey identSig)
  where
    decodeField :: Word8 -> ByteString -> Either String (ByteString, ByteString)
    decodeField expectedTag input
      | BS.null input = Left "decodeNoisePayload: unexpected end of input"
      | BS.head input /= expectedTag =
          Left $ "decodeNoisePayload: expected tag " <> show expectedTag <> " got " <> show (BS.head input)
      | otherwise = do
          let rest = BS.tail input
          (len, rest2) <- decodeUvarint rest
          let fieldLen = fromIntegral len :: Int
          if BS.length rest2 < fieldLen
            then Left "decodeNoisePayload: not enough bytes for field"
            else Right (BS.take fieldLen rest2, BS.drop fieldLen rest2)
