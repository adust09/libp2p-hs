-- | Deterministic protobuf encoding for libp2p PublicKey/PrivateKey messages.
--
-- Hand-rolled encoding to guarantee deterministic output:
-- - Minimal varint encoding
-- - Fields in field number order (Type=1, Data=2)
-- - All fields present, no extras
module LibP2P.Crypto.Protobuf
  ( encodePublicKey
  , decodePublicKey
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word64, Word8)
import Numeric (showHex)
import LibP2P.Core.Varint (decodeUvarint, encodeUvarint)
import LibP2P.Crypto.Key (KeyType (..), PublicKey (..))

-- | Protobuf KeyType enum values (per libp2p peer-ids spec: RSA=0, Ed25519=1,
-- Secp256k1=2, ECDSA=3).
keyTypeToProto :: KeyType -> Word64
keyTypeToProto RSA = 0
keyTypeToProto Ed25519 = 1
keyTypeToProto Secp256k1 = 2
keyTypeToProto ECDSA = 3

keyTypeFromProto :: Word64 -> Either String KeyType
keyTypeFromProto 0 = Right RSA
keyTypeFromProto 1 = Right Ed25519
keyTypeFromProto 2 = Right Secp256k1
keyTypeFromProto 3 = Right ECDSA
keyTypeFromProto n = Left $ "unknown KeyType: " <> show n

-- | Deterministic protobuf encoding of a PublicKey message.
--
-- Layout:
--   Field 1 (Type): tag=0x08 (field 1, wire type 0=varint), value=keytype
--   Field 2 (Data): tag=0x12 (field 2, wire type 2=length-delimited), length, bytes
encodePublicKey :: PublicKey -> ByteString
encodePublicKey (PublicKey kt rawKey) =
  -- Field 1: tag 0x08 + varint value
  BS.singleton 0x08 <> encodeUvarint (keyTypeToProto kt)
    -- Field 2: tag 0x12 + varint length + raw bytes
    <> BS.singleton 0x12
    <> encodeUvarint (fromIntegral (BS.length rawKey))
    <> rawKey

-- | Decode a protobuf-encoded PublicKey message.
decodePublicKey :: ByteString -> Either String PublicKey
decodePublicKey bs = do
  -- Field 1: expect tag 0x08
  (tag1, rest1) <- takeExpectedByte 0x08 bs "expected tag 0x08 for field 1"
  _ <- pure tag1
  (typeVal, rest2) <- decodeUvarint rest1
  kt <- keyTypeFromProto typeVal
  -- Field 2: expect tag 0x12
  (_, rest3) <- takeExpectedByte 0x12 rest2 "expected tag 0x12 for field 2"
  (dataLen, rest4) <- decodeUvarint rest3
  let len = fromIntegral dataLen :: Int
  if BS.length rest4 < len
    then Left "decodePublicKey: not enough bytes for key data"
    else
      let keyData = BS.take len rest4
       in Right (PublicKey kt keyData)
  where
    takeExpectedByte :: Word8 -> ByteString -> String -> Either String (Word8, ByteString)
    takeExpectedByte expected input msg
      | BS.null input = Left $ "decodePublicKey: " <> msg <> " (empty input)"
      | BS.head input /= expected =
          Left $ "decodePublicKey: " <> msg <> " (got 0x" <> showHex (BS.head input) ")"
      | otherwise = Right (expected, BS.tail input)
