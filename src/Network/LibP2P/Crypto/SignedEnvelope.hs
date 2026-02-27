-- | Signed Envelopes (RFC 0002) for domain-separated signed payloads.
--
-- A signed envelope wraps a payload with:
--   - Public key of the signer
--   - Domain string (prevents cross-protocol signature reuse)
--   - Payload type (multicodec)
--   - Payload bytes
--   - Signature over domain-separated content
--
-- Signing content: [varint(len(domain))][domain][payload_type][payload]
-- Wire format (protobuf):
--   field 1: public_key (bytes, protobuf-encoded PublicKey)
--   field 2: payload_type (bytes, multicodec)
--   field 3: payload (bytes)
--   field 5: signature (bytes)
module Network.LibP2P.Crypto.SignedEnvelope
  ( SignedEnvelope (..)
  , createEnvelope
  , verifyEnvelope
  , encodeSignedEnvelope
  , decodeSignedEnvelope
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word64)
import Network.LibP2P.Core.Varint (encodeUvarint)
import Network.LibP2P.Crypto.Key (PublicKey (..), PrivateKey, sign, verify)
import Network.LibP2P.Crypto.Protobuf (encodePublicKey, decodePublicKey)
import qualified Proto3.Wire.Decode as Decode
import Proto3.Wire.Decode (Parser, RawMessage, at, one, parse)
import qualified Proto3.Wire.Encode as Encode
import Proto3.Wire.Types (FieldNumber (..))

-- | A signed envelope containing a domain-separated signed payload.
data SignedEnvelope = SignedEnvelope
  { sePublicKey   :: !PublicKey     -- ^ Signer's public key
  , seDomain      :: !ByteString    -- ^ Domain separation string
  , sePayloadType :: !ByteString    -- ^ Payload type (multicodec bytes)
  , sePayload     :: !ByteString    -- ^ Payload bytes
  , seSignature   :: !ByteString    -- ^ Ed25519 signature
  } deriving (Show, Eq)

-- | Build the content that gets signed.
-- Format: [varint(len(domain))][domain][payload_type][payload]
buildSigningContent :: ByteString -> ByteString -> ByteString -> ByteString
buildSigningContent domain payloadType payload =
  let domainLen = encodeUvarint (fromIntegral (BS.length domain) :: Word64)
  in domainLen <> domain <> payloadType <> payload

-- | Create a signed envelope.
createEnvelope :: PrivateKey -> PublicKey -> ByteString -> ByteString -> ByteString -> Either String SignedEnvelope
createEnvelope privKey pubKey domain payloadType payload = do
  let content = buildSigningContent domain payloadType payload
  sig <- sign privKey content
  Right SignedEnvelope
    { sePublicKey   = pubKey
    , seDomain      = domain
    , sePayloadType = payloadType
    , sePayload     = payload
    , seSignature   = sig
    }

-- | Verify a signed envelope against an expected domain.
-- Reconstructs the signing content and verifies the signature.
verifyEnvelope :: SignedEnvelope -> ByteString -> Bool
verifyEnvelope env expectedDomain =
  let content = buildSigningContent expectedDomain (sePayloadType env) (sePayload env)
  in verify (sePublicKey env) content (seSignature env)

-- | Encode a signed envelope to protobuf wire format.
-- Note: domain is NOT included in the wire format (it's implicit from protocol context).
-- Fields: 1=public_key, 2=payload_type, 3=payload, 5=signature
encodeSignedEnvelope :: SignedEnvelope -> ByteString
encodeSignedEnvelope env = BL.toStrict $ Encode.toLazyByteString $
     Encode.byteString (FieldNumber 1) (encodePublicKey (sePublicKey env))
  <> Encode.byteString (FieldNumber 2) (sePayloadType env)
  <> Encode.byteString (FieldNumber 3) (sePayload env)
  <> Encode.byteString (FieldNumber 5) (seSignature env)

-- | Decode a signed envelope from protobuf wire format.
-- Note: domain is NOT in the wire format — caller must know it.
decodeSignedEnvelope :: ByteString -> Either String SignedEnvelope
decodeSignedEnvelope bs =
  case parse signedEnvelopeParser bs of
    Left err -> Left $ "SignedEnvelope decode error: " ++ show err
    Right (pkBytes, ptBytes, payload, sig) -> do
      pk <- decodePublicKey pkBytes
      Right SignedEnvelope
        { sePublicKey   = pk
        , seDomain      = BS.empty  -- domain not stored on wire
        , sePayloadType = ptBytes
        , sePayload     = payload
        , seSignature   = sig
        }

signedEnvelopeParser :: Parser RawMessage (ByteString, ByteString, ByteString, ByteString)
signedEnvelopeParser = (,,,)
  <$> at (one Decode.byteString BS.empty) (FieldNumber 1)  -- public_key (protobuf-encoded)
  <*> at (one Decode.byteString BS.empty) (FieldNumber 2)  -- payload_type
  <*> at (one Decode.byteString BS.empty) (FieldNumber 3)  -- payload
  <*> at (one Decode.byteString BS.empty) (FieldNumber 5)  -- signature
