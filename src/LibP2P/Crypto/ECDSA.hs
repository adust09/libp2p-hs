-- | ECDSA (NIST P-256) key operations for libp2p peer identity, using crypton.
--
-- Wire formats follow the libp2p peer-ids spec (matching go-libp2p):
-- - Public key: DER-encoded SubjectPublicKeyInfo (PKIX), uncompressed point.
-- - Private key: DER-encoded RFC 5915 ECPrivateKey (SEC1), with the named
--   curve parameters and the public key included, as emitted by Go's
--   x509.MarshalECPrivateKey.
-- - Signatures: ECDSA over SHA-256, DER-encoded (SEQUENCE { r, s }).
--
-- Operates on raw 'ByteString' so this module has no dependency on
-- "LibP2P.Crypto.Key".
module LibP2P.Crypto.ECDSA
  ( generate
  , signIO
  , verify
  , derivePublicKey
  ) where

import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import Crypto.PubKey.ECC.Generate (generateQ)
import Crypto.PubKey.ECC.Types
  ( Curve
  , CurveCommon (ecc_n)
  , CurveName (SEC_p256r1)
  , Point (..)
  , common_curve
  , getCurveByName
  )
import Crypto.Random (getRandomBytes)
import Data.ASN1.BinaryEncoding (DER (..))
import Data.ASN1.BitArray (toBitArray)
import Data.ASN1.Encoding (decodeASN1', encodeASN1')
import Data.ASN1.Types (ASN1 (..), ASN1Class (..), ASN1ConstructionType (..), fromASN1, toASN1)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.X509 (PubKey (PubKeyEC), PubKeyEC (PubKeyEC_Named), SerializedPoint (..))
import Data.X509.EC (unserializePoint)

-- | The NIST P-256 curve (a.k.a. prime256v1 / secp256r1).
curve :: Curve
curve = getCurveByName SEC_p256r1

-- | Curve order (n).
curveOrder :: Integer
curveOrder = ecc_n (common_curve curve)

-- | ASN.1 OID for prime256v1 / secp256r1.
p256Oid :: [Integer]
p256Oid = [1, 2, 840, 10045, 3, 1, 7]

-- | Generate a new P-256 key pair, returning (public SPKI DER, private SEC1 DER).
generate :: IO (ByteString, ByteString)
generate = do
  d <- randomScalar
  let q = generateQ curve d
  pure (encodePublicKey q, encodePrivateKey d q)

-- | Draw a private scalar in [1, n-1] via rejection sampling.
randomScalar :: IO Integer
randomScalar = do
  bytes <- getRandomBytes 32 :: IO ByteString
  let d = os2ip bytes
  if d >= 1 && d < curveOrder then pure d else randomScalar

-- | Sign a message with a SEC1-DER private key (ECDSA/SHA-256, DER output).
-- Runs in IO because ECDSA signing requires a random nonce.
signIO :: ByteString -> ByteString -> IO (Either String ByteString)
signIO privDer msg =
  case decodePrivateKey privDer of
    Left err -> pure (Left err)
    Right d -> do
      let priv = ECDSA.PrivateKey curve d
      sig <- ECDSA.sign priv SHA256 msg
      pure (Right (encodeSignature sig))

-- | Derive the SPKI-DER public key from a SEC1-DER private key.
derivePublicKey :: ByteString -> Either String ByteString
derivePublicKey privDer = encodePublicKey . generateQ curve <$> decodePrivateKey privDer

-- | Verify a DER signature against an SPKI-DER public key (ECDSA/SHA-256).
verify :: ByteString -> ByteString -> ByteString -> Bool
verify pubDer msg sigDer =
  case (decodePublicKey pubDer, decodeSignature sigDer) of
    (Right pt, Right sig) -> ECDSA.verify SHA256 (ECDSA.PublicKey curve pt) sig msg
    _ -> False

-- | Encode a curve point as DER SubjectPublicKeyInfo (uncompressed point).
encodePublicKey :: Point -> ByteString
encodePublicKey PointO = BS.empty
encodePublicKey q =
  let pub = PubKeyEC (PubKeyEC_Named SEC_p256r1 (SerializedPoint (uncompressedPoint q)))
   in encodeASN1' DER (toASN1 pub [])

-- | SEC1 uncompressed point encoding (0x04 || X || Y).
uncompressedPoint :: Point -> ByteString
uncompressedPoint PointO = BS.empty
uncompressedPoint (Point x y) = BS.cons 0x04 (i2ospOf_ 32 x <> i2ospOf_ 32 y)

-- | Decode a DER SubjectPublicKeyInfo into a curve point.
decodePublicKey :: ByteString -> Either String Point
decodePublicKey bs =
  case decodeASN1' DER bs of
    Left err -> Left $ "ECDSA.decodePublicKey: " <> show err
    Right asn1 -> case fromASN1 asn1 of
      Right (PubKeyEC (PubKeyEC_Named name sp), _) ->
        case unserializePoint (getCurveByName name) sp of
          Just pt -> Right pt
          Nothing -> Left "ECDSA.decodePublicKey: invalid EC point"
      Right _ -> Left "ECDSA.decodePublicKey: not a named EC public key"
      Left err -> Left $ "ECDSA.decodePublicKey: " <> err

-- | Encode a private scalar as an RFC 5915 ECPrivateKey DER, including the
-- named-curve parameters and the public key (Go's x509.MarshalECPrivateKey
-- layout, which the peer-ids spec test vector uses).
encodePrivateKey :: Integer -> Point -> ByteString
encodePrivateKey d q =
  encodeASN1'
    DER
    [ Start Sequence
    , IntVal 1 -- ecPrivkeyVer1
    , OctetString (i2ospOf_ 32 d)
    , Start (Container Context 0)
    , OID p256Oid
    , End (Container Context 0)
    , Start (Container Context 1)
    , BitString (toBitArray (uncompressedPoint q) 0)
    , End (Container Context 1)
    , End Sequence
    ]

-- | Decode an RFC 5915 ECPrivateKey DER into the private scalar.
-- The curve parameters and public key fields are optional; when the curve
-- is present it must be P-256.
decodePrivateKey :: ByteString -> Either String Integer
decodePrivateKey bs =
  case decodeASN1' DER bs of
    Left err -> Left $ "ECDSA.decodePrivateKey: " <> show err
    Right (Start Sequence : IntVal 1 : OctetString priv : rest)
      | BS.length priv /= 32 ->
          Left "ECDSA.decodePrivateKey: expected a 32-byte P-256 scalar"
      | otherwise -> os2ip priv <$ checkCurveOid rest
    Right _ -> Left "ECDSA.decodePrivateKey: not an RFC 5915 ECPrivateKey"
  where
    checkCurveOid (Start (Container Context 0) : OID oid : End (Container Context 0) : _)
      | oid == p256Oid = Right ()
      | otherwise = Left "ECDSA.decodePrivateKey: unsupported curve (expected P-256)"
    checkCurveOid _ = Right ()

-- | Encode an ECDSA signature as DER SEQUENCE { r, s }.
encodeSignature :: ECDSA.Signature -> ByteString
encodeSignature (ECDSA.Signature r s) =
  encodeASN1' DER [Start Sequence, IntVal r, IntVal s, End Sequence]

-- | Decode a DER SEQUENCE { r, s } into an ECDSA signature.
decodeSignature :: ByteString -> Either String ECDSA.Signature
decodeSignature bs =
  case decodeASN1' DER bs of
    Left err -> Left $ "ECDSA.decodeSignature: " <> show err
    Right (Start Sequence : IntVal r : IntVal s : End Sequence : _) ->
      Right (ECDSA.Signature r s)
    Right _ -> Left "ECDSA.decodeSignature: unexpected ASN.1 structure"
