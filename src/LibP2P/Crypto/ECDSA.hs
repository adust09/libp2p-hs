-- | ECDSA (NIST P-256) key operations for libp2p peer identity, using crypton.
--
-- Wire formats follow the libp2p peer-ids spec (matching go-libp2p):
-- - Public key: DER-encoded SubjectPublicKeyInfo (PKIX), uncompressed point.
-- - Private key: 32-byte big-endian scalar.
-- - Signatures: ECDSA over SHA-256, DER-encoded (SEQUENCE { r, s }).
--
-- Operates on raw 'ByteString' so this module has no dependency on
-- "LibP2P.Crypto.Key".
module LibP2P.Crypto.ECDSA
  ( generate
  , signIO
  , verify
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
import Data.ASN1.Encoding (decodeASN1', encodeASN1')
import Data.ASN1.Types (ASN1 (..), ASN1ConstructionType (..), fromASN1, toASN1)
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

-- | Generate a new P-256 key pair, returning (public SPKI DER, 32-byte private).
generate :: IO (ByteString, ByteString)
generate = do
  d <- randomScalar
  let q = generateQ curve d
  pure (encodePublicKey q, i2ospOf_ 32 d)

-- | Draw a private scalar in [1, n-1] via rejection sampling.
randomScalar :: IO Integer
randomScalar = do
  bytes <- getRandomBytes 32 :: IO ByteString
  let d = os2ip bytes
  if d >= 1 && d < curveOrder then pure d else randomScalar

-- | Sign a message with a 32-byte private scalar (ECDSA/SHA-256, DER output).
-- Runs in IO because ECDSA signing requires a random nonce.
signIO :: ByteString -> ByteString -> IO (Either String ByteString)
signIO privRaw msg
  | BS.length privRaw /= 32 = pure (Left "ECDSA.signIO: private key must be 32 bytes")
  | otherwise = do
      let priv = ECDSA.PrivateKey curve (os2ip privRaw)
      sig <- ECDSA.sign priv SHA256 msg
      pure (Right (encodeSignature sig))

-- | Verify a DER signature against an SPKI-DER public key (ECDSA/SHA-256).
verify :: ByteString -> ByteString -> ByteString -> Bool
verify pubDer msg sigDer =
  case (decodePublicKey pubDer, decodeSignature sigDer) of
    (Right pt, Right sig) -> ECDSA.verify SHA256 (ECDSA.PublicKey curve pt) sig msg
    _ -> False

-- | Encode a curve point as DER SubjectPublicKeyInfo (uncompressed point).
encodePublicKey :: Point -> ByteString
encodePublicKey PointO = BS.empty
encodePublicKey (Point x y) =
  let uncompressed = BS.cons 0x04 (i2ospOf_ 32 x <> i2ospOf_ 32 y)
      pub = PubKeyEC (PubKeyEC_Named SEC_p256r1 (SerializedPoint uncompressed))
   in encodeASN1' DER (toASN1 pub [])

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
