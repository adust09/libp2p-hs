-- | Secp256k1 key operations for libp2p peer identity, using crypton.
--
-- Wire formats follow the libp2p peer-ids spec:
-- - Public key: 33-byte SEC1 compressed point (0x02/0x03 prefix + 32-byte X).
-- - Private key: 32-byte big-endian scalar.
-- - Signatures: ECDSA over SHA-256, DER-encoded (SEQUENCE { r, s }).
--
-- Operates on raw 'ByteString' so this module has no dependency on
-- "LibP2P.Crypto.Key".
module LibP2P.Crypto.Secp256k1
  ( generate
  , signIO
  , verify
  ) where

import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Number.ModArithmetic (expFast)
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import Crypto.PubKey.ECC.Generate (generateQ)
import Crypto.PubKey.ECC.Types
  ( Curve (..)
  , CurveCommon (..)
  , CurveName (SEC_p256k1)
  , CurvePrime (..)
  , Point (..)
  , getCurveByName
  )
import Crypto.Random (getRandomBytes)
import Data.ASN1.BinaryEncoding (DER (..))
import Data.ASN1.Encoding (decodeASN1', encodeASN1')
import Data.ASN1.Types (ASN1 (..), ASN1ConstructionType (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | The secp256k1 curve.
curve :: Curve
curve = getCurveByName SEC_p256k1

-- | Field prime and curve coefficients (a, b) for secp256k1.
curveParams :: (Integer, Integer, Integer)
curveParams = case curve of
  CurveFP (CurvePrime p cc) -> (p, ecc_a cc, ecc_b cc)
  _ -> error "Secp256k1: expected a prime-field curve"

-- | Curve order (n).
curveOrder :: Integer
curveOrder = case curve of
  CurveFP (CurvePrime _ cc) -> ecc_n cc
  _ -> error "Secp256k1: expected a prime-field curve"

-- | Generate a new secp256k1 key pair, returning (compressed public, 32-byte private).
generate :: IO (ByteString, ByteString)
generate = do
  d <- randomScalar
  let q = generateQ curve d
  pure (encodePoint q, i2ospOf_ 32 d)

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
  | BS.length privRaw /= 32 = pure (Left "Secp256k1.signIO: private key must be 32 bytes")
  | otherwise = do
      let priv = ECDSA.PrivateKey curve (os2ip privRaw)
      sig <- ECDSA.sign priv SHA256 msg
      pure (Right (encodeSignature sig))

-- | Verify a DER signature against a 33-byte compressed public key (ECDSA/SHA-256).
verify :: ByteString -> ByteString -> ByteString -> Bool
verify pubRaw msg sigDer =
  case (decodePoint pubRaw, decodeSignature sigDer) of
    (Right pt, Right sig) -> ECDSA.verify SHA256 (ECDSA.PublicKey curve pt) sig msg
    _ -> False

-- | Encode a curve point as a 33-byte SEC1 compressed public key.
encodePoint :: Point -> ByteString
encodePoint PointO = BS.replicate 33 0
encodePoint (Point x y) =
  let prefix = if even y then 0x02 else 0x03
   in BS.cons prefix (i2ospOf_ 32 x)

-- | Decode a 33-byte SEC1 compressed public key into a curve point.
decodePoint :: ByteString -> Either String Point
decodePoint bs
  | BS.length bs /= 33 = Left "Secp256k1.decodePoint: expected 33 bytes"
  | prefix /= 0x02 && prefix /= 0x03 = Left "Secp256k1.decodePoint: bad prefix"
  | otherwise =
      let (p, a, b) = curveParams
          x = os2ip (BS.drop 1 bs)
          rhs = (x * x * x + a * x + b) `mod` p
          y0 = expFast rhs ((p + 1) `div` 4) p
          wantOdd = prefix == 0x03
          y = if (odd y0) == wantOdd then y0 else p - y0
       in Right (Point x y)
  where
    prefix = BS.head bs

-- | Encode an ECDSA signature as DER SEQUENCE { r, s }.
encodeSignature :: ECDSA.Signature -> ByteString
encodeSignature (ECDSA.Signature r s) =
  encodeASN1' DER [Start Sequence, IntVal r, IntVal s, End Sequence]

-- | Decode a DER SEQUENCE { r, s } into an ECDSA signature.
decodeSignature :: ByteString -> Either String ECDSA.Signature
decodeSignature bs =
  case decodeASN1' DER bs of
    Left err -> Left $ "Secp256k1.decodeSignature: " <> show err
    Right (Start Sequence : IntVal r : IntVal s : End Sequence : _) ->
      Right (ECDSA.Signature r s)
    Right _ -> Left "Secp256k1.decodeSignature: unexpected ASN.1 structure"
