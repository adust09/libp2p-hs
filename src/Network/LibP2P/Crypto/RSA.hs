-- | RSA key operations for libp2p peer identity, using crypton.
--
-- Wire formats follow the libp2p peer-ids spec:
-- - Public key: DER-encoded SubjectPublicKeyInfo (PKIX).
-- - Private key: DER-encoded PKCS#1 RSAPrivateKey.
-- - Signatures: RSASSA-PKCS1-v1_5 over SHA-256.
--
-- All functions operate on raw 'ByteString' so this module does not depend on
-- "Network.LibP2P.Crypto.Key" (avoids an import cycle with the dispatcher).
module Network.LibP2P.Crypto.RSA
  ( generate
  , sign
  , verify
  ) where

import Crypto.Hash.Algorithms (SHA256 (..))
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as PKCS15
import Data.ASN1.BinaryEncoding (DER (..))
import Data.ASN1.Encoding (decodeASN1', encodeASN1')
import Data.ASN1.Types (ASN1 (..), ASN1ConstructionType (..), fromASN1, toASN1)
import Data.ByteString (ByteString)
import Data.X509 (PubKey (PubKeyRSA))

-- | Public exponent used for generated keys (65537).
publicExponent :: Integer
publicExponent = 0x10001

-- | Default modulus size in bytes (2048-bit).
keySizeBytes :: Int
keySizeBytes = 256

-- | Generate a new RSA key pair, returning (public SPKI DER, private PKCS#1 DER).
generate :: IO (ByteString, ByteString)
generate = do
  (pub, priv) <- RSA.generate keySizeBytes publicExponent
  pure (encodePublicKey pub, encodePrivateKey priv)

-- | Sign a message with a PKCS#1-DER private key (RSASSA-PKCS1-v1_5, SHA-256).
-- Deterministic (no blinder), so it is pure.
sign :: ByteString -> ByteString -> Either String ByteString
sign privDer msg = do
  priv <- decodePrivateKey privDer
  case PKCS15.sign Nothing (Just SHA256) priv msg of
    Left err -> Left $ "RSA.sign: " <> show err
    Right sig -> Right sig

-- | Verify a signature against an SPKI-DER public key (RSASSA-PKCS1-v1_5, SHA-256).
verify :: ByteString -> ByteString -> ByteString -> Bool
verify pubDer msg sig =
  case decodePublicKey pubDer of
    Left _ -> False
    Right pub -> PKCS15.verify (Just SHA256) pub msg sig

-- | Encode an RSA public key as DER SubjectPublicKeyInfo.
encodePublicKey :: RSA.PublicKey -> ByteString
encodePublicKey pub = encodeASN1' DER (toASN1 (PubKeyRSA pub) [])

-- | Decode a DER SubjectPublicKeyInfo into an RSA public key.
decodePublicKey :: ByteString -> Either String RSA.PublicKey
decodePublicKey bs =
  case decodeASN1' DER bs of
    Left err -> Left $ "RSA.decodePublicKey: " <> show err
    Right asn1 -> case fromASN1 asn1 of
      Right (PubKeyRSA pub, _) -> Right pub
      Right _ -> Left "RSA.decodePublicKey: not an RSA public key"
      Left err -> Left $ "RSA.decodePublicKey: " <> err

-- | Encode an RSA private key as DER PKCS#1 RSAPrivateKey.
encodePrivateKey :: RSA.PrivateKey -> ByteString
encodePrivateKey priv =
  encodeASN1' DER $
    [ Start Sequence
    , IntVal 0 -- version (two-prime)
    , IntVal (RSA.public_n (RSA.private_pub priv))
    , IntVal (RSA.public_e (RSA.private_pub priv))
    , IntVal (RSA.private_d priv)
    , IntVal (RSA.private_p priv)
    , IntVal (RSA.private_q priv)
    , IntVal (RSA.private_dP priv)
    , IntVal (RSA.private_dQ priv)
    , IntVal (RSA.private_qinv priv)
    , End Sequence
    ]

-- | Decode a DER PKCS#1 RSAPrivateKey into an RSA private key.
decodePrivateKey :: ByteString -> Either String RSA.PrivateKey
decodePrivateKey bs =
  case decodeASN1' DER bs of
    Left err -> Left $ "RSA.decodePrivateKey: " <> show err
    Right
      ( Start Sequence
          : IntVal _ver
          : IntVal n
          : IntVal e
          : IntVal d
          : IntVal p
          : IntVal q
          : IntVal dP
          : IntVal dQ
          : IntVal qinv
          : End Sequence
          : _
        ) ->
        Right
          RSA.PrivateKey
            { RSA.private_pub =
                RSA.PublicKey {RSA.public_size = keySizeBytes, RSA.public_n = n, RSA.public_e = e}
            , RSA.private_d = d
            , RSA.private_p = p
            , RSA.private_q = q
            , RSA.private_dP = dP
            , RSA.private_dQ = dQ
            , RSA.private_qinv = qinv
            }
    Right _ -> Left "RSA.decodePrivateKey: unexpected ASN.1 structure"
