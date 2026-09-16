-- | libp2p identity certificates for QUIC's mutual TLS handshake.
module LibP2P.Transport.QUIC.Certificate
  ( newQUICCredential
  , verifyQUICCertificate
  , libp2pExtensionOID
  ) where

import qualified Crypto.Error as CE
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Crypto.Random (getRandomBytes)
import Data.ASN1.BinaryEncoding (DER (..))
import Data.ASN1.Encoding (decodeASN1', encodeASN1')
import Data.ASN1.Types (ASN1 (..), ASN1ConstructionType (..), OID, toASN1)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.Hourglass (Date (..), DateTime (..), Month (..), TimeOfDay (..))
import Data.X509
  ( Certificate (..)
  , CertificateChain (..)
  , DistinguishedName (..)
  , ExtensionRaw (..)
  , Extensions (..)
  , PrivKey (..)
  , PubKey (..)
  , PubKeyALG (..)
  , SignatureALG (..)
  , SignedCertificate
  , getSigned
  , objectToSignedExact
  , signedObject
  )
import Data.X509.Validation
  ( SignatureVerification (..)
  , verifySignedSignature
  )
import LibP2P.Crypto.Key (KeyPair (..), sign, verify)
import LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import qualified LibP2P.Crypto.Protobuf as Protobuf
import Network.TLS (Credential)

-- | IANA private enterprise extension assigned to libp2p.
libp2pExtensionOID :: OID
libp2pExtensionOID = [1, 3, 6, 1, 4, 1, 53594, 1, 1]

identitySignaturePrefix :: ByteString
identitySignaturePrefix = "libp2p-tls-handshake:"

certificateSignatureAlgorithm :: SignatureALG
certificateSignatureAlgorithm = SignatureALG_IntrinsicHash PubKeyALG_Ed25519

-- | Generate an ephemeral self-signed Ed25519 certificate bound to a host key.
newQUICCredential :: KeyPair -> IO Credential
newQUICCredential identity = do
  seed <- getRandomBytes 32 :: IO ByteString
  tlsSecret <- case CE.eitherCryptoError (Ed25519.secretKey seed) of
    Left err -> fail $ "newQUICCredential: " <> show err
    Right key -> pure key
  let tlsPublic = Ed25519.toPublic tlsSecret
      certificatePublic = PubKeyEd25519 tlsPublic
      spki = encodeASN1' DER (toASN1 certificatePublic [])
  identitySignature <- either fail pure $
    sign (kpPrivate identity) (identitySignaturePrefix <> spki)
  let extension = ExtensionRaw
        { extRawOID = libp2pExtensionOID
        , extRawCritical = True
        , extRawContent = encodeSignedKey (Protobuf.encodePublicKey (kpPublic identity)) identitySignature
        }
      certificate = makeCertificate certificatePublic extension
      signCertificate bytes =
        ( convert (Ed25519.sign tlsSecret tlsPublic bytes)
        , certificateSignatureAlgorithm
        , ()
        )
      (signedCertificate, ()) = objectToSignedExact signCertificate certificate
  pure (CertificateChain [signedCertificate], PrivKeyEd25519 tlsSecret)

-- | Verify a peer's one-certificate chain and recover its authenticated Peer ID.
verifyQUICCertificate :: CertificateChain -> IO (Either String PeerId)
verifyQUICCertificate (CertificateChain [signedCertificate]) =
  pure $ verifyCertificate signedCertificate
verifyQUICCertificate _ =
  pure $ Left "QUIC certificate chain must contain exactly one certificate"

verifyCertificate :: SignedCertificate -> Either String PeerId
verifyCertificate signedCertificate = do
  let certificate = signedObject (getSigned signedCertificate)
  require (certIssuerDN certificate == certSubjectDN certificate)
    "QUIC certificate is not self-signed"
  case verifySignedSignature signedCertificate (certPubKey certificate) of
    SignaturePass -> pure ()
    SignatureFailed _ -> Left "QUIC certificate self-signature verification failed"
  -- The TLS spec permits this extension to be critical or non-critical.
  extension <- findIdentityExtension (certExtensions certificate)
  (encodedHostKey, identitySignature) <- decodeSignedKey (extRawContent extension)
  hostKey <- Protobuf.decodePublicKey encodedHostKey
  let spki = encodeASN1' DER (toASN1 (certPubKey certificate) [])
  require (verify hostKey (identitySignaturePrefix <> spki) identitySignature)
    "QUIC certificate host-key signature verification failed"
  pure (fromPublicKey hostKey)

findIdentityExtension :: Extensions -> Either String ExtensionRaw
findIdentityExtension (Extensions maybeExtensions) = do
  let extensions = maybe [] id maybeExtensions
      unknownCritical = filter isUnknownCritical extensions
      identities = filter ((== libp2pExtensionOID) . extRawOID) extensions
  require (null unknownCritical) "QUIC certificate contains an unknown critical extension"
  case identities of
    [extension] -> Right extension
    [] -> Left "QUIC certificate is missing the libp2p identity extension"
    _ -> Left "QUIC certificate contains multiple libp2p identity extensions"

isUnknownCritical :: ExtensionRaw -> Bool
isUnknownCritical extension =
  extRawCritical extension && extRawOID extension `notElem` recognizedCriticalOIDs

recognizedCriticalOIDs :: [OID]
recognizedCriticalOIDs =
  [ libp2pExtensionOID
  , [2, 5, 29, 14] -- subject key identifier
  , [2, 5, 29, 15] -- key usage
  , [2, 5, 29, 17] -- subject alternative name
  , [2, 5, 29, 19] -- basic constraints
  , [2, 5, 29, 35] -- authority key identifier
  , [2, 5, 29, 37] -- extended key usage
  ]

encodeSignedKey :: ByteString -> ByteString -> ByteString
encodeSignedKey publicKey signature =
  encodeASN1' DER
    [ Start Sequence
    , OctetString publicKey
    , OctetString signature
    , End Sequence
    ]

decodeSignedKey :: ByteString -> Either String (ByteString, ByteString)
decodeSignedKey bytes = case decodeASN1' DER bytes of
  Left err -> Left $ "invalid libp2p certificate extension: " <> show err
  Right [Start Sequence, OctetString publicKey, OctetString signature, End Sequence] ->
    Right (publicKey, signature)
  Right _ -> Left "invalid libp2p certificate extension structure"

makeCertificate :: PubKey -> ExtensionRaw -> Certificate
makeCertificate publicKey extension = Certificate
  { certVersion = 2
  , certSerial = 1
  , certSignatureAlg = certificateSignatureAlgorithm
  , certIssuerDN = DistinguishedName []
  , certValidity = (notBefore, notAfter)
  , certSubjectDN = DistinguishedName []
  , certPubKey = publicKey
  , certExtensions = Extensions (Just [extension])
  }
  where
    notBefore = DateTime (Date 1970 January 1) (TimeOfDay 0 0 0 0)
    notAfter = DateTime (Date 4096 January 1) (TimeOfDay 0 0 0 0)

require :: Bool -> String -> Either String ()
require True _ = Right ()
require False message = Left message
