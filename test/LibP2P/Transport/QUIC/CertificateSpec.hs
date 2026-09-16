module LibP2P.Transport.QUIC.CertificateSpec (spec) where

import qualified LibP2P.Crypto.Ed25519 as Ed25519
import LibP2P.Crypto.Key (KeyPair (..))
import LibP2P.Crypto.PeerId (fromPublicKey)
import LibP2P.Transport.QUIC.Certificate
  ( libp2pExtensionOID
  , newQUICCredential
  , verifyQUICCertificate
  )
import Network.TLS (Credential)
import Data.X509
  ( Certificate (..)
  , CertificateChain (..)
  , ExtensionRaw (..)
  , Extensions (..)
  , getSigned
  , signedObject
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "libp2p QUIC certificates" $ do
    it "authenticates the host Peer ID carried by a generated certificate" $ do
      identity <- generateIdentity
      let expectedPeerId = fromPublicKey (kpPublic identity)
      credential <- newQUICCredential identity
      verifyQUICCertificate (credentialChain credential)
        `shouldReturn` Right expectedPeerId

    it "marks the libp2p identity extension critical" $ do
      identity <- generateIdentity
      credential <- newQUICCredential identity
      extensions <- extractExtensions credential
      let identityExtensions =
            filter ((== libp2pExtensionOID) . extRawOID) extensions
      map extRawCritical identityExtensions `shouldBe` [True]

    it "rejects an empty certificate chain" $
      verifyQUICCertificate (CertificateChain [])
        `shouldReturn` Left "QUIC certificate chain must contain exactly one certificate"

    it "rejects certificate chains containing intermediates" $ do
      identity <- generateIdentity
      credential <- newQUICCredential identity
      let CertificateChain certificates = credentialChain credential
      verifyQUICCertificate (CertificateChain (certificates <> certificates))
        `shouldReturn` Left "QUIC certificate chain must contain exactly one certificate"

generateIdentity :: IO KeyPair
generateIdentity = do
  generated <- Ed25519.generateKeyPair
  either fail pure generated

credentialChain :: Credential -> CertificateChain
credentialChain = fst

extractExtensions :: Credential -> IO [ExtensionRaw]
extractExtensions credential = case credentialChain credential of
  CertificateChain [signedCertificate] ->
    let certificate = signedObject (getSigned signedCertificate)
    in case certExtensions certificate of
      Extensions (Just extensions) -> pure extensions
      Extensions Nothing -> fail "generated certificate has no extensions"
  _ -> fail "generated credential has an invalid certificate chain"
