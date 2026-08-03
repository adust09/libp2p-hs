-- | Cross-implementation private key import tests.
--
-- All fixtures are the official test vectors from the libp2p peer-ids spec
-- (specs/peer-ids/peer-ids.md, "Test vectors"): hex-encoded protobuf
-- PrivateKey/PublicKey messages produced by go-libp2p. The spec requires
-- that implementations can produce the provided public key from the
-- private key.
module LibP2P.Crypto.KeyImportSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified LibP2P.Crypto.ECDSA as ECDSA
import LibP2P.Crypto.Key
import LibP2P.Crypto.Protobuf
import qualified LibP2P.Crypto.Secp256k1 as Secp256k1
import LibP2P.Crypto.SpecVectors
import Test.Hspec

spec :: Spec
spec = describe "private key import (peer-ids spec test vectors)" $ do
  describe "Ed25519" $ do
    it "derives the spec public key from the spec private key" $
      withImported ed25519PrivHex $ \kp ->
        encodePublicKey (kpPublic kp) `shouldBe` unhex ed25519PubHex

    it "normalizes the private key to the 64-byte (seed || public key) form" $
      withImported ed25519PrivHex $ \kp ->
        skBytes (kpPrivate kp) `shouldBe` unhex ed25519RawPrivHex

    it "signs with the imported private key; the spec public key verifies" $
      withImported ed25519PrivHex $ \kp -> do
        let msg = "cross-implementation ed25519" :: ByteString
        case sign (kpPrivate kp) msg of
          Left err -> expectationFailure err
          Right sig -> verifyWithSpecPub ed25519PubHex msg sig

    it "accepts the legacy 96-byte form when both public keys match" $ do
      let raw96 = unhex ed25519RawPrivHex <> unhex ed25519RawPubHex
      case keyPairFromPrivateKey (PrivateKey Ed25519 raw96) of
        Left err -> expectationFailure err
        Right kp -> do
          skBytes (kpPrivate kp) `shouldBe` unhex ed25519RawPrivHex
          pkBytes (kpPublic kp) `shouldBe` unhex ed25519RawPubHex

    it "rejects the legacy 96-byte form when the public keys differ" $ do
      let raw96 = unhex ed25519RawPrivHex <> BS.replicate 32 0x00
      shouldFailImport (PrivateKey Ed25519 raw96)

    it "rejects a bare 32-byte seed (invalid per the spec)" $ do
      let seed = BS.take 32 (unhex ed25519RawPrivHex)
      shouldFailImport (PrivateKey Ed25519 seed)

  describe "ECDSA (P-256)" $ do
    it "derives the spec public key from the spec DER private key" $
      withImported ecdsaPrivHex $ \kp ->
        encodePublicKey (kpPublic kp) `shouldBe` unhex ecdsaPubHex

    it "signs with the imported DER private key; the spec public key verifies" $
      withImported ecdsaPrivHex $ \kp -> do
        let msg = "cross-implementation ecdsa" :: ByteString
        signed <- ECDSA.signIO (skBytes (kpPrivate kp)) msg
        case signed of
          Left err -> expectationFailure err
          Right sig -> verifyWithSpecPub ecdsaPubHex msg sig

    it "round-trips locally generated key pairs through the import path" $ do
      kp <- generateECDSAKeyPair
      case keyPairFromPrivateKey (kpPrivate kp) of
        Left err -> expectationFailure err
        Right kp' -> pkBytes (kpPublic kp') `shouldBe` pkBytes (kpPublic kp)

  describe "RSA (4096-bit spec vector)" $ do
    it "signs with the imported private key (no hardcoded modulus size)" $
      withImported rsaPrivHex $ \kp -> do
        let msg = "cross-implementation rsa" :: ByteString
        case sign (kpPrivate kp) msg of
          Left err -> expectationFailure err
          Right sig -> verifyWithSpecPub rsaPubHex msg sig

    it "derives the spec public key from the spec private key" $
      withImported rsaPrivHex $ \kp ->
        encodePublicKey (kpPublic kp) `shouldBe` unhex rsaPubHex

  describe "secp256k1" $ do
    it "derives the spec public key from the spec private key" $
      withImported secp256k1PrivHex $ \kp ->
        encodePublicKey (kpPublic kp) `shouldBe` unhex secp256k1PubHex

    it "signs with the imported private key; the spec public key verifies" $
      withImported secp256k1PrivHex $ \kp -> do
        let msg = "cross-implementation secp256k1" :: ByteString
        signed <- Secp256k1.signIO (skBytes (kpPrivate kp)) msg
        case signed of
          Left err -> expectationFailure err
          Right sig -> verifyWithSpecPub secp256k1PubHex msg sig

  describe "PrivateKey protobuf" $ do
    it "re-encodes every decoded spec vector byte-identically" $
      mapM_ reencodesIdentically
        [ed25519PrivHex, ecdsaPrivHex, rsaPrivHex, secp256k1PrivHex]

    it "fails gracefully on an unknown key type" $ do
      let bs = BS.pack [0x08, 0x63, 0x12, 0x00]
      case decodePrivateKey bs of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected decode to fail for unknown type"

-- | Decode a protobuf PrivateKey vector, derive the key pair, and run a check.
withImported :: String -> (KeyPair -> Expectation) -> Expectation
withImported privHex check =
  case decodePrivateKey (unhex privHex) >>= keyPairFromPrivateKey of
    Left err -> expectationFailure err
    Right kp -> check kp

-- | Verify a signature against a spec PublicKey protobuf vector.
verifyWithSpecPub :: String -> ByteString -> ByteString -> Expectation
verifyWithSpecPub pubHex msg sig =
  case decodePublicKey (unhex pubHex) of
    Left err -> expectationFailure err
    Right pub -> verify pub msg sig `shouldBe` True

-- | decode >>> encode must reproduce the exact vector bytes.
reencodesIdentically :: String -> Expectation
reencodesIdentically privHex =
  case decodePrivateKey (unhex privHex) of
    Left err -> expectationFailure err
    Right sk -> encodePrivateKey sk `shouldBe` unhex privHex

-- | Importing this private key must be rejected.
shouldFailImport :: PrivateKey -> Expectation
shouldFailImport sk =
  case keyPairFromPrivateKey sk of
    Left _ -> pure ()
    Right _ -> expectationFailure "expected key import to fail"
