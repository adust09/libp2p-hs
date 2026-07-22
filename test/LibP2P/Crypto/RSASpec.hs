module LibP2P.Crypto.RSASpec (spec) where

import qualified Data.ByteString as BS
import LibP2P.Crypto.Key
import LibP2P.Crypto.PeerId
import LibP2P.Crypto.Protobuf
import Test.Hspec

spec :: Spec
spec = do
  describe "RSA peer identity" $ do
    it "derives a SHA-256 multihash PeerId for keys larger than 42 bytes" $ do
      kp <- generateRSAKeyPair
      let pub = publicKey kp
      -- The serialized SPKI DER of a 2048-bit RSA key is far larger than 42 bytes.
      (BS.length (encodePublicKey pub) > 42) `shouldBe` True
      let PeerId mh = fromPublicKey pub
      -- SHA-256 multihash: 0x12 (code) 0x20 (length=32) + 32 bytes.
      BS.head mh `shouldBe` 0x12
      BS.index mh 1 `shouldBe` 0x20
      BS.length mh `shouldBe` 34

    it "round-trips the public key through protobuf" $ do
      kp <- generateRSAKeyPair
      let pub = publicKey kp
      case decodePublicKey (encodePublicKey pub) of
        Left err -> expectationFailure err
        Right pub' -> do
          pkType pub' `shouldBe` RSA
          pkType pub' `shouldBe` pkType pub
          pkBytes pub' `shouldBe` pkBytes pub

    it "signs and verifies a message" $ do
      kp <- generateRSAKeyPair
      let msg = "libp2p rsa identity"
      case sign (kpPrivate kp) msg of
        Left err -> expectationFailure err
        Right sig -> do
          verify (kpPublic kp) msg sig `shouldBe` True
          verify (kpPublic kp) "tampered" sig `shouldBe` False

  describe "protobuf key-type decoding" $ do
    it "fails gracefully on an unknown key type" $ do
      let bs = BS.pack [0x08, 0x63, 0x12, 0x00]
      case decodePublicKey bs of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected decode to fail for unknown type"
