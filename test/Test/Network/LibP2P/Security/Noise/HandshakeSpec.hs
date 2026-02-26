module Test.Network.LibP2P.Security.Noise.HandshakeSpec (spec) where

import qualified Data.ByteString as BS
import Network.LibP2P.Crypto.Ed25519 (generateKeyPair)
import Network.LibP2P.Crypto.Key
import Network.LibP2P.Crypto.Protobuf (encodePublicKey)
import Network.LibP2P.Security.Noise.Framing
import Network.LibP2P.Security.Noise.Handshake
import Test.Hspec

spec :: Spec
spec = do
  describe "Framing" $ do
    it "encodes with 2-byte BE length prefix" $ do
      let msg = BS.pack [0x01, 0x02, 0x03]
      encodeFrame msg `shouldBe` BS.pack [0x00, 0x03, 0x01, 0x02, 0x03]

    it "decodes framed message" $ do
      let framed = BS.pack [0x00, 0x03, 0x01, 0x02, 0x03]
      decodeFrame framed `shouldBe` Right (BS.pack [0x01, 0x02, 0x03], BS.empty)

    it "decodes with remaining bytes" $ do
      let framed = BS.pack [0x00, 0x02, 0xAA, 0xBB, 0xCC]
      decodeFrame framed `shouldBe` Right (BS.pack [0xAA, 0xBB], BS.pack [0xCC])

    it "fails on short input" $
      decodeFrame (BS.pack [0x00]) `shouldSatisfy` isLeft

    it "fails when not enough payload bytes" $ do
      let framed = BS.pack [0x00, 0x05, 0x01, 0x02]
      decodeFrame framed `shouldSatisfy` isLeft

    it "round-trip" $ do
      let msg = BS.replicate 100 0x42
      case decodeFrame (encodeFrame msg) of
        Right (decoded, remaining) -> do
          decoded `shouldBe` msg
          remaining `shouldBe` BS.empty
        Left err -> expectationFailure err

  describe "Static key signing" $ do
    it "signStaticKey produces verifiable signature" $ do
      kp <- generateKeyPair
      let noiseStaticPK = BS.replicate 32 0xAA -- simulated X25519 pubkey
      let sig = signStaticKey (kpPrivate kp) noiseStaticPK
      verifyStaticKey (kpPublic kp) noiseStaticPK sig `shouldBe` True

    it "verification fails with wrong static key" $ do
      kp <- generateKeyPair
      let noiseStaticPK = BS.replicate 32 0xAA
      let sig = signStaticKey (kpPrivate kp) noiseStaticPK
      let wrongPK = BS.replicate 32 0xBB
      verifyStaticKey (kpPublic kp) wrongPK sig `shouldBe` False

    it "verification fails with wrong identity key" $ do
      kp1 <- generateKeyPair
      kp2 <- generateKeyPair
      let noiseStaticPK = BS.replicate 32 0xAA
      let sig = signStaticKey (kpPrivate kp1) noiseStaticPK
      verifyStaticKey (kpPublic kp2) noiseStaticPK sig `shouldBe` False

  describe "NoisePayload protobuf" $ do
    it "encodes and decodes identity_key and identity_sig" $ do
      kp <- generateKeyPair
      let identKey = encodePublicKey (kpPublic kp)
      let noiseStaticPK = BS.replicate 32 0xCC
      let identSig = signStaticKey (kpPrivate kp) noiseStaticPK
      let payload = NoisePayload identKey identSig
      let encoded = encodeNoisePayload payload
      decodeNoisePayload encoded `shouldBe` Right payload

    it "round-trip with various sizes" $ do
      let key = BS.replicate 36 0x11
      let sig = BS.replicate 64 0x22
      let payload = NoisePayload key sig
      decodeNoisePayload (encodeNoisePayload payload) `shouldBe` Right payload

    it "fails on empty input" $
      decodeNoisePayload BS.empty `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
