module Test.Network.LibP2P.Crypto.SignedEnvelopeSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import Network.LibP2P.Crypto.SignedEnvelope
import Network.LibP2P.Crypto.Key (KeyPair (..))
import Network.LibP2P.Crypto.Ed25519 (generateKeyPair)
import Network.LibP2P.Core.Varint (encodeUvarint)
import Data.Word (Word64)

spec :: Spec
spec = do
  describe "SignedEnvelope" $ do
    it "create → verify round-trip succeeds" $ do
      Right kp <- generateKeyPair
      let domain = "test-domain"
          payloadType = BS.pack [0x03, 0x02]  -- example multicodec
          payload = BS.pack [0xDE, 0xAD, 0xBE, 0xEF]
      case createEnvelope (kpPrivate kp) (kpPublic kp) domain payloadType payload of
        Left err -> expectationFailure $ "createEnvelope failed: " ++ err
        Right env -> do
          sePublicKey env `shouldBe` kpPublic kp
          seDomain env `shouldBe` domain
          sePayloadType env `shouldBe` payloadType
          sePayload env `shouldBe` payload
          verifyEnvelope env domain `shouldBe` True

    it "verify fails with wrong domain" $ do
      Right kp <- generateKeyPair
      let domain = "correct-domain"
          payloadType = BS.pack [0x01]
          payload = BS.pack [1, 2, 3]
      case createEnvelope (kpPrivate kp) (kpPublic kp) domain payloadType payload of
        Left err -> expectationFailure $ "createEnvelope failed: " ++ err
        Right env ->
          verifyEnvelope env "wrong-domain" `shouldBe` False

    it "verify fails with tampered payload" $ do
      Right kp <- generateKeyPair
      let domain = "test-domain"
          payloadType = BS.pack [0x03, 0x02]
          payload = BS.pack [0xCA, 0xFE]
      case createEnvelope (kpPrivate kp) (kpPublic kp) domain payloadType payload of
        Left err -> expectationFailure $ "createEnvelope failed: " ++ err
        Right env -> do
          let tampered = env { sePayload = BS.pack [0xFF, 0xFF] }
          verifyEnvelope tampered domain `shouldBe` False

    it "verify fails with tampered signature" $ do
      Right kp <- generateKeyPair
      let domain = "test-domain"
          payloadType = BS.pack [0x03, 0x02]
          payload = BS.pack [0xCA, 0xFE]
      case createEnvelope (kpPrivate kp) (kpPublic kp) domain payloadType payload of
        Left err -> expectationFailure $ "createEnvelope failed: " ++ err
        Right env -> do
          let tampered = env { seSignature = BS.replicate 64 0x00 }
          verifyEnvelope tampered domain `shouldBe` False

    it "encode → decode round-trip preserves all fields" $ do
      Right kp <- generateKeyPair
      let domain = "libp2p-relay-rsvp"
          payloadType = BS.pack [0x03, 0x02]
          payload = BS.pack [0x01, 0x02, 0x03, 0x04, 0x05]
      case createEnvelope (kpPrivate kp) (kpPublic kp) domain payloadType payload of
        Left err -> expectationFailure $ "createEnvelope failed: " ++ err
        Right env -> do
          let encoded = encodeSignedEnvelope env
          case decodeSignedEnvelope encoded of
            Left err -> expectationFailure $ "decodeSignedEnvelope failed: " ++ err
            Right decoded -> do
              sePublicKey decoded `shouldBe` sePublicKey env
              sePayloadType decoded `shouldBe` sePayloadType env
              sePayload decoded `shouldBe` sePayload env
              seSignature decoded `shouldBe` seSignature env

    it "decoded envelope verifies successfully" $ do
      Right kp <- generateKeyPair
      let domain = "libp2p-relay-rsvp"
          payloadType = BS.pack [0x03, 0x02]
          payload = BS.pack [0xAA, 0xBB, 0xCC]
      case createEnvelope (kpPrivate kp) (kpPublic kp) domain payloadType payload of
        Left err -> expectationFailure $ "createEnvelope failed: " ++ err
        Right env -> do
          let encoded = encodeSignedEnvelope env
          case decodeSignedEnvelope encoded of
            Left err -> expectationFailure $ "decodeSignedEnvelope failed: " ++ err
            Right decoded ->
              verifyEnvelope decoded domain `shouldBe` True

    -- RFC 0002 compliance tests
    describe "buildSigningContent (RFC 0002)" $ do
      it "produces varint-length-prefixed format for all three fields" $ do
        -- RFC 0002: [varint(len(domain))][domain][varint(len(payload_type))][payload_type][varint(len(payload))][payload]
        let domain = "test"            -- 4 bytes
            payloadType = BS.pack [0x01, 0x02]  -- 2 bytes
            payload = BS.pack [0xAA, 0xBB, 0xCC]  -- 3 bytes
            result = buildSigningContent domain payloadType payload
            lenPrefix bs = encodeUvarint (fromIntegral (BS.length bs) :: Word64) <> bs
            expected = lenPrefix domain <> lenPrefix payloadType <> lenPrefix payload
        result `shouldBe` expected

      it "encodes varint length prefix for each field independently" $ do
        -- Verify byte-level structure:
        -- domain "AB" (2 bytes): [0x02, 0x41, 0x42]
        -- payloadType [0xFF] (1 byte): [0x01, 0xFF]
        -- payload [0xDE, 0xAD] (2 bytes): [0x02, 0xDE, 0xAD]
        let domain = "AB"
            payloadType = BS.pack [0xFF]
            payload = BS.pack [0xDE, 0xAD]
            result = buildSigningContent domain payloadType payload
            expected = BS.pack [0x02, 0x41, 0x42, 0x01, 0xFF, 0x02, 0xDE, 0xAD]
        result `shouldBe` expected

      it "handles empty payload_type and payload with varint(0) prefix" $ do
        -- Empty fields still get a varint(0) length prefix
        let domain = "d"
            payloadType = BS.empty
            payload = BS.empty
            result = buildSigningContent domain payloadType payload
            -- [0x01, 'd', 0x00, 0x00]
            expected = BS.pack [0x01, 0x64, 0x00, 0x00]
        result `shouldBe` expected
