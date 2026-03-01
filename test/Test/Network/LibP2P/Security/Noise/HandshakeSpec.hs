module Test.Network.LibP2P.Security.Noise.HandshakeSpec (spec) where

import qualified Data.ByteString as BS
import Network.LibP2P.Crypto.Ed25519 (generateKeyPair)
import Network.LibP2P.Crypto.Key
import Network.LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import Network.LibP2P.Crypto.Protobuf (encodePublicKey)
import Network.LibP2P.Security.Noise.Framing
import Network.LibP2P.Security.Noise.Handshake
import Network.LibP2P.Security.Noise.Session
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
      Right kp <- generateKeyPair
      let noiseStaticPK = BS.replicate 32 0xAA -- simulated X25519 pubkey
      case signStaticKey (kpPrivate kp) noiseStaticPK of
        Right sig -> verifyStaticKey (kpPublic kp) noiseStaticPK sig `shouldBe` True
        Left err -> expectationFailure err

    it "verification fails with wrong static key" $ do
      Right kp <- generateKeyPair
      let noiseStaticPK = BS.replicate 32 0xAA
      case signStaticKey (kpPrivate kp) noiseStaticPK of
        Right sig -> do
          let wrongPK = BS.replicate 32 0xBB
          verifyStaticKey (kpPublic kp) wrongPK sig `shouldBe` False
        Left err -> expectationFailure err

    it "verification fails with wrong identity key" $ do
      Right kp1 <- generateKeyPair
      Right kp2 <- generateKeyPair
      let noiseStaticPK = BS.replicate 32 0xAA
      case signStaticKey (kpPrivate kp1) noiseStaticPK of
        Right sig -> verifyStaticKey (kpPublic kp2) noiseStaticPK sig `shouldBe` False
        Left err -> expectationFailure err

  describe "NoisePayload protobuf" $ do
    it "encodes and decodes identity_key and identity_sig" $ do
      Right kp <- generateKeyPair
      let identKey = encodePublicKey (kpPublic kp)
      let noiseStaticPK = BS.replicate 32 0xCC
      case signStaticKey (kpPrivate kp) noiseStaticPK of
        Right identSig -> do
          let payload = NoisePayload identKey identSig
          let encoded = encodeNoisePayload payload
          decodeNoisePayload encoded `shouldBe` Right payload
        Left err -> expectationFailure err

    it "round-trip with various sizes" $ do
      let key = BS.replicate 36 0x11
      let sig = BS.replicate 64 0x22
      let payload = NoisePayload key sig
      decodeNoisePayload (encodeNoisePayload payload) `shouldBe` Right payload

    it "fails on empty input" $
      decodeNoisePayload BS.empty `shouldSatisfy` isLeft

  describe "Noise XX handshake" $ do
    it "completes 3-message handshake between initiator and responder" $ do
      -- Generate identity key pairs for both peers
      Right aliceIdentity <- generateKeyPair
      Right bobIdentity <- generateKeyPair

      -- Create handshake states
      (aliceInit, aliceNoiseStaticPub) <- initHandshakeInitiator aliceIdentity
      (bobInit, bobNoiseStaticPub) <- initHandshakeResponder bobIdentity

      -- Message 1: Alice -> Bob (ephemeral key, empty payload)
      (msg1, aliceState1) <- either fail pure $ writeHandshakeMsg aliceInit BS.empty
      (payload1, bobState1) <- either fail pure $ readHandshakeMsg bobInit msg1

      -- Message 1 has no encrypted payload
      payload1 `shouldBe` BS.empty

      -- Message 2: Bob -> Alice (ephemeral + static key, identity payload)
      let bobPayload = encodeNoisePayload $ buildHandshakePayload bobIdentity bobNoiseStaticPub
      (msg2, bobState2) <- either fail pure $ writeHandshakeMsg bobState1 bobPayload
      (payload2, aliceState2) <- either fail pure $ readHandshakeMsg aliceState1 msg2

      -- Alice can decode Bob's identity from payload2
      case decodeNoisePayload payload2 of
        Left err -> expectationFailure $ "Failed to decode Bob's payload: " <> err
        Right np -> do
          let remotePubKey = decodePublicKey (npIdentityKey np)
          remotePubKey `shouldSatisfy` isRight

      -- Message 3: Alice -> Bob (static key, identity payload)
      let alicePayload = encodeNoisePayload $ buildHandshakePayload aliceIdentity aliceNoiseStaticPub
      (msg3, aliceSession) <- either fail pure $ writeHandshakeMsg aliceState2 alicePayload
      (payload3, bobSession) <- either fail pure $ readHandshakeMsg bobState2 msg3

      -- Bob can decode Alice's identity from payload3
      case decodeNoisePayload payload3 of
        Left err -> expectationFailure $ "Failed to decode Alice's payload: " <> err
        Right np -> do
          let remotePubKey = decodePublicKey (npIdentityKey np)
          remotePubKey `shouldSatisfy` isRight

      -- Both sides should indicate handshake is complete
      sessionComplete aliceSession `shouldBe` True
      sessionComplete bobSession `shouldBe` True

    it "derives correct remote Peer IDs" $ do
      Right aliceIdentity <- generateKeyPair
      Right bobIdentity <- generateKeyPair

      let alicePeerId = fromPublicKey (kpPublic aliceIdentity)
      let bobPeerId = fromPublicKey (kpPublic bobIdentity)

      result <- performFullHandshake aliceIdentity bobIdentity
      case result of
        Left err -> expectationFailure err
        Right (aliceRemotePeerId, bobRemotePeerId) -> do
          -- Alice sees Bob's PeerId
          aliceRemotePeerId `shouldBe` bobPeerId
          -- Bob sees Alice's PeerId
          bobRemotePeerId `shouldBe` alicePeerId

    it "replayed identity payload is detected by verifyStaticKey (MitM scenario)" $ do
      -- Scenario: Eve intercepts and runs her own Noise session with Alice,
      -- but sends Bob's identity payload (key + signature). The signature was
      -- over Bob's Noise static key, not Eve's, so verification fails.
      Right aliceIdentity <- generateKeyPair
      Right bobIdentity <- generateKeyPair

      (aliceInit, _aliceNoiseStaticPub) <- initHandshakeInitiator aliceIdentity
      -- Eve runs her own Noise session (different Noise keys than Bob)
      (eveInit, _eveNoiseStaticPub) <- initHandshakeResponder bobIdentity

      -- Bob's payload signed over a DIFFERENT Noise static key (not the one in this session)
      -- Simulate: Bob pre-signed his identity for a different Noise session
      let bobOtherNoiseKey = BS.replicate 32 0xBB  -- arbitrary, not the actual session key
      let replayedPayload = encodeNoisePayload $ buildHandshakePayload bobIdentity bobOtherNoiseKey

      -- Message 1 (normal)
      (msg1, aliceState1) <- either fail pure $ writeHandshakeMsg aliceInit BS.empty
      (_payload1, eveState1) <- either fail pure $ readHandshakeMsg eveInit msg1

      -- Message 2: Eve sends Bob's identity payload (replay attack)
      (msg2, _eveState2) <- either fail pure $ writeHandshakeMsg eveState1 replayedPayload
      (payload2, aliceState2) <- either fail pure $ readHandshakeMsg aliceState1 msg2

      -- Alice decodes payload (valid protobuf, Bob's key)
      let Right remoteNP = decodeNoisePayload payload2
      let Right remotePubKey = decodePublicKey (npIdentityKey remoteNP)

      -- Alice extracts the ACTUAL Noise static key from the handshake state
      case getRemoteNoiseStaticKey aliceState2 of
        Nothing -> expectationFailure "expected remote Noise static key"
        Just remoteNoisePub -> do
          -- Verification MUST fail: Bob's signature was over bobOtherNoiseKey,
          -- but the actual Noise static key is Eve's
          verifyStaticKey remotePubKey remoteNoisePub (npIdentitySig remoteNP)
            `shouldBe` False

    it "valid identity signature passes verifyStaticKey" $ do
      Right aliceIdentity <- generateKeyPair
      Right bobIdentity <- generateKeyPair

      (aliceInit, _aliceNoiseStaticPub) <- initHandshakeInitiator aliceIdentity
      (bobInit, bobNoiseStaticPub) <- initHandshakeResponder bobIdentity

      -- Normal 3-message handshake
      (msg1, aliceState1) <- either fail pure $ writeHandshakeMsg aliceInit BS.empty
      (_payload1, bobState1) <- either fail pure $ readHandshakeMsg bobInit msg1

      let bobPayload = encodeNoisePayload $ buildHandshakePayload bobIdentity bobNoiseStaticPub
      (msg2, _bobState2) <- either fail pure $ writeHandshakeMsg bobState1 bobPayload
      (payload2, aliceState2) <- either fail pure $ readHandshakeMsg aliceState1 msg2

      -- Alice decodes Bob's legitimate payload
      let Right remoteNP = decodeNoisePayload payload2
      let Right remotePubKey = decodePublicKey (npIdentityKey remoteNP)

      -- Verification MUST pass: Bob signed his own Noise static key
      case getRemoteNoiseStaticKey aliceState2 of
        Nothing -> expectationFailure "expected remote Noise static key"
        Just remoteNoisePub ->
          verifyStaticKey remotePubKey remoteNoisePub (npIdentitySig remoteNP)
            `shouldBe` True

    it "post-handshake encrypted transport works" $ do
      Right aliceIdentity <- generateKeyPair
      Right bobIdentity <- generateKeyPair

      result <- performFullHandshakeWithSessions aliceIdentity bobIdentity
      case result of
        Left err -> expectationFailure err
        Right (aliceSession, bobSession) -> do
          -- Alice encrypts a message for Bob
          let plaintext = "Hello Bob from Alice!"
          (ciphertext, aliceSession') <- either fail pure $ encryptMessage aliceSession plaintext
          (decrypted, bobSession') <- either fail pure $ decryptMessage bobSession ciphertext
          decrypted `shouldBe` plaintext

          -- Bob encrypts a message for Alice
          let reply = "Hello Alice from Bob!"
          (ciphertext2, _bobSession'') <- either fail pure $ encryptMessage bobSession' reply
          (decrypted2, _aliceSession'') <- either fail pure $ decryptMessage aliceSession' ciphertext2
          decrypted2 `shouldBe` reply

    it "post-handshake messages are order-dependent (nonce)" $ do
      Right aliceIdentity <- generateKeyPair
      Right bobIdentity <- generateKeyPair

      result <- performFullHandshakeWithSessions aliceIdentity bobIdentity
      case result of
        Left err -> expectationFailure err
        Right (aliceSession, bobSession) -> do
          -- Alice sends two messages
          (ct1, aliceSession') <- either fail pure $ encryptMessage aliceSession "msg1"
          (ct2, _aliceSession'') <- either fail pure $ encryptMessage aliceSession' "msg2"

          -- Bob must decrypt in order
          (_pt1, bobSession') <- either fail pure $ decryptMessage bobSession ct1
          (pt2, _bobSession'') <- either fail pure $ decryptMessage bobSession' ct2
          pt2 `shouldBe` "msg2"

          -- Decrypting out of order should fail
          case decryptMessage bobSession ct2 of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected decryption to fail out of order"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
