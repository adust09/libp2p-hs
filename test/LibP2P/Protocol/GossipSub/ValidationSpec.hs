module LibP2P.Protocol.GossipSub.ValidationSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (KeyPair (..), sign)
import LibP2P.Crypto.PeerId (PeerId (..), fromPublicKey)
import LibP2P.Crypto.Protobuf (encodePublicKey)
import LibP2P.Protocol.GossipSub.Types
import LibP2P.Protocol.GossipSub.Validation

-- | Build a correctly signed message from a key pair, mirroring the publish path.
signedMessage :: KeyPair -> Topic -> BS.ByteString -> PubSubMessage
signedMessage kp topic payload =
  let PeerId from = fromPublicKey (kpPublic kp)
      unsigned = PubSubMessage
        { msgFrom      = Just from
        , msgData      = payload
        , msgSeqNo     = Just (BS.pack [0, 0, 0, 0, 0, 0, 0, 1])
        , msgTopic     = topic
        , msgSignature = Nothing
        , msgKey       = Just (encodePublicKey (kpPublic kp))
        }
  in case sign (kpPrivate kp) (signingBytes unsigned) of
       Left err  -> error ("test fixture signing failed: " <> err)
       Right sig -> unsigned { msgSignature = Just sig }

newKeyPair :: IO KeyPair
newKeyPair = either (error . ("keygen failed: " <>)) id <$> generateKeyPair

spec :: Spec
spec = do
  describe "validateMessage (StrictSign)" $ do
    it "accepts a correctly signed message" $ do
      kp <- newKeyPair
      validateMessage StrictSign (signedMessage kp "blocks" "payload")
        `shouldBe` Right ()

    it "accepts a signed message with the key field omitted (inlined in from)" $ do
      kp <- newKeyPair
      let msg = signedMessage kp "blocks" "payload"
      validateMessage StrictSign (msg { msgKey = Nothing }) `shouldBe` Right ()

    it "rejects a message whose payload was tampered with" $ do
      kp <- newKeyPair
      let msg = signedMessage kp "blocks" "payload"
      validateMessage StrictSign (msg { msgData = "tampered" })
        `shouldBe` Left BadSignature

    it "rejects a message whose topic was tampered with" $ do
      kp <- newKeyPair
      let msg = signedMessage kp "blocks" "payload"
      validateMessage StrictSign (msg { msgTopic = "other" })
        `shouldBe` Left BadSignature

    it "rejects a garbage signature" $ do
      kp <- newKeyPair
      let msg = signedMessage kp "blocks" "payload"
      validateMessage StrictSign (msg { msgSignature = Just (BS.replicate 64 0) })
        `shouldBe` Left BadSignature

    it "rejects a spoofed from that does not match the key" $ do
      kp <- newKeyPair
      other <- newKeyPair
      let msg = signedMessage kp "blocks" "payload"
          PeerId otherFrom = fromPublicKey (kpPublic other)
      validateMessage StrictSign (msg { msgFrom = Just otherFrom })
        `shouldBe` Left KeyPeerIdMismatch

    it "rejects a key that does not derive the from peer ID" $ do
      kp <- newKeyPair
      other <- newKeyPair
      let msg = signedMessage kp "blocks" "payload"
      validateMessage StrictSign (msg { msgKey = Just (encodePublicKey (kpPublic other)) })
        `shouldBe` Left KeyPeerIdMismatch

    it "rejects a malformed key" $ do
      kp <- newKeyPair
      let msg = signedMessage kp "blocks" "payload"
      case validateMessage StrictSign (msg { msgKey = Just "not-a-protobuf" }) of
        Left (MalformedKey _) -> pure ()
        other -> expectationFailure ("expected MalformedKey, got " <> show other)

    it "rejects missing signature, from and seqno" $ do
      kp <- newKeyPair
      let msg = signedMessage kp "blocks" "payload"
      validateMessage StrictSign (msg { msgSignature = Nothing })
        `shouldBe` Left (MissingField "signature")
      validateMessage StrictSign (msg { msgFrom = Nothing })
        `shouldBe` Left (MissingField "from")
      validateMessage StrictSign (msg { msgSeqNo = Nothing })
        `shouldBe` Left (MissingField "seqno")

  describe "validateMessage (StrictNoSign)" $ do
    it "accepts a message with no signing fields" $ do
      let msg = PubSubMessage Nothing "payload" Nothing "blocks" Nothing Nothing
      validateMessage StrictNoSign msg `shouldBe` Right ()

    it "rejects any present signing field" $ do
      let msg = PubSubMessage Nothing "payload" Nothing "blocks" Nothing Nothing
      validateMessage StrictNoSign (msg { msgSignature = Just "sig" })
        `shouldBe` Left (UnexpectedField "signature")
      validateMessage StrictNoSign (msg { msgKey = Just "key" })
        `shouldBe` Left (UnexpectedField "key")
      validateMessage StrictNoSign (msg { msgFrom = Just "from" })
        `shouldBe` Left (UnexpectedField "from")
      validateMessage StrictNoSign (msg { msgSeqNo = Just "seqno" })
        `shouldBe` Left (UnexpectedField "seqno")
