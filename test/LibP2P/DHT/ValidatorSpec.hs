-- | Tests for DHT record validation (issue #148).
--
-- Per specs/kad-dht, PUT_VALUE records must be validated before storage
-- and the /pk/ namespace binds the record key to the public key value:
-- the key is "/pk/" ++ multihash where the multihash must equal the
-- Peer ID derived from the serialized public key in the value.
module LibP2P.DHT.ValidatorSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Either (isLeft)
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (KeyPair (..))
import LibP2P.Crypto.PeerId (fromPublicKey, peerIdBytes)
import LibP2P.Crypto.Protobuf (encodePublicKey)
import LibP2P.DHT.Validator

-- | Generate a fresh Ed25519 identity for /pk/ record fixtures.
mkIdentity :: IO (BS.ByteString, BS.ByteString)
-- ^ Returns (peer ID multihash bytes, serialized PublicKey protobuf).
mkIdentity = do
  ekp <- generateKeyPair
  case ekp of
    Left err -> fail ("keypair generation failed: " ++ err)
    Right kp -> do
      let pub = kpPublic kp
          pid = fromPublicKey pub
      pure (peerIdBytes pid, encodePublicKey pub)

spec :: Spec
spec = do
  describe "splitRecordKey" $ do
    it "splits /pk/<mh> into namespace and path" $ do
      splitRecordKey (BSC.pack "/pk/" <> BS.pack [1, 2, 3])
        `shouldBe` Right (BSC.pack "pk", BS.pack [1, 2, 3])

    it "rejects a key without a leading slash" $ do
      splitRecordKey (BSC.pack "pk/abc") `shouldSatisfy` isLeft

    it "rejects a key without a namespace separator" $ do
      splitRecordKey (BSC.pack "/pk") `shouldSatisfy` isLeft

  describe "pkValidator" $ do
    it "accepts a record whose key is the peer ID of the value's public key" $ do
      (mh, pubBytes) <- mkIdentity
      valValidate pkValidator (BSC.pack "/pk/" <> mh) pubBytes
        `shouldBe` Right ()

    it "rejects a record whose key names a different peer" $ do
      (_, pubBytes) <- mkIdentity
      (otherMh, _) <- mkIdentity
      valValidate pkValidator (BSC.pack "/pk/" <> otherMh) pubBytes
        `shouldSatisfy` isLeft

    it "rejects a value that is not a serialized public key" $ do
      (mh, _) <- mkIdentity
      valValidate pkValidator (BSC.pack "/pk/" <> mh) (BSC.pack "garbage")
        `shouldSatisfy` isLeft

    it "selects the first record" $ do
      valSelect pkValidator (BSC.pack "/pk/x") [BSC.pack "a", BSC.pack "b"]
        `shouldBe` Right 0

    it "select fails on an empty candidate list" $ do
      valSelect pkValidator (BSC.pack "/pk/x") [] `shouldSatisfy` isLeft

  describe "defaultValidator (namespaced)" $ do
    it "delegates /pk/ records to the pk validator" $ do
      (mh, pubBytes) <- mkIdentity
      valValidate defaultValidator (BSC.pack "/pk/" <> mh) pubBytes
        `shouldBe` Right ()

    it "rejects keys in an unregistered namespace" $ do
      valValidate defaultValidator (BSC.pack "/unknown/key") (BSC.pack "v")
        `shouldSatisfy` isLeft

    it "rejects keys with no namespace" $ do
      valValidate defaultValidator (BSC.pack "rawkey") (BSC.pack "v")
        `shouldSatisfy` isLeft
