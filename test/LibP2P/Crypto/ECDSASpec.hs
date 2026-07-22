module LibP2P.Crypto.ECDSASpec (spec) where

import qualified Data.ByteString as BS
import qualified LibP2P.Crypto.ECDSA as ECDSA
import LibP2P.Crypto.Key
import LibP2P.Crypto.PeerId
import LibP2P.Crypto.Protobuf
import Test.Hspec

spec :: Spec
spec = do
  describe "ECDSA (P-256) peer identity" $ do
    it "generates a key pair of the ECDSA key type" $ do
      kp <- generateECDSAKeyPair
      pkType (publicKey kp) `shouldBe` ECDSA
      (BS.length (pkBytes (publicKey kp)) > 0) `shouldBe` True

    it "round-trips the public key through protobuf" $ do
      kp <- generateECDSAKeyPair
      let pub = publicKey kp
      case decodePublicKey (encodePublicKey pub) of
        Left err -> expectationFailure err
        Right pub' -> do
          pkType pub' `shouldBe` ECDSA
          pkBytes pub' `shouldBe` pkBytes pub

    it "round-trips the PeerId through base58" $ do
      kp <- generateECDSAKeyPair
      let pid = fromPublicKey (publicKey kp)
      fromBase58 (toBase58 pid) `shouldBe` Right pid

    it "signs and verifies a message" $ do
      kp <- generateECDSAKeyPair
      let msg = "libp2p ecdsa identity"
      signed <- ECDSA.signIO (skBytes (kpPrivate kp)) msg
      case signed of
        Left err -> expectationFailure err
        Right sig -> do
          verify (kpPublic kp) msg sig `shouldBe` True
          verify (kpPublic kp) "tampered" sig `shouldBe` False
