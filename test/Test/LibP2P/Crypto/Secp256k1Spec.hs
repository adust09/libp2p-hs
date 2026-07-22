module Test.LibP2P.Crypto.Secp256k1Spec (spec) where

import qualified Data.ByteString as BS
import LibP2P.Crypto.Key
import LibP2P.Crypto.PeerId
import LibP2P.Crypto.Protobuf
import qualified LibP2P.Crypto.Secp256k1 as Secp256k1
import Test.Hspec

spec :: Spec
spec = do
  describe "secp256k1 peer identity" $ do
    it "produces a 33-byte compressed public key" $ do
      kp <- generateSecp256k1KeyPair
      let pub = publicKey kp
      pkType pub `shouldBe` Secp256k1
      BS.length (pkBytes pub) `shouldBe` 33
      -- Compressed SEC1 prefix is 0x02 or 0x03.
      (BS.head (pkBytes pub) `elem` [0x02, 0x03]) `shouldBe` True

    it "round-trips the public key through protobuf" $ do
      kp <- generateSecp256k1KeyPair
      let pub = publicKey kp
      case decodePublicKey (encodePublicKey pub) of
        Left err -> expectationFailure err
        Right pub' -> do
          pkType pub' `shouldBe` Secp256k1
          pkBytes pub' `shouldBe` pkBytes pub

    it "round-trips the PeerId through base58" $ do
      kp <- generateSecp256k1KeyPair
      let pid = fromPublicKey (publicKey kp)
      fromBase58 (toBase58 pid) `shouldBe` Right pid

    it "signs and verifies a message" $ do
      kp <- generateSecp256k1KeyPair
      let msg = "libp2p secp256k1 identity"
      signed <- Secp256k1.signIO (skBytes (kpPrivate kp)) msg
      case signed of
        Left err -> expectationFailure err
        Right sig -> do
          verify (kpPublic kp) msg sig `shouldBe` True
          verify (kpPublic kp) "tampered" sig `shouldBe` False
