module LibP2P.Crypto.Secp256k1Spec (spec) where

import Crypto.Number.Serialize (i2ospOf_)
import Data.ASN1.BinaryEncoding (DER (..))
import Data.ASN1.Encoding (decodeASN1')
import Data.ASN1.Types (ASN1 (..), ASN1ConstructionType (..))
import qualified Data.ByteString as BS
import Data.Either (isLeft)
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
      case Secp256k1.sign (skBytes (kpPrivate kp)) msg of
        Left err -> expectationFailure err
        Right sig -> do
          verify (kpPublic kp) msg sig `shouldBe` True
          verify (kpPublic kp) "tampered" sig `shouldBe` False

    it "signs deterministically (same key and message yield identical signatures)" $ do
      kp <- generateSecp256k1KeyPair
      let msg = "deterministic nonce"
          sk = skBytes (kpPrivate kp)
      Secp256k1.sign sk msg `shouldBe` Secp256k1.sign sk msg

    it "uses RFC 6979 deterministic nonces (secp256k1/SHA-256 known vector)" $ do
      -- Well-known RFC 6979 secp256k1 vector: d = 1, message "Satoshi Nakamoto".
      let rExpected = 0x934B1EA10A4B3C1757E2B0C017D0B6143CE3C9A7E6A4A49860D7A6AB210EE3D8
          sExpected = 0xDBBD3162D46E9F9BEF7FEB87C16DC13B4F6568A87F4E83F728E2443BA586675C
          n = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
      case Secp256k1.sign (i2ospOf_ 32 (1 :: Integer)) "Satoshi Nakamoto" of
        Left err -> expectationFailure err
        Right sigDer -> case decodeASN1' DER sigDer of
          Right [Start Sequence, IntVal r, IntVal s, End Sequence] -> do
            r `shouldBe` rExpected
            -- crypton normalizes signatures to the low-s form.
            (s == sExpected || s == n - sExpected) `shouldBe` True
          other -> expectationFailure ("unexpected signature structure: " <> show other)

  describe "secp256k1 point decompression" $ do
    it "decodes a generated public key to a curve point" $ do
      kp <- generateSecp256k1KeyPair
      case Secp256k1.decodePoint (pkBytes (publicKey kp)) of
        Left err -> expectationFailure err
        Right _ -> pure ()

    it "rejects a compressed point whose X is not on the curve" $ do
      -- x = 5: x^3 + 7 is a quadratic non-residue mod p, so no point
      -- with this X coordinate exists on secp256k1.
      let bad = BS.cons 0x02 (i2ospOf_ 32 (5 :: Integer))
      isLeft (Secp256k1.decodePoint bad) `shouldBe` True

    it "rejects a compressed point with X outside the field" $ do
      let pField = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
          bad = BS.cons 0x02 (i2ospOf_ 32 (pField :: Integer))
      isLeft (Secp256k1.decodePoint bad) `shouldBe` True

    it "rejects a point with a bad SEC1 prefix" $ do
      let bad = BS.cons 0x04 (BS.replicate 32 0x01)
      isLeft (Secp256k1.decodePoint bad) `shouldBe` True
