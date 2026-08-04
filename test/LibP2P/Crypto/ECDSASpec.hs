module LibP2P.Crypto.ECDSASpec (spec) where

import Crypto.Number.Serialize (i2ospOf_)
import Data.ASN1.BinaryEncoding (DER (..))
import Data.ASN1.BitArray (toBitArray)
import Data.ASN1.Encoding (decodeASN1', encodeASN1')
import Data.ASN1.Types (ASN1 (..), ASN1Class (..), ASN1ConstructionType (..))
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
      case ECDSA.sign (skBytes (kpPrivate kp)) msg of
        Left err -> expectationFailure err
        Right sig -> do
          verify (kpPublic kp) msg sig `shouldBe` True
          verify (kpPublic kp) "tampered" sig `shouldBe` False

    it "signs deterministically (same key and message yield identical signatures)" $ do
      kp <- generateECDSAKeyPair
      let msg = "deterministic nonce"
          sk = skBytes (kpPrivate kp)
      ECDSA.sign sk msg `shouldBe` ECDSA.sign sk msg

    it "uses RFC 6979 deterministic nonces (A.2.5 P-256/SHA-256 'sample' vector)" $ do
      -- RFC 6979 appendix A.2.5, message "sample", SHA-256.
      let d = 0xC9AFA9D845BA75166B5C215767B1D6934E50C3DB36E89B127B8A622B120F6721
          ux = 0x60FED4BA255A9D31C961EB74C6356D68C049B8923B61FA6CE669622E60F29FB6
          uy = 0x7903FE1008B8BC99A41AE9E95628BC64F2F1B20C2D7E9F5177A3C294D4462299
          rExpected = 0xEFD48B2AACB6A8FD1140DD9CD45E81D69D2C877B56AAF991C34D0EA84EAF3716
          sExpected = 0xF7CB1C942D657C41D436C7A1B6E29F65F3E900DBB9AFF4064DC4AB2F843ACDA8
          n = 0xFFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551
      case ECDSA.sign (rfc5915Der d ux uy) "sample" of
        Left err -> expectationFailure err
        Right sigDer -> case decodeASN1' DER sigDer of
          Right [Start Sequence, IntVal r, IntVal s, End Sequence] -> do
            r `shouldBe` rExpected
            -- crypton normalizes signatures to the low-s form.
            (s == sExpected || s == n - sExpected) `shouldBe` True
          other -> expectationFailure ("unexpected signature structure: " <> show other)

-- | Build an RFC 5915 ECPrivateKey DER for a P-256 scalar and public point.
rfc5915Der :: Integer -> Integer -> Integer -> BS.ByteString
rfc5915Der d ux uy =
  encodeASN1'
    DER
    [ Start Sequence
    , IntVal 1
    , OctetString (i2ospOf_ 32 d)
    , Start (Container Context 0)
    , OID [1, 2, 840, 10045, 3, 1, 7]
    , End (Container Context 0)
    , Start (Container Context 1)
    , BitString (toBitArray (BS.cons 0x04 (i2ospOf_ 32 ux <> i2ospOf_ 32 uy)) 0)
    , End (Container Context 1)
    , End Sequence
    ]
