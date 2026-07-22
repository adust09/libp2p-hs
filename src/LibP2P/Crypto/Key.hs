-- | Key types and KeyPair abstraction for libp2p peer identity.
module LibP2P.Crypto.Key
  ( KeyType (..)
  , KeyPair (..)
  , PublicKey (..)
  , PrivateKey (..)
  , publicKey
  , sign
  , verify
  , generateRSAKeyPair
  , generateSecp256k1KeyPair
  , generateECDSAKeyPair
  ) where

import qualified Crypto.Error as CE
import qualified Crypto.PubKey.Ed25519 as Ed
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified LibP2P.Crypto.ECDSA as ECDSA
import qualified LibP2P.Crypto.RSA as RSA
import qualified LibP2P.Crypto.Secp256k1 as Secp256k1

-- | Supported key types per the libp2p spec.
data KeyType
  = Ed25519
  | RSA
  | Secp256k1
  | ECDSA
  deriving (Show, Eq, Ord)

-- | A public key with its type.
data PublicKey = PublicKey
  { pkType :: KeyType
  , pkBytes :: ByteString
  }
  deriving (Show, Eq)

-- | A private key with its type.
data PrivateKey = PrivateKey
  { skType :: KeyType
  , skBytes :: ByteString
  }

-- | A key pair containing both public and private keys.
data KeyPair = KeyPair
  { kpPublic :: PublicKey
  , kpPrivate :: PrivateKey
  }

-- | Extract the public key from a key pair.
publicKey :: KeyPair -> PublicKey
publicKey = kpPublic

-- | Sign a message with a private key.
--
-- Ed25519 and RSA signing are deterministic and therefore pure. secp256k1 (ECDSA)
-- requires a random nonce, so its signing lives in IO
-- ('LibP2P.Crypto.Secp256k1.signIO'); local peer identities in this
-- implementation are Ed25519. Returns Left on invalid key bytes.
sign :: PrivateKey -> ByteString -> Either String ByteString
sign (PrivateKey Ed25519 skRaw) msg =
  case CE.eitherCryptoError (Ed.secretKey skRaw) of
    Left err -> Left $ "sign: invalid secret key: " <> show err
    Right sk ->
      let pk = Ed.toPublic sk
          sig = Ed.sign sk pk msg
       in Right (convert sig)
sign (PrivateKey RSA skRaw) msg = RSA.sign skRaw msg
sign (PrivateKey Secp256k1 _) _ =
  Left "sign: secp256k1 signing runs in IO (LibP2P.Crypto.Secp256k1.signIO)"
sign (PrivateKey ECDSA _) _ =
  Left "sign: ECDSA signing runs in IO (LibP2P.Crypto.ECDSA.signIO)"

-- | Verify a signature against a public key and message.
-- Supports every libp2p key type so remote peers of any type can be authenticated.
verify :: PublicKey -> ByteString -> ByteString -> Bool
verify (PublicKey Ed25519 pkRaw) msg sigRaw =
  case (CE.eitherCryptoError (Ed.publicKey pkRaw), CE.eitherCryptoError (Ed.signature sigRaw)) of
    (Right pk, Right sig) -> Ed.verify pk msg sig
    _ -> False
verify (PublicKey RSA pkRaw) msg sigRaw = RSA.verify pkRaw msg sigRaw
verify (PublicKey Secp256k1 pkRaw) msg sigRaw = Secp256k1.verify pkRaw msg sigRaw
verify (PublicKey ECDSA pkRaw) msg sigRaw = ECDSA.verify pkRaw msg sigRaw

-- | Generate a new RSA key pair (2048-bit) with libp2p wire-format key bytes.
generateRSAKeyPair :: IO KeyPair
generateRSAKeyPair = do
  (pub, priv) <- RSA.generate
  pure $ KeyPair (PublicKey RSA pub) (PrivateKey RSA priv)

-- | Generate a new secp256k1 key pair with libp2p wire-format key bytes.
generateSecp256k1KeyPair :: IO KeyPair
generateSecp256k1KeyPair = do
  (pub, priv) <- Secp256k1.generate
  pure $ KeyPair (PublicKey Secp256k1 pub) (PrivateKey Secp256k1 priv)

-- | Generate a new ECDSA (P-256) key pair with libp2p wire-format key bytes.
generateECDSAKeyPair :: IO KeyPair
generateECDSAKeyPair = do
  (pub, priv) <- ECDSA.generate
  pure $ KeyPair (PublicKey ECDSA pub) (PrivateKey ECDSA priv)
