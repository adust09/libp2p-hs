-- | Key types and KeyPair abstraction for libp2p peer identity.
module LibP2P.Crypto.Key
  ( KeyType (..)
  , KeyPair (..)
  , PublicKey (..)
  , PrivateKey (..)
  , publicKey
  , sign
  , verify
  , keyPairFromPrivateKey
  , generateRSAKeyPair
  , generateSecp256k1KeyPair
  , generateECDSAKeyPair
  ) where

import qualified Crypto.Error as CE
import qualified Crypto.PubKey.Ed25519 as Ed
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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
-- Signing is deterministic (and therefore pure) for every key type:
-- Ed25519 and RSA (PKCS#1 v1.5) are deterministic by construction, and
-- secp256k1/ECDSA use RFC 6979 deterministic nonces. Returns Left on
-- invalid key bytes.
sign :: PrivateKey -> ByteString -> Either String ByteString
sign (PrivateKey Ed25519 skRaw) msg
  | BS.length skRaw /= 64 =
      Left "sign: Ed25519 private key must be 64 bytes (seed || public key)"
  | otherwise =
      case CE.eitherCryptoError (Ed.secretKey (BS.take 32 skRaw)) of
        Left err -> Left $ "sign: invalid secret key: " <> show err
        Right sk ->
          let pk = Ed.toPublic sk
              sig = Ed.sign sk pk msg
           in Right (convert sig)
sign (PrivateKey RSA skRaw) msg = RSA.sign skRaw msg
sign (PrivateKey Secp256k1 skRaw) msg = Secp256k1.sign skRaw msg
sign (PrivateKey ECDSA skRaw) msg = ECDSA.sign skRaw msg

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

-- | Reconstruct a full key pair from a private key in libp2p wire format,
-- deriving the public key. This is the import path for keys produced by
-- other implementations (the peer-ids spec requires that implementations
-- can produce the public key from the private key).
keyPairFromPrivateKey :: PrivateKey -> Either String KeyPair
keyPairFromPrivateKey (PrivateKey Ed25519 raw) = do
  privBytes <- normalizeEd25519Private raw
  let (seed, embeddedPk) = BS.splitAt 32 privBytes
  case CE.eitherCryptoError (Ed.secretKey seed) of
    Left err -> Left $ "keyPairFromPrivateKey: " <> show err
    Right sk
      | pkRaw /= embeddedPk ->
          Left "keyPairFromPrivateKey: Ed25519 public key does not match the seed"
      | otherwise ->
          Right (KeyPair (PublicKey Ed25519 pkRaw) (PrivateKey Ed25519 privBytes))
      where
        pkRaw = convert (Ed.toPublic sk)
keyPairFromPrivateKey (PrivateKey RSA raw) =
  (\pub -> KeyPair (PublicKey RSA pub) (PrivateKey RSA raw)) <$> RSA.derivePublicKey raw
keyPairFromPrivateKey (PrivateKey Secp256k1 raw) =
  (\pub -> KeyPair (PublicKey Secp256k1 pub) (PrivateKey Secp256k1 raw))
    <$> Secp256k1.derivePublicKey raw
keyPairFromPrivateKey (PrivateKey ECDSA raw) =
  (\pub -> KeyPair (PublicKey ECDSA pub) (PrivateKey ECDSA raw)) <$> ECDSA.derivePublicKey raw

-- | Normalize Ed25519 private key bytes to the preferred 64-byte form
-- (seed || public key). The legacy 96-byte form (seed || pub || pub) is
-- accepted after verifying that both embedded public keys are identical,
-- per the peer-ids spec.
normalizeEd25519Private :: ByteString -> Either String ByteString
normalizeEd25519Private raw
  | BS.length raw == 64 = Right raw
  | BS.length raw == 96 =
      let (privBytes, redundantPk) = BS.splitAt 64 raw
       in if BS.drop 32 privBytes == redundantPk
            then Right privBytes
            else Left "normalizeEd25519Private: legacy 96-byte key has mismatched public keys"
  | otherwise =
      Left $
        "normalizeEd25519Private: private key must be 64 or 96 bytes, got "
          <> show (BS.length raw)

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
