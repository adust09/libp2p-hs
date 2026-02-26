-- | Key types and KeyPair abstraction for libp2p peer identity.
module Network.LibP2P.Crypto.Key
  ( KeyType (..)
  , KeyPair (..)
  , PublicKey (..)
  , PrivateKey (..)
  , publicKey
  , sign
  , verify
  ) where

import qualified Crypto.Error as CE
import qualified Crypto.PubKey.Ed25519 as Ed
import Data.ByteArray (convert)
import Data.ByteString (ByteString)

-- | Supported key types per the libp2p spec.
data KeyType
  = Ed25519
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
sign :: PrivateKey -> ByteString -> ByteString
sign (PrivateKey Ed25519 skRaw) msg =
  case CE.eitherCryptoError (Ed.secretKey skRaw) of
    Left err -> error $ "sign: invalid secret key: " <> show err
    Right sk ->
      let pk = Ed.toPublic sk
          sig = Ed.sign sk pk msg
       in convert sig

-- | Verify a signature against a public key and message.
verify :: PublicKey -> ByteString -> ByteString -> Bool
verify (PublicKey Ed25519 pkRaw) msg sigRaw =
  case (CE.eitherCryptoError (Ed.publicKey pkRaw), CE.eitherCryptoError (Ed.signature sigRaw)) of
    (Right pk, Right sig) -> Ed.verify pk msg sig
    _ -> False
