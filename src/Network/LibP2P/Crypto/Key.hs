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

import Data.ByteString (ByteString)

-- | Supported key types per the libp2p spec.
data KeyType
  = Ed25519
  deriving (Show, Eq, Ord)

-- | A public key with its type.
data PublicKey = PublicKey
  { pkType :: KeyType
  , pkBytes :: ByteString -- ^ Raw public key bytes
  }
  deriving (Show, Eq)

-- | A private key with its type.
data PrivateKey = PrivateKey
  { skType :: KeyType
  , skBytes :: ByteString -- ^ Raw private key seed bytes
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
sign = error "Not yet implemented"

-- | Verify a signature against a public key and message.
verify :: PublicKey -> ByteString -> ByteString -> Bool
verify = error "Not yet implemented"
