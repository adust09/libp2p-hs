-- | Ed25519 key operations using crypton.
module Network.LibP2P.Crypto.Ed25519
  ( generateKeyPair
  , keyPairFromSeed
  ) where

import Data.ByteString (ByteString)
import Network.LibP2P.Crypto.Key (KeyPair)

-- | Generate a new random Ed25519 key pair.
generateKeyPair :: IO KeyPair
generateKeyPair = error "Not yet implemented"

-- | Create an Ed25519 key pair from a 32-byte seed.
keyPairFromSeed :: ByteString -> Either String KeyPair
keyPairFromSeed = error "Not yet implemented"
