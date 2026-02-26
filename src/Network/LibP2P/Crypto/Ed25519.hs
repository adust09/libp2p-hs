-- | Ed25519 key operations using crypton.
module Network.LibP2P.Crypto.Ed25519
  ( generateKeyPair
  , keyPairFromSeed
  ) where

import qualified Crypto.Error as CE
import qualified Crypto.PubKey.Ed25519 as Ed
import Crypto.Random (getRandomBytes)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.LibP2P.Crypto.Key

-- | Generate a new random Ed25519 key pair.
generateKeyPair :: IO KeyPair
generateKeyPair = do
  seed <- getRandomBytes 32 :: IO ByteString
  case keyPairFromSeed seed of
    Right kp -> pure kp
    Left err -> error $ "generateKeyPair: " <> err

-- | Create an Ed25519 key pair from a 32-byte seed.
keyPairFromSeed :: ByteString -> Either String KeyPair
keyPairFromSeed seed
  | BS.length seed /= 32 = Left "keyPairFromSeed: seed must be 32 bytes"
  | otherwise =
      case CE.eitherCryptoError (Ed.secretKey seed) of
        Left err -> Left $ "keyPairFromSeed: " <> show err
        Right sk ->
          let pk = Ed.toPublic sk
           in Right $
                KeyPair
                  { kpPublic = PublicKey Ed25519 (convert pk)
                  , kpPrivate = PrivateKey Ed25519 (convert sk)
                  }
