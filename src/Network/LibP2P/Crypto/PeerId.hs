-- | Peer ID derivation from public keys.
--
-- A Peer ID is a multihash of the serialized PublicKey protobuf message.
-- Ed25519 keys (36 bytes serialized) use identity multihash.
-- Larger keys (RSA) use SHA-256 multihash.
module Network.LibP2P.Crypto.PeerId
  ( PeerId (..)
  , fromPublicKey
  , toBase58
  , fromBase58
  , peerIdBytes
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.LibP2P.Crypto.Key (PublicKey)

-- | A Peer ID is a multihash of the serialized public key.
newtype PeerId = PeerId ByteString
  deriving (Show, Eq, Ord)

-- | Derive a Peer ID from a public key.
fromPublicKey :: PublicKey -> PeerId
fromPublicKey = error "Not yet implemented"

-- | Encode a Peer ID as base58btc text.
toBase58 :: PeerId -> Text
toBase58 = error "Not yet implemented"

-- | Decode a Peer ID from base58btc text.
fromBase58 :: Text -> Either String PeerId
fromBase58 = error "Not yet implemented"

-- | Get the raw multihash bytes of a Peer ID.
peerIdBytes :: PeerId -> ByteString
peerIdBytes (PeerId bs) = bs
