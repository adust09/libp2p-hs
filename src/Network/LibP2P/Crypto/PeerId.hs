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
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Network.LibP2P.Core.Base58 (base58Decode, base58Encode)
import Network.LibP2P.Core.Multihash (HashFunction (..), encodeMultihash)
import Network.LibP2P.Crypto.Key (PublicKey)
import Network.LibP2P.Crypto.Protobuf (encodePublicKey)

-- | A Peer ID is a multihash of the serialized public key.
newtype PeerId = PeerId ByteString
  deriving (Show, Eq, Ord)

-- | Maximum serialized size for identity multihash.
maxInlineKeyLength :: Int
maxInlineKeyLength = 42

-- | Derive a Peer ID from a public key.
fromPublicKey :: PublicKey -> PeerId
fromPublicKey pk =
  let serialized = encodePublicKey pk
      mh =
        if BS.length serialized <= maxInlineKeyLength
          then encodeMultihash Identity serialized
          else encodeMultihash SHA256 serialized
   in PeerId mh

-- | Encode a Peer ID as base58btc text.
toBase58 :: PeerId -> Text
toBase58 (PeerId bs) = T.pack $ base58Encode (BS.unpack bs)

-- | Decode a Peer ID from base58btc text.
fromBase58 :: Text -> Either String PeerId
fromBase58 t = case base58Decode (T.unpack t) of
  Just bs -> Right (PeerId bs)
  Nothing -> Left "fromBase58: invalid base58 encoding"

-- | Get the raw multihash bytes of a Peer ID.
peerIdBytes :: PeerId -> ByteString
peerIdBytes (PeerId bs) = bs
