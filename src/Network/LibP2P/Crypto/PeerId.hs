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
import Data.Word (Word8)
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

-- Base58btc (Bitcoin alphabet) encoding/decoding

base58Alphabet :: String
base58Alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

base58Encode :: [Word8] -> String
base58Encode bytes =
  let leadingZeros = length (takeWhile (== 0) bytes)
      n = foldl (\acc b -> acc * 256 + toInteger b) 0 bytes
      encoded = encodeN n
   in replicate leadingZeros '1' <> encoded
  where
    encodeN :: Integer -> String
    encodeN 0 = ""
    encodeN m =
      let (q, r) = m `divMod` 58
       in encodeN q <> [base58Alphabet !! fromIntegral r]

base58Decode :: String -> Maybe ByteString
base58Decode str =
  let leadingOnes = length (takeWhile (== '1') str)
   in do
        n <- decodeChars str
        let bytes = decodeN n
        Just $ BS.pack (replicate leadingOnes 0 <> bytes)
  where
    decodeChars :: String -> Maybe Integer
    decodeChars = foldl step (Just 0)
      where
        step Nothing _ = Nothing
        step (Just acc) c = case charIndex c of
          Nothing -> Nothing
          Just i -> Just (acc * 58 + toInteger i)

    charIndex :: Char -> Maybe Int
    charIndex c = lookup c (zip base58Alphabet [0 ..])

    decodeN :: Integer -> [Word8]
    decodeN 0 = []
    decodeN m =
      let (q, r) = m `divMod` 256
       in decodeN q <> [fromIntegral r]
