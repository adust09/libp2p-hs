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
  , parsePeerId
  , toCIDv1
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base58 as B58
import Data.ByteArray.Encoding (Base (Base32), convertFromBase, convertToBase)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char (toLower, toUpper)
import Data.Word (Word64)
import Network.LibP2P.Core.Multihash (HashFunction (..), encodeMultihash, validateMultihash)
import Network.LibP2P.Core.Varint (encodeUvarint, decodeUvarint)
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
toBase58 (PeerId bs) = TE.decodeUtf8 (B58.encode bs)

-- | Decode a Peer ID from base58btc text.
-- Validates that decoded bytes are a well-formed multihash.
fromBase58 :: Text -> Either String PeerId
fromBase58 t = case B58.decode (TE.encodeUtf8 t) of
  Nothing -> Left "fromBase58: invalid base58 encoding"
  Just bs -> do
    _ <- validateMultihash bs
    Right (PeerId bs)

-- | Get the raw multihash bytes of a Peer ID.
peerIdBytes :: PeerId -> ByteString
peerIdBytes (PeerId bs) = bs

-- | Parse a Peer ID from text, accepting both base58btc and CIDv1 (base32lower) formats.
-- CIDv1 format: 'b' prefix + base32lower(0x01 || 0x72 || multihash)
parsePeerId :: Text -> Either String PeerId
parsePeerId t
  | T.null t = Left "parsePeerId: empty input"
  | T.head t == 'b' = fromCIDv1 t
  | otherwise = fromBase58 t

-- | Encode a Peer ID as CIDv1 text (base32lower, no padding).
-- Format: 'b' + base32lower(varint(1) + varint(0x72) + multihash_bytes)
toCIDv1 :: PeerId -> Text
toCIDv1 (PeerId mhBytes) =
  let cidVersion = encodeUvarint (1 :: Word64)
      codec = encodeUvarint (0x72 :: Word64)  -- libp2p-key
      cidBytes = cidVersion <> codec <> mhBytes
      base32Upper = convertToBase Base32 cidBytes :: ByteString
      -- Strip padding '=' and convert to lowercase
      base32NoPad = BS.filter (/= 0x3D) base32Upper  -- 0x3D = '='
      base32Lower = BS.map (\w -> if w >= 0x41 && w <= 0x5A then w + 32 else w) base32NoPad
  in "b" <> TE.decodeUtf8 base32Lower

-- | Decode a Peer ID from CIDv1 text.
fromCIDv1 :: Text -> Either String PeerId
fromCIDv1 t
  | T.null t = Left "fromCIDv1: empty input"
  | T.head t /= 'b' = Left "fromCIDv1: expected 'b' multibase prefix"
  | otherwise = do
      let base32Text = T.drop 1 t  -- strip 'b' prefix
          -- Convert to uppercase for standard Base32 decoding, add padding
          upperText = T.map toUpper base32Text
          padLen = case T.length upperText `mod` 8 of
                     0 -> 0
                     n -> 8 - n
          paddedText = upperText <> T.replicate padLen "="
      cidBytes <- case convertFromBase Base32 (TE.encodeUtf8 paddedText) :: Either String ByteString of
        Left err -> Left $ "fromCIDv1: base32 decode error: " <> err
        Right bs -> Right bs
      -- Parse CID: version + codec + multihash
      (version, rest1) <- decodeUvarint cidBytes
      if version /= (1 :: Word64)
        then Left $ "fromCIDv1: expected CID version 1, got " <> show version
        else do
          (codec, rest2) <- decodeUvarint rest1
          if codec /= (0x72 :: Word64)
            then Left $ "fromCIDv1: expected libp2p-key codec 0x72, got 0x" <> showHexW64 codec
            else do
              _ <- validateMultihash rest2
              Right (PeerId rest2)

-- | Show a Word64 as hex.
showHexW64 :: Word64 -> String
showHexW64 = go []
  where
    go acc 0 | null acc = "0"
             | otherwise = acc
    go acc n = go (hexDigit (fromIntegral (n `mod` 16)) : acc) (n `div` 16)
    hexDigit :: Int -> Char
    hexDigit d
      | d < 10 = toEnum (d + fromEnum '0')
      | otherwise = toEnum (d - 10 + fromEnum 'a')
