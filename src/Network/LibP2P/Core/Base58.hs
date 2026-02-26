-- | Base58btc (Bitcoin alphabet) encoding and decoding.
--
-- Used for Peer ID display in multiaddr text form and PeerId module.
module Network.LibP2P.Core.Base58
  ( base58Encode
  , base58Decode
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

-- | Base58btc alphabet (Bitcoin variant).
base58Alphabet :: String
base58Alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

-- | Encode a ByteString to base58btc String.
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

-- | Decode a base58btc String to ByteString.
-- Returns Nothing on invalid characters.
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
