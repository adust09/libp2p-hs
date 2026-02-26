-- | Deterministic protobuf encoding for libp2p PublicKey/PrivateKey messages.
--
-- Hand-rolled encoding to guarantee deterministic output:
-- - Minimal varint encoding
-- - Fields in field number order (Type=1, Data=2)
-- - All fields present, no extras
module Network.LibP2P.Crypto.Protobuf
  ( encodePublicKey
  , decodePublicKey
  ) where

import Data.ByteString (ByteString)
import Network.LibP2P.Crypto.Key (PublicKey)

-- | Deterministic protobuf encoding of a PublicKey message.
encodePublicKey :: PublicKey -> ByteString
encodePublicKey = error "Not yet implemented"

-- | Decode a protobuf-encoded PublicKey message.
decodePublicKey :: ByteString -> Either String PublicKey
decodePublicKey = error "Not yet implemented"
