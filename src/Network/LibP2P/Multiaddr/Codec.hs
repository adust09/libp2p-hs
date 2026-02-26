-- | Binary and text encoding/decoding for multiaddr.
module Network.LibP2P.Multiaddr.Codec
  ( encodeProtocols
  , decodeProtocols
  , protocolsToText
  , textToProtocols
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.LibP2P.Multiaddr.Protocol (Protocol)

-- | Encode a list of protocols to binary multiaddr format.
encodeProtocols :: [Protocol] -> ByteString
encodeProtocols = error "Not yet implemented"

-- | Decode binary multiaddr format to a list of protocols.
decodeProtocols :: ByteString -> Either String [Protocol]
decodeProtocols = error "Not yet implemented"

-- | Convert a list of protocols to human-readable text form.
protocolsToText :: [Protocol] -> Text
protocolsToText = error "Not yet implemented"

-- | Parse human-readable text form to a list of protocols.
textToProtocols :: Text -> Either String [Protocol]
textToProtocols = error "Not yet implemented"
