-- | Multiaddr: self-describing, composable network addresses.
--
-- A multiaddr is a binary-encoded, composable network address that describes
-- the entire protocol stack needed to reach a peer.
module Network.LibP2P.Multiaddr.Multiaddr
  ( Multiaddr (..)
  , fromText
  , toText
  , fromBytes
  , toBytes
  , encapsulate
  , protocols
  , splitP2P
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.LibP2P.Crypto.PeerId (PeerId (..))
import Network.LibP2P.Multiaddr.Codec
  ( decodeProtocols
  , encodeProtocols
  , protocolsToText
  , textToProtocols
  )
import Network.LibP2P.Multiaddr.Protocol (Protocol (..))

-- | A multiaddr is a list of protocol components.
newtype Multiaddr = Multiaddr [Protocol]
  deriving (Show, Eq)

-- | Parse a multiaddr from its text representation (e.g. "/ip4/127.0.0.1/tcp/4001").
fromText :: Text -> Either String Multiaddr
fromText t = Multiaddr <$> textToProtocols t

-- | Render a multiaddr as text.
toText :: Multiaddr -> Text
toText (Multiaddr ps) = protocolsToText ps

-- | Parse a multiaddr from binary format.
fromBytes :: ByteString -> Either String Multiaddr
fromBytes bs = Multiaddr <$> decodeProtocols bs

-- | Encode a multiaddr to binary format.
toBytes :: Multiaddr -> ByteString
toBytes (Multiaddr ps) = encodeProtocols ps

-- | Encapsulate: append another multiaddr's protocols.
encapsulate :: Multiaddr -> Multiaddr -> Multiaddr
encapsulate (Multiaddr a) (Multiaddr b) = Multiaddr (a <> b)

-- | Get the list of protocols in a multiaddr.
protocols :: Multiaddr -> [Protocol]
protocols (Multiaddr ps) = ps

-- | Split off the trailing /p2p/<peerId> component from a multiaddr.
-- Returns the transport address and the peer ID, or Nothing if the
-- multiaddr does not end with a /p2p/ component.
splitP2P :: Multiaddr -> Maybe (Multiaddr, PeerId)
splitP2P (Multiaddr ps) = case reverse ps of
  (P2P mhBytes : rest) -> Just (Multiaddr (reverse rest), PeerId mhBytes)
  _ -> Nothing
