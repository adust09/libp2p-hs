-- | Multiaddr: self-describing, composable network addresses.
--
-- A multiaddr is a binary-encoded, composable network address that describes
-- the entire protocol stack needed to reach a peer.
module LibP2P.Multiaddr
  ( Multiaddr (..)
  , fromText
  , toText
  , fromBytes
  , toBytes
  , encapsulate
  , decapsulate
  , protocols
  , splitP2P
  , isPublicAddr
  , isRelayedAddr
  ) where

import Data.Bits (shiftL, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (isSuffixOf)
import qualified Data.Text as T
import Data.Word (Word32, Word8)
import Data.List (isPrefixOf, tails)
import Data.Text (Text)
import LibP2P.Crypto.PeerId (PeerId (..))
import LibP2P.Multiaddr.Codec
  ( decodeProtocols
  , encodeProtocols
  , protocolsToText
  , textToProtocols
  )
import LibP2P.Multiaddr.Protocol (Protocol (..))

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

-- | Decapsulate: remove the last occurrence of the given suffix multiaddr
-- and everything after it (specs/addressing). Returns the original
-- multiaddr unchanged when the suffix does not occur. Decapsulating the
-- empty multiaddr is a no-op, making 'decapsulate' a left inverse of
-- 'encapsulate': @decapsulate (encapsulate a b) b == a@ for non-empty b.
decapsulate :: Multiaddr -> Multiaddr -> Multiaddr
decapsulate ma@(Multiaddr a) (Multiaddr b)
  | null b = ma
  | otherwise =
      case [i | (i, suffix) <- zip [0 :: Int ..] (tails a), b `isPrefixOf` suffix] of
        [] -> ma
        matches -> Multiaddr (take (last matches) a)

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

-- Address classification

-- | Whether the address goes through a circuit relay, i.e. contains a
-- @/p2p-circuit@ component.
isRelayedAddr :: Multiaddr -> Bool
isRelayedAddr (Multiaddr ps) = P2PCircuit `elem` ps


-- | Whether the address is publicly routable.
--
-- Mirrors go-multiaddr's @manet.IsPublicAddr@ (net/private.go), which
-- the DCUtR unilateral-upgrade check depends on: a peer is only worth
-- dialling directly if it advertises an address that can be reached.
--
-- IPv4 is classified by exclusion (anything outside the private and
-- unroutable ranges is public); IPv6 by inclusion (only the global
-- unicast allocation, minus documentation and multicast, plus the NAT64
-- prefixes). A DNS name is public unless it is a special-use domain.
-- An address with no IP or DNS component is not public.
isPublicAddr :: Multiaddr -> Bool
isPublicAddr (Multiaddr ps) = any componentIsPublic ps
  where
    componentIsPublic (IP4 w)     = publicIPv4 w
    componentIsPublic (IP6 bs)    = publicIPv6 bs
    componentIsPublic (DNS h)     = publicDomain h
    componentIsPublic (DNS4 h)    = publicDomain h
    componentIsPublic (DNS6 h)    = publicDomain h
    componentIsPublic (DNSAddr h) = publicDomain h
    componentIsPublic _           = False

-- | IPv4 is public unless it falls in a private or unroutable range.
publicIPv4 :: Word32 -> Bool
publicIPv4 w = not (any (inRange4 w) (privateRanges4 ++ unroutableRanges4))

-- | Private IPv4 ranges: loopback, RFC1918, CGNAT and link-local.
privateRanges4 :: [(Word32, Int)]
privateRanges4 =
  [ (0x7F000000, 8)   -- 127.0.0.0/8    localhost
  , (0x0A000000, 8)   -- 10.0.0.0/8
  , (0x64400000, 10)  -- 100.64.0.0/10  CGNAT
  , (0xAC100000, 12)  -- 172.16.0.0/12
  , (0xC0A80000, 16)  -- 192.168.0.0/16
  , (0xA9FE0000, 16)  -- 169.254.0.0/16 link local
  ]

-- | Well-known unroutable IPv4 ranges.
unroutableRanges4 :: [(Word32, Int)]
unroutableRanges4 =
  [ (0x00000000, 8)   -- 0.0.0.0/8
  , (0xC0000000, 26)  -- 192.0.0.0/26
  , (0xC0000200, 24)  -- 192.0.2.0/24
  , (0xC0586300, 24)  -- 192.88.99.0/24
  , (0xC6120000, 15)  -- 198.18.0.0/15
  , (0xC6336400, 24)  -- 198.51.100.0/24
  , (0xCB007100, 24)  -- 203.0.113.0/24
  , (0xE0000000, 4)   -- 224.0.0.0/4    multicast
  , (0xF0000000, 4)   -- 240.0.0.0/4
  , (0xFFFFFFFF, 32)  -- 255.255.255.255/32
  ]

-- | Whether an IPv4 address falls inside a CIDR block.
inRange4 :: Word32 -> (Word32, Int) -> Bool
inRange4 addr (base, bits) = addr .&. mask == base .&. mask
  where
    mask | bits <= 0  = 0
         | bits >= 32 = 0xFFFFFFFF
         | otherwise  = complementLow (32 - bits)
    complementLow n = 0xFFFFFFFF `shiftL` n .&. 0xFFFFFFFF

-- | IPv6 is public only inside the global unicast allocation (minus the
-- documentation prefix) or inside a NAT64 prefix.
--
-- The NAT64 well-known prefix (RFC 6052) can only reference a public
-- IPv4 address. The local-use prefix (RFC 8215) may reference a private
-- one, but the translation is left to the operator, so go-multiaddr
-- counts both as public on the grounds that a false negative here is
-- worse than a false positive. This follows that choice.
publicIPv6 :: ByteString -> Bool
publicIPv6 bs
  | BS.length bs /= 16 = False
  | globalUnicast && not documentation = True
  | otherwise = nat64
  where
    globalUnicast = inRange6 bs (BS.pack [0x20, 0x00], 3)
    documentation = inRange6 bs (BS.pack [0x20, 0x01, 0x0D, 0xB8], 32)
    nat64 = inRange6 bs (BS.pack [0x00, 0x64, 0xFF, 0x9B, 0x00, 0x00], 96)
              || inRange6 bs (BS.pack [0x00, 0x64, 0xFF, 0x9B, 0x00, 0x01], 48)

-- | Whether an IPv6 address falls inside a CIDR block, given the
-- block's leading bytes and its prefix length.
inRange6 :: ByteString -> (ByteString, Int) -> Bool
inRange6 addr (prefix, bits) =
  BS.length padded >= wholeBytes
    && BS.take wholeBytes addr == BS.take wholeBytes padded
    && remainderMatches
  where
    padded = prefix <> BS.replicate (16 - BS.length prefix) 0
    (wholeBytes, spare) = bits `divMod` 8
    remainderMatches
      | spare == 0 = True
      | otherwise  = maskByte (BS.index addr wholeBytes) == maskByte (BS.index padded wholeBytes)
    maskByte :: Word8 -> Word8
    maskByte b = b .&. (0xFF `shiftL` (8 - spare) .&. 0xFF)

-- | A DNS name is public unless it is a special-use domain that either
-- does not resolve or is reserved for private use.
publicDomain :: T.Text -> Bool
publicDomain host = not (any (`isSuffixOf` lowered) specialUseDomains)
  where
    lowered = T.unpack (T.toLower host)

-- | Special-use domains that never denote a publicly routable host.
specialUseDomains :: [String]
specialUseDomains =
  [ ".localhost"
  , ".in-addr.arpa"
  , ".ip6.arpa"
  , ".invalid"
  , ".home.arpa"
  , ".local"
  , ".test"
  ]
