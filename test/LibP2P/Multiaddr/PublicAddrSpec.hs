-- | Tests for public-address classification (issue #258).
--
-- The table mirrors go-multiaddr's @manet.IsPublicAddr@ (net/private.go),
-- which is what the DCUtR unilateral-upgrade check needs: a peer is only
-- worth dialling directly if it advertises a reachable address.
module LibP2P.Multiaddr.PublicAddrSpec (spec) where

import qualified Data.ByteString as BS
import Data.Word (Word32, Word8)
import LibP2P.Multiaddr (Multiaddr (..), isPublicAddr)
import LibP2P.Multiaddr.Protocol (Protocol (..))
import Test.Hspec

ip4 :: Word32 -> Multiaddr
ip4 w = Multiaddr [IP4 w, TCP 4001]

-- | Build an IPv6 multiaddr from the leading bytes, zero-padded.
ip6 :: [Word8] -> Multiaddr
ip6 leading = Multiaddr [IP6 (BS.pack leading <> BS.replicate (16 - length leading) 0), TCP 4001]

spec :: Spec
spec = describe "isPublicAddr" $ do
  describe "IPv4" $ do
    it "accepts globally routable addresses" $ do
      isPublicAddr (ip4 0x08080808) `shouldBe` True   -- 8.8.8.8
      isPublicAddr (ip4 0x01010101) `shouldBe` True   -- 1.1.1.1
      isPublicAddr (ip4 0x2D2D2D2D) `shouldBe` True   -- 45.45.45.45

    it "rejects loopback, RFC1918, CGNAT and link-local" $ do
      isPublicAddr (ip4 0x7F000001) `shouldBe` False  -- 127.0.0.1
      isPublicAddr (ip4 0x0A000001) `shouldBe` False  -- 10.0.0.1
      isPublicAddr (ip4 0xAC100001) `shouldBe` False  -- 172.16.0.1
      isPublicAddr (ip4 0xAC1F0001) `shouldBe` False  -- 172.31.0.1
      isPublicAddr (ip4 0xC0A80001) `shouldBe` False  -- 192.168.0.1
      isPublicAddr (ip4 0x64400001) `shouldBe` False  -- 100.64.0.1  CGNAT
      isPublicAddr (ip4 0xA9FE0001) `shouldBe` False  -- 169.254.0.1 link-local

    it "accepts addresses just outside the private ranges" $ do
      isPublicAddr (ip4 0xAC0FFFFF) `shouldBe` True   -- 172.15.255.255
      isPublicAddr (ip4 0xAC200000) `shouldBe` True   -- 172.32.0.0
      isPublicAddr (ip4 0x643FFFFF) `shouldBe` True   -- 100.63.255.255
      isPublicAddr (ip4 0x64800000) `shouldBe` True   -- 100.128.0.0

    it "rejects the unroutable ranges" $ do
      isPublicAddr (ip4 0x00000000) `shouldBe` False  -- 0.0.0.0
      isPublicAddr (ip4 0xC0000001) `shouldBe` False  -- 192.0.0.1
      isPublicAddr (ip4 0xC0000201) `shouldBe` False  -- 192.0.2.1
      isPublicAddr (ip4 0xC0586301) `shouldBe` False  -- 192.88.99.1
      isPublicAddr (ip4 0xC6120001) `shouldBe` False  -- 198.18.0.1
      isPublicAddr (ip4 0xC6336401) `shouldBe` False  -- 198.51.100.1
      isPublicAddr (ip4 0xCB007101) `shouldBe` False  -- 203.0.113.1
      isPublicAddr (ip4 0xE0000001) `shouldBe` False  -- 224.0.0.1  multicast
      isPublicAddr (ip4 0xF0000001) `shouldBe` False  -- 240.0.0.1
      isPublicAddr (ip4 0xFFFFFFFF) `shouldBe` False  -- 255.255.255.255

  describe "IPv6" $ do
    it "accepts the global unicast allocation" $ do
      isPublicAddr (ip6 [0x20, 0x01, 0x4A, 0x60]) `shouldBe` True
      isPublicAddr (ip6 [0x2A, 0x00]) `shouldBe` True
      isPublicAddr (ip6 [0x3F, 0xFF]) `shouldBe` True

    it "rejects everything outside global unicast" $ do
      isPublicAddr (ip6 [0x00]) `shouldBe` False                      -- ::
      isPublicAddr (ip6 [0xFC, 0x00]) `shouldBe` False                -- ULA
      isPublicAddr (ip6 [0xFD, 0x00]) `shouldBe` False                -- ULA
      isPublicAddr (ip6 [0xFE, 0x80]) `shouldBe` False                -- link-local
      isPublicAddr (ip6 [0xFF, 0x02]) `shouldBe` False                -- multicast
      isPublicAddr (ip6 [0x1F, 0xFF]) `shouldBe` False                -- below 2000::/3

    it "rejects loopback ::1" $
      isPublicAddr (Multiaddr [IP6 (BS.replicate 15 0 <> BS.singleton 1), TCP 4001])
        `shouldBe` False

    it "rejects the documentation prefix inside global unicast" $
      isPublicAddr (ip6 [0x20, 0x01, 0x0D, 0xB8]) `shouldBe` False

    it "accepts the NAT64 prefixes" $ do
      isPublicAddr (ip6 [0x00, 0x64, 0xFF, 0x9B, 0x00, 0x00]) `shouldBe` True  -- RFC 6052
      isPublicAddr (ip6 [0x00, 0x64, 0xFF, 0x9B, 0x00, 0x01]) `shouldBe` True  -- RFC 8215

  describe "DNS" $ do
    it "accepts ordinary hostnames" $ do
      isPublicAddr (Multiaddr [DNS "example.com", TCP 443]) `shouldBe` True
      isPublicAddr (Multiaddr [DNS4 "bootstrap.libp2p.io", TCP 443]) `shouldBe` True
      isPublicAddr (Multiaddr [DNSAddr "bootstrap.libp2p.io"]) `shouldBe` True

    it "rejects special-use domains" $ do
      isPublicAddr (Multiaddr [DNS "host.localhost", TCP 443]) `shouldBe` False
      isPublicAddr (Multiaddr [DNS "printer.local", TCP 443]) `shouldBe` False
      isPublicAddr (Multiaddr [DNS "router.home.arpa", TCP 443]) `shouldBe` False
      isPublicAddr (Multiaddr [DNS "thing.test", TCP 443]) `shouldBe` False
      isPublicAddr (Multiaddr [DNS "nope.invalid", TCP 443]) `shouldBe` False
      isPublicAddr (Multiaddr [DNS6 "1.0.0.127.in-addr.arpa", TCP 443]) `shouldBe` False

    it "is case-insensitive about the domain suffix" $
      isPublicAddr (Multiaddr [DNS "Printer.LOCAL", TCP 443]) `shouldBe` False

  describe "addresses with no IP or DNS component" $
    it "are not public" $ do
      isPublicAddr (Multiaddr [P2PCircuit]) `shouldBe` False
      isPublicAddr (Multiaddr []) `shouldBe` False

  describe "relayed addresses" $
    it "classify by their transport component, so the caller must filter circuits itself" $
      -- /ip4/1.2.3.4/tcp/4001/p2p/<relay>/p2p-circuit is 'public' by IP;
      -- DCUtR drops relayed addresses before applying this predicate.
      isPublicAddr (Multiaddr [IP4 0x01020304, TCP 4001, P2PCircuit]) `shouldBe` True
