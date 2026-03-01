module Test.Network.LibP2P.Multiaddr.MultiaddrSpec (spec) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import Network.LibP2P.Core.Varint (encodeUvarint)
import Network.LibP2P.Crypto.PeerId (PeerId (..))
import Network.LibP2P.Multiaddr.Codec
import Network.LibP2P.Multiaddr.Multiaddr
import Network.LibP2P.Multiaddr.Protocol
import Test.Hspec

spec :: Spec
spec = do
  describe "Binary encoding" $ do
    it "encodes /ip4/127.0.0.1/tcp/4001 to correct bytes" $ do
      let ps = [IP4 0x7f000001, TCP 4001]
      -- 04 7f000001 06 0fa1
      encodeProtocols ps `shouldBe` BS.pack [0x04, 0x7f, 0x00, 0x00, 0x01, 0x06, 0x0f, 0xa1]

    it "encodes /ip4/198.51.100.0/udp/9090/quic-v1 to correct bytes" $ do
      let ps = [IP4 0xc6336400, UDP 9090, QuicV1]
      -- 04 c6336400 9102 2382 cc03
      encodeProtocols ps
        `shouldBe` BS.pack [0x04, 0xc6, 0x33, 0x64, 0x00, 0x91, 0x02, 0x23, 0x82, 0xcc, 0x03]

    it "encodes zero-address protocols (ws, wss, p2p-circuit)" $ do
      let ps = [QuicV1, WS, P2PCircuit]
      let encoded = encodeProtocols ps
      -- Each is just its varint code, no address bytes
      BS.length encoded `shouldSatisfy` (> 0)

  describe "Binary decoding" $ do
    it "decodes /ip4/127.0.0.1/tcp/4001 from bytes" $ do
      let bytes = BS.pack [0x04, 0x7f, 0x00, 0x00, 0x01, 0x06, 0x0f, 0xa1]
      decodeProtocols bytes `shouldBe` Right [IP4 0x7f000001, TCP 4001]

    it "decodes /ip4/198.51.100.0/udp/9090/quic-v1 from bytes" $ do
      let bytes = BS.pack [0x04, 0xc6, 0x33, 0x64, 0x00, 0x91, 0x02, 0x23, 0x82, 0xcc, 0x03]
      decodeProtocols bytes `shouldBe` Right [IP4 0xc6336400, UDP 9090, QuicV1]

    it "fails on empty input" $
      decodeProtocols BS.empty `shouldBe` Right []

    it "fails on unknown protocol code" $ do
      -- 0xff 0x7f = varint 16383, unknown protocol
      let bytes = BS.pack [0xff, 0x7f]
      decodeProtocols bytes `shouldSatisfy` isLeft

  describe "Binary round-trip" $ do
    it "decode(encode(ps)) == ps for /ip4/tcp" $ do
      let ps = [IP4 0x0a000001, TCP 8080]
      decodeProtocols (encodeProtocols ps) `shouldBe` Right ps

    it "decode(encode(ps)) == ps for /ip4/udp/quic-v1" $ do
      let ps = [IP4 0xc0a80001, UDP 443, QuicV1]
      decodeProtocols (encodeProtocols ps) `shouldBe` Right ps

    it "decode(encode(ps)) == ps for p2p with multihash" $ do
      let peerIdBytes = BS.pack $ [0x00, 0x24, 0x08, 0x01, 0x12, 0x20] <> replicate 32 0xAB
      let ps = [IP4 0x7f000001, TCP 4001, P2P peerIdBytes]
      decodeProtocols (encodeProtocols ps) `shouldBe` Right ps

  describe "Text encoding" $ do
    it "renders /ip4/127.0.0.1/tcp/4001" $ do
      let ps = [IP4 0x7f000001, TCP 4001]
      protocolsToText ps `shouldBe` ("/ip4/127.0.0.1/tcp/4001" :: Text)

    it "renders /ip4/198.51.100.0/udp/9090/quic-v1" $ do
      let ps = [IP4 0xc6336400, UDP 9090, QuicV1]
      protocolsToText ps `shouldBe` ("/ip4/198.51.100.0/udp/9090/quic-v1" :: Text)

    it "renders dns4 protocols" $ do
      let ps = [DNS4 "example.com", TCP 443, WSS]
      protocolsToText ps `shouldBe` ("/dns4/example.com/tcp/443/wss" :: Text)

  describe "Text parsing" $ do
    it "parses /ip4/127.0.0.1/tcp/4001" $ do
      textToProtocols "/ip4/127.0.0.1/tcp/4001"
        `shouldBe` Right [IP4 0x7f000001, TCP 4001]

    it "parses /ip4/198.51.100.0/udp/9090/quic-v1" $ do
      textToProtocols "/ip4/198.51.100.0/udp/9090/quic-v1"
        `shouldBe` Right [IP4 0xc6336400, UDP 9090, QuicV1]

    it "parses /dns4/example.com/tcp/443/wss" $ do
      textToProtocols "/dns4/example.com/tcp/443/wss"
        `shouldBe` Right [DNS4 "example.com", TCP 443, WSS]

    it "fails on invalid protocol name" $
      textToProtocols "/invalid/foo" `shouldSatisfy` isLeft

    it "fails on missing address for ip4" $
      textToProtocols "/ip4" `shouldSatisfy` isLeft

  describe "UTF-8 multibyte DNS names" $ do
    it "binary round-trip for multibyte UTF-8 DNS name" $ do
      -- Japanese domain: テスト.jp (3-byte UTF-8 chars)
      let ps = [DNS4 "\12486\12473\12488.jp", TCP 443]
      decodeProtocols (encodeProtocols ps) `shouldBe` Right ps

    it "binary round-trip for emoji DNS name" $ do
      -- Emoji domain (4-byte UTF-8 char)
      let ps = [DNS "\128640.example.com", TCP 80]
      decodeProtocols (encodeProtocols ps) `shouldBe` Right ps

    it "text round-trip for multibyte UTF-8 DNS name" $ do
      let ps = [DNS4 "\12486\12473\12488.jp", TCP 443]
      textToProtocols (protocolsToText ps) `shouldBe` Right ps

  describe "IPv6 rendering and parsing" $ do
    it "renders /ip6/::1 correctly" $ do
      -- ::1 = 15 zero bytes followed by 0x01
      let loopback = BS.pack (replicate 15 0x00 <> [0x01])
      protocolsToText [IP6 loopback] `shouldBe` "/ip6/::1"

    it "renders /ip6/fe80::1 correctly" $ do
      -- fe80::1 = fe80 0000 0000 0000 0000 0000 0000 0001
      let linkLocal = BS.pack [0xfe, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                               , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]
      protocolsToText [IP6 linkLocal] `shouldBe` "/ip6/fe80::1"

    it "renders /ip6/2001:db8::1 correctly" $ do
      let addr = BS.pack [0x20, 0x01, 0x0d, 0xb8, 0x00, 0x00, 0x00, 0x00
                          , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]
      protocolsToText [IP6 addr] `shouldBe` "/ip6/2001:db8::1"

    it "parses /ip6/::1 correctly" $ do
      let expected = BS.pack (replicate 15 0x00 <> [0x01])
      textToProtocols "/ip6/::1" `shouldBe` Right [IP6 expected]

    it "parses /ip6/fe80::1 correctly" $ do
      let expected = BS.pack [0xfe, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                              , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]
      textToProtocols "/ip6/fe80::1" `shouldBe` Right [IP6 expected]

    it "IPv6 text round-trip" $ do
      let addr = BS.pack [0x20, 0x01, 0x0d, 0xb8, 0x00, 0x00, 0x00, 0x00
                          , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]
      let ps = [IP6 addr, TCP 4001]
      textToProtocols (protocolsToText ps) `shouldBe` Right ps

  describe "Text round-trip" $ do
    it "textToProtocols(protocolsToText(ps)) == ps" $ do
      let ps = [IP4 0x0a000001, TCP 8080]
      textToProtocols (protocolsToText ps) `shouldBe` Right ps

  describe "Multiaddr operations" $ do
    it "encapsulate combines two multiaddrs" $ do
      let ma1 = Multiaddr [IP4 0x7f000001, TCP 4001]
      let ma2 = Multiaddr [WS]
      protocols (encapsulate ma1 ma2) `shouldBe` [IP4 0x7f000001, TCP 4001, WS]

    it "fromText and toText round-trip" $ do
      let input = "/ip4/127.0.0.1/tcp/4001" :: Text
      case fromText input of
        Right ma -> toText ma `shouldBe` input
        Left err -> expectationFailure err

  describe "splitP2P" $ do
    it "splits /ip4/.../tcp/.../p2p/<id> into transport addr and PeerId" $ do
      let peerIdMH = BS.pack $ [0x00, 0x24, 0x08, 0x01, 0x12, 0x20] <> replicate 32 0xAB
      let ma = Multiaddr [IP4 0x7f000001, TCP 4001, P2P peerIdMH]
      case splitP2P ma of
        Nothing -> expectationFailure "splitP2P returned Nothing"
        Just (transport, pid) -> do
          transport `shouldBe` Multiaddr [IP4 0x7f000001, TCP 4001]
          pid `shouldBe` PeerId peerIdMH

    it "returns Nothing when multiaddr has no /p2p/ suffix" $ do
      let ma = Multiaddr [IP4 0x7f000001, TCP 4001]
      splitP2P ma `shouldBe` Nothing

    it "returns Nothing for empty multiaddr" $ do
      let ma = Multiaddr []
      splitP2P ma `shouldBe` Nothing

    it "roundtrips: encapsulate transport (Multiaddr [P2P id]) == original" $ do
      let peerIdMH = BS.pack $ [0x00, 0x24, 0x08, 0x01, 0x12, 0x20] <> replicate 32 0xCC
      let original = Multiaddr [IP4 0xc0a80001, TCP 9090, P2P peerIdMH]
      case splitP2P original of
        Nothing -> expectationFailure "splitP2P returned Nothing"
        Just (transport, PeerId mhBytes) ->
          encapsulate transport (Multiaddr [P2P mhBytes]) `shouldBe` original

  describe "P2P validation" $ do
    it "text /p2p/INVALID rejects non-base58 input" $ do
      textToProtocols "/p2p/INVALID!!!" `shouldSatisfy` isLeft

    it "text /p2p/ with invalid multihash rejects" $ do
      -- base58-encode bytes that aren't a valid multihash
      textToProtocols "/p2p/1111" `shouldSatisfy` isLeft

    it "binary P2P with invalid multihash rejects" $ do
      -- Protocol code 421 = P2P, followed by invalid multihash bytes
      let invalidMh = BS.pack [0xDE, 0xAD]  -- unknown hash code 0xDE
      let encoded = encodeUvarint 421 <> encodeUvarint (fromIntegral (BS.length invalidMh)) <> invalidMh
      decodeProtocols encoded `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
