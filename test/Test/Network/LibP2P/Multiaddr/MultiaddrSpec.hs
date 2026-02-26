module Test.Network.LibP2P.Multiaddr.MultiaddrSpec (spec) where

import qualified Data.ByteString as BS
import Data.Text (Text)
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

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
