module LibP2P.Transport.TCPSpec (spec) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import LibP2P.Multiaddr (Multiaddr (..), encapsulate, fromText)
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import LibP2P.Transport.TCP (multiaddrToHostPort, newTCPTransport)
import LibP2P.Transport
import Test.Hspec

spec :: Spec
spec = do
  describe "multiaddrToHostPort" $ do
    it "parses /ip4/127.0.0.1/tcp/8080 -> (\"127.0.0.1\", \"8080\")" $ do
      let Right addr = fromText "/ip4/127.0.0.1/tcp/8080"
      multiaddrToHostPort addr `shouldBe` Right ("127.0.0.1", "8080")

    it "parses /ip6/::1/tcp/8080 -> (\"::1\", \"8080\")" $ do
      let Right addr = fromText "/ip6/::1/tcp/8080"
      multiaddrToHostPort addr `shouldBe` Right ("::1", "8080")

    it "ignores a trailing /p2p component" $ do
      let addr = Multiaddr [IP4 0x7f000001, TCP 8080, P2P testPeerIdMH]
      multiaddrToHostPort addr `shouldBe` Right ("127.0.0.1", "8080")

  describe "transportCanDial" $ do
    it "returns True for /ip4/127.0.0.1/tcp/4001" $ do
      transport <- newTCPTransport
      let Right addr = fromText "/ip4/127.0.0.1/tcp/4001"
      transportCanDial transport addr `shouldBe` True

    it "returns True for a TCP address with a trailing /p2p component" $ do
      -- Identify- and DHT-learned addresses carry the /p2p suffix; the
      -- transport must still recognise them as dialable.
      transport <- newTCPTransport
      let addr = Multiaddr [IP4 0x7f000001, TCP 4001, P2P testPeerIdMH]
      transportCanDial transport addr `shouldBe` True

    it "returns False for a /p2p component alone" $ do
      transport <- newTCPTransport
      transportCanDial transport (Multiaddr [P2P testPeerIdMH]) `shouldBe` False

  describe "Loopback" $ do
    it "listen on /ip4/127.0.0.1/tcp/0, dial, exchange data" $ do
      transport <- newTCPTransport
      let Right listenAddr = fromText "/ip4/127.0.0.1/tcp/0"
      listener <- transportListen transport listenAddr
      let boundAddr = listenerAddr listener
      (serverConn, clientConn) <-
        concurrently
          (listenerAccept listener)
          (transportDial transport boundAddr)
      -- Exchange data
      let clientIO = rcStreamIO clientConn
          serverIO = rcStreamIO serverConn
      streamWrite clientIO "hello"
      received <- BS.pack <$> mapM (const (streamReadByte serverIO)) [1 :: Int .. 5]
      received `shouldBe` "hello"
      streamWrite serverIO "world"
      received2 <- BS.pack <$> mapM (const (streamReadByte clientIO)) [1 :: Int .. 5]
      received2 `shouldBe` "world"
      rcClose clientConn
      rcClose serverConn
      listenerClose listener

    it "dials an address with a trailing /p2p component (suffix stripped before connect)" $ do
      transport <- newTCPTransport
      let Right listenAddr = fromText "/ip4/127.0.0.1/tcp/0"
      listener <- transportListen transport listenAddr
      let boundAddr = listenerAddr listener
          dialAddr = encapsulate boundAddr (Multiaddr [P2P testPeerIdMH])
      (serverConn, clientConn) <-
        concurrently
          (listenerAccept listener)
          (transportDial transport dialAddr)
      -- The connection works and the remote addr keeps the /p2p suffix
      streamWrite (rcStreamIO clientConn) "x"
      b <- streamReadByte (rcStreamIO serverConn)
      BS.singleton b `shouldBe` "x"
      rcRemoteAddr clientConn `shouldBe` dialAddr
      rcClose clientConn
      rcClose serverConn
      listenerClose listener

  describe "Connection close" $ do
    it "rcClose cleanly closes socket (subsequent read returns error)" $ do
      transport <- newTCPTransport
      let Right listenAddr = fromText "/ip4/127.0.0.1/tcp/0"
      listener <- transportListen transport listenAddr
      let boundAddr = listenerAddr listener
      (serverConn, clientConn) <-
        concurrently
          (listenerAccept listener)
          (transportDial transport boundAddr)
      rcClose clientConn
      result <- try (streamReadByte (rcStreamIO serverConn)) :: IO (Either SomeException Word8)
      case result of
        Left _ -> pure () -- expected: connection closed
        Right _ -> expectationFailure "Expected read to fail after close"
      rcClose serverConn
      listenerClose listener

  describe "Dial failure" $ do
    it "dial to refused port returns error" $ do
      -- Use a high port on loopback that's very unlikely to be listening.
      -- The OS will immediately return ECONNREFUSED.
      transport <- newTCPTransport
      let Right addr = fromText "/ip4/127.0.0.1/tcp/1"
      result <- try (transportDial transport addr) :: IO (Either SomeException RawConnection)
      case result of
        Left _ -> pure () -- expected: connection refused
        Right conn -> do
          rcClose conn
          expectationFailure "Expected dial to fail"

-- | An Ed25519-shaped identity multihash usable as a /p2p component.
testPeerIdMH :: BS.ByteString
testPeerIdMH = BS.pack $ [0x00, 0x24, 0x08, 0x01, 0x12, 0x20] <> replicate 32 0xAB

