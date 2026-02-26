module Test.Network.LibP2P.Transport.TCPSpec (spec) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Network.LibP2P.Multiaddr.Multiaddr (fromText)
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import Network.LibP2P.Transport.TCP (multiaddrToHostPort, newTCPTransport)
import Network.LibP2P.Transport.Transport
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

  describe "transportCanDial" $ do
    it "returns True for /ip4/127.0.0.1/tcp/4001" $ do
      transport <- newTCPTransport
      let Right addr = fromText "/ip4/127.0.0.1/tcp/4001"
      transportCanDial transport addr `shouldBe` True

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

