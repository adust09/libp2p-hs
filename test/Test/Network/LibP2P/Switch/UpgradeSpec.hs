module Test.Network.LibP2P.Switch.UpgradeSpec (spec) where

import Control.Concurrent.Async (concurrently)
import qualified Data.ByteString as BS
import Data.IORef (newIORef)
import Network.LibP2P.Crypto.Ed25519 (generateKeyPair)
import Network.LibP2P.Crypto.Key (KeyPair, publicKey)
import Network.LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr (..))
import Network.LibP2P.Multiaddr.Protocol (Protocol (..))
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..), mkMemoryStreamPair)
import Network.LibP2P.Security.Noise.Handshake (HandshakeResult (..))
import Network.LibP2P.Switch.Types (Connection (..), Direction (..), MuxerSession (..))
import Network.LibP2P.Switch.Upgrade
import Network.LibP2P.Transport.Transport (RawConnection (..))
import Test.Hspec

-- | Generate a test identity (PeerId, KeyPair).
mkTestIdentity :: IO (PeerId, KeyPair)
mkTestIdentity = do
  Right kp <- generateKeyPair
  let pid = fromPublicKey (publicKey kp)
  pure (pid, kp)

-- | Create a mock RawConnection from a StreamIO and addresses.
mkMockRawConn :: StreamIO -> Multiaddr -> Multiaddr -> IO RawConnection
mkMockRawConn sio local remote = pure RawConnection
  { rcStreamIO   = sio
  , rcLocalAddr  = local
  , rcRemoteAddr = remote
  , rcClose      = pure ()
  }

localAddr :: Multiaddr
localAddr = Multiaddr [IP4 0x7f000001, TCP 12345]

remoteAddr :: Multiaddr
remoteAddr = Multiaddr [IP4 0x7f000001, TCP 54321]

spec :: Spec
spec = do
  describe "Framed message I/O" $ do
    it "writeFramedMessage + readFramedMessage round-trips" $ do
      (streamA, streamB) <- mkMemoryStreamPair
      writeFramedMessage streamA "hello noise"
      received <- readFramedMessage streamB
      received `shouldBe` "hello noise"

    it "readFramedMessage handles empty payload" $ do
      (streamA, streamB) <- mkMemoryStreamPair
      writeFramedMessage streamA BS.empty
      received <- readFramedMessage streamB
      received `shouldBe` BS.empty

  describe "performStreamHandshake" $ do
    it "initiator and responder complete handshake and return correct PeerIds" $ do
      (pidA, kpA) <- mkTestIdentity
      (pidB, kpB) <- mkTestIdentity
      (streamA, streamB) <- mkMemoryStreamPair
      ((_sessA, resultA), (_sessB, resultB)) <-
        concurrently
          (performStreamHandshake kpA Outbound streamA)
          (performStreamHandshake kpB Inbound streamB)
      -- A sees B's PeerId, B sees A's PeerId
      hrRemotePeerId resultA `shouldBe` pidB
      hrRemotePeerId resultB `shouldBe` pidA

  describe "noiseSessionToStreamIO" $ do
    it "encrypted write on one side is decrypted read on other side" $ do
      (_pidA, kpA) <- mkTestIdentity
      (_pidB, kpB) <- mkTestIdentity
      (rawA, rawB) <- mkMemoryStreamPair
      -- Perform handshake to get sessions
      ((sessA, _), (sessB, _)) <-
        concurrently
          (performStreamHandshake kpA Outbound rawA)
          (performStreamHandshake kpB Inbound rawB)
      -- Create encrypted StreamIOs
      sendRefA <- newIORef sessA
      recvRefA <- newIORef sessA
      bufRefA  <- newIORef BS.empty
      sendRefB <- newIORef sessB
      recvRefB <- newIORef sessB
      bufRefB  <- newIORef BS.empty
      let encA = noiseSessionToStreamIO sendRefA recvRefA bufRefA rawA
          encB = noiseSessionToStreamIO sendRefB recvRefB bufRefB rawB
      -- A writes "hello", B reads
      streamWrite encA "hello"
      received <- readExact encB 5
      received `shouldBe` "hello"
      -- B writes "world", A reads
      streamWrite encB "world"
      received2 <- readExact encA 5
      received2 `shouldBe` "world"

  describe "Full upgrade pipeline" $ do
    it "upgradeOutbound + upgradeInbound exchange data on muxed stream" $ do
      (_pidA, kpA) <- mkTestIdentity
      (_pidB, kpB) <- mkTestIdentity
      (rawA, rawB) <- mkMemoryStreamPair
      rawConnA <- mkMockRawConn rawA localAddr remoteAddr
      rawConnB <- mkMockRawConn rawB remoteAddr localAddr
      (connA, connB) <-
        concurrently
          (upgradeOutbound kpA rawConnA)
          (upgradeInbound kpB rawConnB)
      -- Open stream from A (client), accept on B (server)
      (streamA, streamB) <-
        concurrently
          (muxOpenStream (connSession connA))
          (muxAcceptStream (connSession connB))
      -- Exchange data
      streamWrite streamA "ping"
      received <- readExact streamB 4
      received `shouldBe` "ping"
      streamWrite streamB "pong"
      received2 <- readExact streamA 4
      received2 `shouldBe` "pong"
      -- Cleanup
      muxClose (connSession connA)
      muxClose (connSession connB)

    it "upgradeOutbound + upgradeInbound verify remote PeerIds" $ do
      (pidA, kpA) <- mkTestIdentity
      (pidB, kpB) <- mkTestIdentity
      (rawA, rawB) <- mkMemoryStreamPair
      rawConnA <- mkMockRawConn rawA localAddr remoteAddr
      rawConnB <- mkMockRawConn rawB remoteAddr localAddr
      (connA, connB) <-
        concurrently
          (upgradeOutbound kpA rawConnA)
          (upgradeInbound kpB rawConnB)
      -- A's connection should show B's PeerId and vice versa
      connPeerId connA `shouldBe` pidB
      connPeerId connB `shouldBe` pidA
      connSecurity connA `shouldBe` "/noise"
      connMuxer connA `shouldBe` "/yamux/1.0.0"
      muxClose (connSession connA)
      muxClose (connSession connB)

    it "multiple streams on single upgraded connection" $ do
      (_pidA, kpA) <- mkTestIdentity
      (_pidB, kpB) <- mkTestIdentity
      (rawA, rawB) <- mkMemoryStreamPair
      rawConnA <- mkMockRawConn rawA localAddr remoteAddr
      rawConnB <- mkMockRawConn rawB remoteAddr localAddr
      (connA, connB) <-
        concurrently
          (upgradeOutbound kpA rawConnA)
          (upgradeInbound kpB rawConnB)
      -- Open two streams from A
      (stream1A, stream1B) <-
        concurrently
          (muxOpenStream (connSession connA))
          (muxAcceptStream (connSession connB))
      (stream2A, stream2B) <-
        concurrently
          (muxOpenStream (connSession connA))
          (muxAcceptStream (connSession connB))
      -- Exchange data on both streams independently
      streamWrite stream1A "stream-1"
      streamWrite stream2A "stream-2"
      recv1 <- readExact stream1B 8
      recv2 <- readExact stream2B 8
      recv1 `shouldBe` "stream-1"
      recv2 `shouldBe` "stream-2"
      muxClose (connSession connA)
      muxClose (connSession connB)
