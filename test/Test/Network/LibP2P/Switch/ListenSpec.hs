module Test.Network.LibP2P.Switch.ListenSpec (spec) where

import Control.Concurrent.Async (async, concurrently)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, retry)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.LibP2P.Crypto.Ed25519 (generateKeyPair)
import Network.LibP2P.Crypto.Key (KeyPair, publicKey)
import Network.LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr (..))
import Network.LibP2P.Multiaddr.Protocol (Protocol (..))
import Network.LibP2P.MultistreamSelect.Negotiation
  ( NegotiationResult (..)
  , StreamIO (..)
  , mkMemoryStreamPair
  , negotiateInitiator
  )
import Network.LibP2P.Switch.ConnPool (lookupConn)
import Network.LibP2P.Switch.Listen
  ( ConnectionGater (..)
  , defaultConnectionGater
  , dispatchStream
  , handleInbound
  , streamAcceptLoop
  )
import Network.LibP2P.Switch.Switch (newSwitch, setStreamHandler)
import Network.LibP2P.Switch.Types
  ( Connection (..)
  , MuxerSession (..)
  , Switch (..)
  )
import Network.LibP2P.Switch.Upgrade (upgradeInbound, upgradeOutbound)
import Network.LibP2P.Transport.Transport (RawConnection (..))
import System.Timeout (timeout)
import Test.Hspec

-- | Generate a test identity (PeerId, KeyPair).
mkTestIdentity :: IO (PeerId, KeyPair)
mkTestIdentity = do
  Right kp <- generateKeyPair
  let pid = fromPublicKey (publicKey kp)
  pure (pid, kp)

localAddr :: Multiaddr
localAddr = Multiaddr [IP4 0x7f000001, TCP 12345]

remoteAddr :: Multiaddr
remoteAddr = Multiaddr [IP4 0x7f000001, TCP 54321]

-- | Create a mock RawConnection from a StreamIO.
mkMockRawConn :: StreamIO -> Multiaddr -> Multiaddr -> RawConnection
mkMockRawConn sio local remote = RawConnection
  { rcStreamIO   = sio
  , rcLocalAddr  = local
  , rcRemoteAddr = remote
  , rcClose      = pure ()
  }

spec :: Spec
spec = do
  describe "handleInbound" $ do
    it "upgrades connection and adds to pool" $ do
      (pidA, kpA) <- mkTestIdentity
      (pidB, kpB) <- mkTestIdentity
      sw <- newSwitch pidB kpB
      (streamA, streamB) <- mkMemoryStreamPair
      let rawConnA = mkMockRawConn streamA localAddr remoteAddr
          rawConnB = mkMockRawConn streamB remoteAddr localAddr
      -- handleInbound blocks on streamAcceptLoop, so spawn async
      _listenerThread <- async $ handleInbound sw defaultConnectionGater rawConnB
      -- Dialer upgrades synchronously; both sides must handshake concurrently
      upgradeResult <- timeout 3000000 $ upgradeOutbound kpA rawConnA
      case upgradeResult of
        Nothing -> expectationFailure "upgradeOutbound timed out (handleInbound stub may not handshake)"
        Just connA -> do
          -- Wait for connection to appear in pool
          poolResult <- timeout 2000000 $ atomically $ do
            mc <- lookupConn (swConnPool sw) pidA
            case mc of
              Nothing -> retry
              Just c  -> pure c
          case poolResult of
            Nothing -> expectationFailure "timeout: connection not added to pool"
            Just c  -> connPeerId c `shouldBe` pidA
          muxClose (connSession connA)

    it "rejects connection when gateAccept returns False" $ do
      (pidB, kpB) <- mkTestIdentity
      sw <- newSwitch pidB kpB
      closeRef <- newIORef False
      (_streamA, streamB) <- mkMemoryStreamPair
      let rawConnB = RawConnection
            { rcStreamIO   = streamB
            , rcLocalAddr  = remoteAddr
            , rcRemoteAddr = localAddr
            , rcClose      = writeIORef closeRef True
            }
          denyGater = ConnectionGater
            { gateAccept  = \_ -> pure False
            , gateSecured = \_ -> pure True
            }
      -- handleInbound should deny at gate and close raw connection
      handleInbound sw denyGater rawConnB
      -- Verify rcClose was called
      closed <- readIORef closeRef
      closed `shouldBe` True

    it "rejects connection when gateSecured returns False" $ do
      (pidA, kpA) <- mkTestIdentity
      (pidB, kpB) <- mkTestIdentity
      sw <- newSwitch pidB kpB
      (streamA, streamB) <- mkMemoryStreamPair
      let rawConnA = mkMockRawConn streamA localAddr remoteAddr
          rawConnB = mkMockRawConn streamB remoteAddr localAddr
          denyAfterSecure = ConnectionGater
            { gateAccept  = \_ -> pure True
            , gateSecured = \_ -> pure False
            }
      -- Handshake completes but gateSecured denies.
      -- Both sides must run concurrently for the handshake to work.
      result <- timeout 3000000 $ concurrently
        (upgradeOutbound kpA rawConnA)
        (handleInbound sw denyAfterSecure rawConnB)
      case result of
        Nothing -> expectationFailure "timeout during handshake"
        Just _ -> pure ()
      -- Pool should be empty (connection denied after handshake)
      poolConn <- atomically $ lookupConn (swConnPool sw) pidA
      case poolConn of
        Nothing -> pure ()
        Just _  -> expectationFailure "connection should not be in pool"

  describe "streamAcceptLoop" $ do
    it "dispatches stream to registered handler" $ do
      (pidA, kpA) <- mkTestIdentity
      (pidB, kpB) <- mkTestIdentity
      sw <- newSwitch pidB kpB
      -- Register a handler that signals when called via MVar
      handlerDone <- newEmptyMVar
      setStreamHandler sw "/test/1.0.0" $ \_stream peerId ->
        putMVar handlerDone peerId
      -- Set up upgraded connections (dialer=outbound, listener=inbound)
      (streamA, streamB) <- mkMemoryStreamPair
      let rawConnA = mkMockRawConn streamA localAddr remoteAddr
          rawConnB = mkMockRawConn streamB remoteAddr localAddr
      (connDialer, connListener) <- concurrently
        (upgradeOutbound kpA rawConnA)
        (upgradeInbound kpB rawConnB)
      -- Start stream accept loop on listener's connection
      _loopThread <- async $ streamAcceptLoop sw connListener
      -- From dialer, open a mux stream, negotiate protocol, wait for handler
      result <- timeout 5000000 $ do
        dialerStream <- muxOpenStream (connSession connDialer)
        negResult <- negotiateInitiator dialerStream ["/test/1.0.0"]
        case negResult of
          Accepted _ -> takeMVar handlerDone
          _          -> fail "protocol negotiation failed"
      case result of
        Nothing -> expectationFailure "timeout: stream not dispatched to handler"
        Just receivedPeerId -> receivedPeerId `shouldBe` pidA
      muxClose (connSession connDialer)
      muxClose (connSession connListener)

  describe "dispatchStream" $ do
    it "handles unknown protocol gracefully" $ do
      (_pidA, kpA) <- mkTestIdentity
      (_pidB, kpB) <- mkTestIdentity
      sw <- newSwitch _pidB kpB
      -- No handlers registered
      (streamA, streamB) <- mkMemoryStreamPair
      let rawConnA = mkMockRawConn streamA localAddr remoteAddr
          rawConnB = mkMockRawConn streamB remoteAddr localAddr
      (connDialer, connListener) <- concurrently
        (upgradeOutbound kpA rawConnA)
        (upgradeInbound kpB rawConnB)
      -- Open mux stream from dialer, accept on listener
      (dialerStream, listenerStream) <- concurrently
        (muxOpenStream (connSession connDialer))
        (muxAcceptStream (connSession connListener))
      -- Dispatch on listener side with no handlers
      _dispatchThread <- async $ dispatchStream sw connListener listenerStream
      -- Dialer proposes unknown protocol; expect NoProtocol or timeout
      result <- timeout 2000000 $ negotiateInitiator dialerStream ["/unknown/1.0.0"]
      case result of
        Just NoProtocol -> pure ()
        Just (Accepted _) -> expectationFailure "expected NoProtocol"
        Nothing -> expectationFailure "timeout during negotiation"
      muxClose (connSession connDialer)
      muxClose (connSession connListener)
