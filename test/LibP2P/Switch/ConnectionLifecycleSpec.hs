-- | Connection lifecycle tests (issue #179).
--
-- Verifies that connections are actually torn down: pool removal on
-- remote disconnect, explicit closeConnection, switchClose teardown,
-- resource release, stream slot accounting, and dial dedup cleanup.
module LibP2P.Switch.ConnectionLifecycleSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar)
import Control.Exception (SomeException, try)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map.Strict as Map
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (KeyPair, publicKey)
import LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import LibP2P.Multiaddr (Multiaddr (..))
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..), mkMemoryStreamPair, negotiateInitiator)
import LibP2P.Switch (addTransport, newSwitch, setStreamHandler, switchClose)
import LibP2P.Switch.ConnPool (addConn, lookupConn)
import LibP2P.Switch.Connection (closeConnection, newStream)
import LibP2P.Switch.Dial (dial)
import LibP2P.Switch.Listen (defaultConnectionGater, dispatchStream, switchListen)
import LibP2P.Switch.ResourceManager
  ( ResourceManager (..)
  , ResourceScope (..)
  , ResourceUsage (..)
  )
import LibP2P.Switch.Types
  ( ConnState (..)
  , Connection (..)
  , DialError (..)
  , Direction (..)
  , MuxerSession (..)
  , Switch (..)
  )
import LibP2P.Switch.Upgrade (upgradeInbound)
import LibP2P.Transport (RawConnection (..), Transport (..))
import LibP2P.Transport.TCP (newTCPTransport)
import System.Timeout (timeout)
import Test.Hspec

-- | Generate a test identity (PeerId, KeyPair).
mkTestIdentity :: IO (PeerId, KeyPair)
mkTestIdentity = do
  Right kp <- generateKeyPair
  let pid = fromPublicKey (publicKey kp)
  pure (pid, kp)

-- | A test multiaddr: /ip4/127.0.0.1/tcp/4001
testAddr :: Multiaddr
testAddr = Multiaddr [IP4 0x7f000001, TCP 4001]

-- | Loopback address with port 0 (OS assigns ephemeral port).
loopbackAddr :: Multiaddr
loopbackAddr = Multiaddr [IP4 0x7f000001, TCP 0]

-- | Poll a condition every 100ms, up to n attempts.
waitUntil :: Int -> IO Bool -> IO Bool
waitUntil 0 _ = pure False
waitUntil n check = do
  ok <- check
  if ok
    then pure True
    else threadDelay 100000 >> waitUntil (n - 1) check

-- | Read the peer scope usage for a peer, if the scope exists.
peerUsage :: Switch -> PeerId -> IO (Maybe ResourceUsage)
peerUsage sw pid = atomically $ do
  peers <- readTVar (rmPeerScopes (swResourceMgr sw))
  case Map.lookup pid peers of
    Nothing -> pure Nothing
    Just scope -> Just <$> readTVar (rsUsage scope)

-- | A dummy connection whose muxer serves in-memory streams.
mkDummyConnection :: PeerId -> IO StreamIO -> IO Connection
mkDummyConnection pid openAction = do
  stateVar <- newTVarIO ConnOpen
  pure Connection
    { connPeerId     = pid
    , connDirection  = Outbound
    , connLocalAddr  = Multiaddr [IP4 0x7f000001, TCP 0]
    , connRemoteAddr = testAddr
    , connSecurity   = "/noise"
    , connMuxer      = "/yamux/1.0.0"
    , connSession    = MuxerSession
        { muxOpenStream   = openAction
        , muxAcceptStream = fail "dummy: no inbound streams"
        , muxClose        = pure ()
        }
    , connState      = stateVar
    }

-- | Mock transport whose dialed connection records rcClose calls in an IORef.
mkClosableMockTransport :: KeyPair -> IORef Bool -> IO Transport
mkClosableMockTransport responderKP closedRef = pure Transport
  { transportDial = \addr -> do
      (streamA, streamB) <- mkMemoryStreamPair
      let rawConnB = RawConnection
            { rcStreamIO   = streamB
            , rcLocalAddr  = addr
            , rcRemoteAddr = Multiaddr [IP4 0x7f000001, TCP 0]
            , rcClose      = pure ()
            }
      _ <- async $ do
        _ <- upgradeInbound responderKP rawConnB
        pure ()
      pure RawConnection
        { rcStreamIO   = streamA
        , rcLocalAddr  = Multiaddr [IP4 0x7f000001, TCP 0]
        , rcRemoteAddr = addr
        , rcClose      = writeIORef closedRef True
        }
  , transportListen = \_ -> error "mock: listen not supported"
  , transportCanDial = \(Multiaddr ps) -> case ps of
      (IP4 _ : TCP _ : _) -> True
      _ -> False
  }

-- | Build a TCP node with Switch.
mkTCPNode :: IO (Switch, PeerId)
mkTCPNode = do
  (pid, kp) <- mkTestIdentity
  sw <- newSwitch pid kp
  tcp <- newTCPTransport
  addTransport sw tcp
  pure (sw, pid)

spec :: Spec
spec = do
  describe "closeConnection" $ do
    it "removes the connection from the pool, closes the transport, and releases the reservation" $ do
      (localPid, localKP) <- mkTestIdentity
      (_remotePid, remoteKP) <- mkTestIdentity
      sw <- newSwitch localPid localKP
      closedRef <- newIORef False
      transport <- mkClosableMockTransport remoteKP closedRef
      addTransport sw transport
      result <- dial sw _remotePid [testAddr]
      case result of
        Left err -> expectationFailure $ "dial failed: " <> show err
        Right conn -> do
          -- Reservation is held while the connection lives
          usageBefore <- peerUsage sw (connPeerId conn)
          fmap ruConnsOutbound usageBefore `shouldBe` Just 1
          closeConnection sw conn
          -- Pool no longer contains the connection
          poolConn <- atomically $ lookupConn (swConnPool sw) (connPeerId conn)
          isNothing poolConn `shouldBe` True
          -- Connection state is ConnClosed
          st <- atomically $ readTVar (connState conn)
          st `shouldBe` ConnClosed
          -- Underlying transport was closed
          closedOk <- waitUntil 20 (readIORef closedRef)
          closedOk `shouldBe` True
          -- Connection reservation was released
          usageAfter <- peerUsage sw (connPeerId conn)
          fmap ruConnsOutbound usageAfter `shouldBe` Just 0

    it "is idempotent (double close does not underflow the reservation)" $ do
      (localPid, localKP) <- mkTestIdentity
      (_remotePid, remoteKP) <- mkTestIdentity
      sw <- newSwitch localPid localKP
      closedRef <- newIORef False
      transport <- mkClosableMockTransport remoteKP closedRef
      addTransport sw transport
      Right conn <- dial sw _remotePid [testAddr]
      closeConnection sw conn
      closeConnection sw conn
      usage <- peerUsage sw (connPeerId conn)
      fmap ruConnsOutbound usage `shouldBe` Just 0

  describe "remote disconnect" $ do
    it "removes the connection from the pool when the remote closes; a fresh dial succeeds" $ do
      (swB, pidB) <- mkTCPNode
      addrs <- switchListen swB defaultConnectionGater [loopbackAddr]
      let listenAddr = head addrs
      (swA, pidA) <- mkTCPNode
      Right conn <- dial swA pidB [listenAddr]
      -- Wait for B to pool the inbound connection
      pooledOk <- waitUntil 30 $ do
        c <- atomically $ lookupConn (swConnPool swB) pidA
        pure (isJust c)
      pooledOk `shouldBe` True
      -- B closes its side of the connection
      Just connB <- atomically $ lookupConn (swConnPool swB) pidA
      closeConnection swB connB
      -- A must notice the dead session and drop the connection from its pool
      droppedOk <- waitUntil 50 $ do
        c <- atomically $ lookupConn (swConnPool swA) pidB
        pure (isNothing c)
      droppedOk `shouldBe` True
      -- A's connection reservation must be released
      usageA <- peerUsage swA pidB
      fmap ruConnsOutbound usageA `shouldBe` Just 0
      -- A subsequent dial creates a fresh connection
      redialResult <- timeout 5000000 $ dial swA pidB [listenAddr]
      case redialResult of
        Nothing -> expectationFailure "redial timed out"
        Just (Left err) -> expectationFailure $ "redial failed: " <> show err
        Just (Right conn2) ->
          -- Fresh connection: distinct state TVar
          (connState conn2 == connState conn) `shouldBe` False
      switchClose swA
      switchClose swB

  describe "switchClose" $ do
    it "closes pooled connections on both sides" $ do
      (swB, pidB) <- mkTCPNode
      addrs <- switchListen swB defaultConnectionGater [loopbackAddr]
      let listenAddr = head addrs
      (swA, pidA) <- mkTCPNode
      Right conn <- dial swA pidB [listenAddr]
      pooledOk <- waitUntil 30 $ do
        c <- atomically $ lookupConn (swConnPool swB) pidA
        pure (isJust c)
      pooledOk `shouldBe` True
      switchClose swA
      -- A's pool is emptied and the connection is closed
      poolA <- atomically $ lookupConn (swConnPool swA) pidB
      isNothing poolA `shouldBe` True
      st <- atomically $ readTVar (connState conn)
      st `shouldBe` ConnClosed
      -- B notices the remote disconnect and drops A from its pool
      droppedOk <- waitUntil 50 $ do
        c <- atomically $ lookupConn (swConnPool swB) pidA
        pure (isNothing c)
      droppedOk `shouldBe` True
      switchClose swB

  describe "stream resource accounting" $ do
    it "newStream reserves an outbound stream slot and releases it on close" $ do
      (localPid, localKP) <- mkTestIdentity
      (remotePid, _remoteKP) <- mkTestIdentity
      sw <- newSwitch localPid localKP
      (sIO, _peerIO) <- mkMemoryStreamPair
      conn <- mkDummyConnection remotePid (pure sIO)
      atomically $ addConn (swConnPool sw) conn
      result <- newStream sw conn
      case result of
        Left err -> expectationFailure $ "newStream failed: " <> show err
        Right stream -> do
          usageOpen <- peerUsage sw remotePid
          fmap ruStreamsOutbound usageOpen `shouldBe` Just 1
          streamClose stream
          usageClosed <- peerUsage sw remotePid
          fmap ruStreamsOutbound usageClosed `shouldBe` Just 0
          -- Double close releases only once
          streamClose stream
          usageDouble <- peerUsage sw remotePid
          fmap ruStreamsOutbound usageDouble `shouldBe` Just 0

    it "dispatchStream counts an inbound stream against the peer limit and releases it" $ do
      (localPid, localKP) <- mkTestIdentity
      (remotePid, _remoteKP) <- mkTestIdentity
      sw <- newSwitch localPid localKP
      usageMVar <- newEmptyMVar
      setStreamHandler sw "/test/1.0.0" $ \_conn _stream -> do
        usage <- peerUsage sw remotePid
        putMVar usageMVar (fmap ruStreamsInbound usage)
      conn <- mkDummyConnection remotePid (fail "no outbound")
      (clientIO, serverIO) <- mkMemoryStreamPair
      _ <- async $ negotiateInitiator clientIO ["/test/1.0.0"]
      dispatchResult <- timeout 5000000 $ dispatchStream sw conn serverIO
      dispatchResult `shouldBe` Just ()
      -- During the handler, the inbound stream slot was held
      heldUsage <- takeMVar usageMVar
      heldUsage `shouldBe` Just 1
      -- After dispatch completes, the slot is released
      usageAfter <- peerUsage sw remotePid
      fmap ruStreamsInbound usageAfter `shouldBe` Just 0

  describe "dial dedup" $ do
    it "removes the pending entry when the dial throws" $ do
      (localPid, localKP) <- mkTestIdentity
      (remotePid, _remoteKP) <- mkTestIdentity
      sw <- newSwitch localPid localKP
      -- A transport whose canDial check blows up mid-dial
      addTransport sw Transport
        { transportDial = \_ -> fail "unreachable"
        , transportListen = \_ -> error "mock: listen not supported"
        , transportCanDial = \_ -> error "boom: canDial exploded"
        }
      firstResult <- try (dial sw remotePid [testAddr]) :: IO (Either SomeException (Either DialError Connection))
      case firstResult of
        Left _ -> pure ()  -- Expected: the dial threw
        Right _ -> expectationFailure "expected the dial to throw"
      -- The pending dial entry must have been cleaned up
      pendingMap <- atomically $ readTVar (swPendingDials sw)
      Map.member remotePid pendingMap `shouldBe` False
      -- A subsequent dial must not wedge on the stale entry
      secondResult <- timeout 2000000 (try (dial sw remotePid [testAddr]) :: IO (Either SomeException (Either DialError Connection)))
      case secondResult of
        Nothing -> expectationFailure "second dial wedged on a stale pending entry"
        Just _ -> pure ()
