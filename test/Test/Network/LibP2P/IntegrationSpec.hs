-- | End-to-end integration tests for the libp2p stack (Phase 10c).
--
-- Tests the complete protocol pipeline over real TCP connections:
-- TCP transport → Noise XX → Yamux → multistream-select → protocols.
module Test.Network.LibP2P.IntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
  ( atomically
  , readTVar
  , writeTVar
  )
import Control.Exception (SomeException, bracket, try)
import qualified Data.Map.Strict as Map
import Network.LibP2P.Crypto.Ed25519 (generateKeyPair)
import Network.LibP2P.Crypto.Key (KeyPair, publicKey)
import Network.LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr (..))
import Network.LibP2P.Multiaddr.Protocol (Protocol (..))
import Network.LibP2P.Protocol.GossipSub.Handler
  ( gossipJoin
  , gossipPublish
  , newGossipSubNode
  , startGossipSub
  , stopGossipSub
  , GossipSubNode (..)
  )
import Network.LibP2P.Protocol.GossipSub.Types
  ( GossipSubRouter (..)
  , PubSubMessage (..)
  , defaultGossipSubParams
  , GossipSubParams (..)
  )
import Network.LibP2P.Protocol.Identify.Identify
  ( registerIdentifyHandlers
  , requestIdentify
  )
import Network.LibP2P.Protocol.Identify.Message (IdentifyInfo (..))
import Network.LibP2P.Protocol.Ping.Ping
  ( PingResult (..)
  , registerPingHandler
  , sendPing
  )
import Network.LibP2P.Switch.ConnPool (lookupConn)
import Network.LibP2P.Switch.Dial (dial)
import Network.LibP2P.Switch.Listen
  ( ConnectionGater (..)
  , defaultConnectionGater
  , switchListen
  )
import Network.LibP2P.Switch.Switch (addTransport, newSwitch, switchClose)
import Network.LibP2P.Switch.Types (Connection (..), DialError (..), Switch (..))
import Network.LibP2P.Transport.TCP (newTCPTransport)
import System.Timeout (timeout)
import Test.Hspec

-- | Generate a test identity (PeerId, KeyPair).
mkTestIdentity :: IO (PeerId, KeyPair)
mkTestIdentity = do
  Right kp <- generateKeyPair
  let pid = fromPublicKey (publicKey kp)
  pure (pid, kp)

-- | Loopback address with port 0 (OS assigns ephemeral port).
loopbackAddr :: Multiaddr
loopbackAddr = Multiaddr [IP4 0x7f000001, TCP 0]

-- | Create a test node: Switch + TCP transport + Identify + Ping.
-- Returns the Switch, PeerId, and KeyPair.
withTestNode :: (Switch -> PeerId -> IO a) -> IO a
withTestNode action = bracket setup teardown (\(sw, pid, _kp) -> action sw pid)
  where
    setup = do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      tcp <- newTCPTransport
      addTransport sw tcp
      registerIdentifyHandlers sw
      registerPingHandler sw
      pure (sw, pid, kp)
    teardown (sw, _pid, _kp) = switchClose sw

-- | Create two connected test nodes: node B listens, node A dials.
-- Returns both Switches, PeerIds, and the connection from A to B.
withConnectedPair :: ((Switch, PeerId) -> (Switch, PeerId) -> Connection -> IO a) -> IO a
withConnectedPair action = do
  -- Node B (listener)
  (pidB, kpB) <- mkTestIdentity
  swB <- newSwitch pidB kpB
  tcpB <- newTCPTransport
  addTransport swB tcpB
  registerIdentifyHandlers swB
  registerPingHandler swB
  addrs <- switchListen swB defaultConnectionGater [loopbackAddr]
  let listenAddr = head addrs
  -- Node A (dialer)
  (pidA, kpA) <- mkTestIdentity
  swA <- newSwitch pidA kpA
  tcpA <- newTCPTransport
  addTransport swA tcpA
  registerIdentifyHandlers swA
  registerPingHandler swA
  -- Dial from A to B
  dialResult <- dial swA pidB [listenAddr]
  case dialResult of
    Left err -> do
      switchClose swB
      switchClose swA
      fail $ "withConnectedPair: dial failed: " ++ show err
    Right conn -> do
      -- Allow listener to complete pool insertion
      threadDelay 300000
      result <- action (swA, pidA) (swB, pidB) conn
      switchClose swA
      switchClose swB
      pure result

spec :: Spec
spec = do
  describe "TCP + Upgrade" $ do
    it "two nodes: switchListen + dial -> connection in pool, correct PeerIds" $ do
      withConnectedPair $ \(swA, pidA) (swB, pidB) conn -> do
        -- A's connection points to B
        connPeerId conn `shouldBe` pidB
        -- B should see A in its pool
        poolConn <- atomically $ lookupConn (swConnPool swB) pidA
        case poolConn of
          Nothing -> expectationFailure "listener should see dialer in pool"
          Just c  -> connPeerId c `shouldBe` pidA

  describe "Ping over real TCP" $ do
    it "sendPing returns valid RTT (>0, <1s for loopback)" $ do
      withConnectedPair $ \_nodeA _nodeB conn -> do
        result <- timeout 5000000 $ sendPing conn
        case result of
          Nothing -> expectationFailure "ping timed out"
          Just (Left err) -> expectationFailure $ "ping failed: " ++ show err
          Just (Right (PingResult rtt)) -> do
            rtt `shouldSatisfy` (> 0)
            rtt `shouldSatisfy` (< 1)  -- < 1 second for loopback

  describe "Identify over real TCP" $ do
    it "requestIdentify returns correct protocols and agentVersion" $ do
      withConnectedPair $ \_nodeA _nodeB conn -> do
        result <- timeout 5000000 $ requestIdentify conn
        case result of
          Nothing -> expectationFailure "identify timed out"
          Just (Left err) -> expectationFailure $ "identify failed: " ++ err
          Just (Right info) -> do
            idAgentVersion info `shouldBe` Just "libp2p-hs/0.1.0"
            idProtocols info `shouldSatisfy` (not . null)

    it "idListenAddrs populated after switchListen" $ do
      withConnectedPair $ \_nodeA _nodeB conn -> do
        result <- timeout 5000000 $ requestIdentify conn
        case result of
          Nothing -> expectationFailure "identify timed out"
          Just (Left err) -> expectationFailure $ "identify failed: " ++ err
          Just (Right info) -> do
            -- B is listening, so idListenAddrs should be non-empty
            idListenAddrs info `shouldSatisfy` (not . null)

  describe "Multi-protocol" $ do
    it "multiple Ping requests on different streams over same connection" $ do
      withConnectedPair $ \_nodeA _nodeB conn -> do
        -- Send two Pings on separate streams over the same muxed connection
        pingResult1 <- sendPing conn
        case pingResult1 of
          Left err -> expectationFailure $ "ping 1 failed: " ++ show err
          Right (PingResult rtt1) -> rtt1 `shouldSatisfy` (> 0)
        pingResult2 <- sendPing conn
        case pingResult2 of
          Left err -> expectationFailure $ "ping 2 failed: " ++ show err
          Right (PingResult rtt2) -> rtt2 `shouldSatisfy` (> 0)

  describe "GossipSub over real TCP" $ do
    it "two nodes join topic, publish -> receive" $ do
      let gsParams = defaultGossipSubParams
            { paramHeartbeatInterval = 60.0  -- Very long to avoid interference
            }
      -- Node B (listener)
      (pidB, kpB) <- mkTestIdentity
      swB <- newSwitch pidB kpB
      tcpB <- newTCPTransport
      addTransport swB tcpB
      gsNodeB <- newGossipSubNode swB gsParams
      startGossipSub gsNodeB
      -- Message callback on B
      msgMVar <- newEmptyMVar
      atomically $ writeTVar (gsOnMessage (gsnRouter gsNodeB))
        (\topic msg -> putMVar msgMVar (topic, msgData msg))
      addrs <- switchListen swB defaultConnectionGater [loopbackAddr]
      let listenAddr = head addrs
      -- Node A (dialer)
      (_pidA, kpA) <- mkTestIdentity
      swA <- newSwitch (fromPublicKey (publicKey kpA)) kpA
      tcpA <- newTCPTransport
      addTransport swA tcpA
      gsNodeA <- newGossipSubNode swA gsParams
      startGossipSub gsNodeA
      -- Dial A -> B: establish connection
      dialResult <- timeout 5000000 $ dial swA pidB [listenAddr]
      case dialResult of
        Nothing -> do
          stopGossipSub gsNodeA; stopGossipSub gsNodeB
          switchClose swA; switchClose swB
          expectationFailure "dial timed out"
        Just (Left err) -> do
          stopGossipSub gsNodeA; stopGossipSub gsNodeB
          switchClose swA; switchClose swB
          expectationFailure $ "dial failed: " ++ show err
        Just (Right conn) -> do
          -- Wait for mux + stream accept loop to start on both sides
          threadDelay 500000
          -- Both nodes join the topic
          gossipJoin gsNodeB "test-topic"
          threadDelay 200000
          gossipJoin gsNodeA "test-topic"
          -- Let subscription RPCs propagate over the real streams
          threadDelay 1000000
          -- Publish from A (floodPublish=True sends to all subscribed peers)
          gossipPublish gsNodeA "test-topic" "integration test message"
          -- Wait for B to receive
          result <- timeout 5000000 $ takeMVar msgMVar
          -- Cleanup
          stopGossipSub gsNodeA; stopGossipSub gsNodeB
          switchClose swA; switchClose swB
          case result of
            Nothing -> expectationFailure "nodeB did not receive message"
            Just (topic, dat) -> do
              topic `shouldBe` "test-topic"
              dat `shouldBe` "integration test message"

  describe "Connection gating" $ do
    it "gateSecured=False rejects peer, no pool entry" $ do
      -- Listener with secured rejection
      (pidB, kpB) <- mkTestIdentity
      swB <- newSwitch pidB kpB
      tcpB <- newTCPTransport
      addTransport swB tcpB
      let securedRejectGater = ConnectionGater
            { gateAccept  = \_ -> pure True
            , gateSecured = \_ -> pure False
            }
      addrs <- switchListen swB securedRejectGater [loopbackAddr]
      let listenAddr = head addrs
      -- Dialer
      (pidA, kpA) <- mkTestIdentity
      swA <- newSwitch pidA kpA
      tcpA <- newTCPTransport
      addTransport swA tcpA
      _dialResult <- timeout 5000000 $ dial swA pidB [listenAddr]
      threadDelay 500000
      -- Verify B does NOT have A in its pool
      poolConn <- atomically $ lookupConn (swConnPool swB) pidA
      case poolConn of
        Nothing -> pure ()
        Just _  -> expectationFailure "connection should not be in pool"
      switchClose swA
      switchClose swB

  describe "Lifecycle" $ do
    it "switchClose cleanly tears down listeners and connections" $ do
      withTestNode $ \sw _pid -> do
        _addrs <- switchListen sw defaultConnectionGater [loopbackAddr]
        -- Verify listening
        listeners <- atomically $ readTVar (swListeners sw)
        length listeners `shouldBe` 1
        -- Close
        switchClose sw
        -- Verify teardown
        listenersAfter <- atomically $ readTVar (swListeners sw)
        length listenersAfter `shouldBe` 0
        closed <- atomically $ readTVar (swClosed sw)
        closed `shouldBe` True

    it "dial after switchClose fails with DialSwitchClosed" $ do
      (pidB, kpB) <- mkTestIdentity
      swB <- newSwitch pidB kpB
      -- Create a switch and close it
      (pidA, kpA) <- mkTestIdentity
      swA <- newSwitch pidA kpA
      tcpA <- newTCPTransport
      addTransport swA tcpA
      switchClose swA
      -- Dial should fail
      result <- dial swA pidB [loopbackAddr]
      case result of
        Left DialSwitchClosed -> pure ()
        Left err -> expectationFailure $ "expected DialSwitchClosed, got: " ++ show err
        Right _ -> expectationFailure "dial should fail after switchClose"
      switchClose swB
