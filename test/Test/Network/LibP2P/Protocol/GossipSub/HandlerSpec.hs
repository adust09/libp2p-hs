-- | Tests for GossipSub Switch Handler (Phase 10b).
--
-- Tests the Handler module that bridges Router ↔ Switch for persistent
-- bidirectional RPC streams.
module Test.Network.LibP2P.Protocol.GossipSub.HandlerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
  ( atomically
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Network.LibP2P.Crypto.Ed25519 (generateKeyPair)
import Network.LibP2P.Crypto.Key (KeyPair, publicKey)
import Network.LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import Network.LibP2P.MultistreamSelect.Negotiation
  ( NegotiationResult (..)
  , StreamIO (..)
  , mkMemoryStreamPair
  , negotiateResponder
  )
import Network.LibP2P.Protocol.GossipSub.Handler
  ( GossipSubNode (..)
  , gossipJoin
  , gossipLeave
  , gossipPublish
  , gossipSubProtocolId
  , handleGossipSubStream
  , newGossipSubNode
  , startGossipSub
  , stopGossipSub
  )
import Network.LibP2P.Protocol.GossipSub.Message
  ( readRPCMessage
  , writeRPCMessage
  )
import Network.LibP2P.Protocol.GossipSub.Router (addPeer)
import Network.LibP2P.Protocol.GossipSub.Types
  ( GossipSubParams (..)
  , GossipSubRouter (..)
  , PeerProtocol (..)
  , PubSubMessage (..)
  , RPC (..)
  , SubOpts (..)
  , Topic
  , defaultGossipSubParams
  , maxRPCSize
  )
import Network.LibP2P.Switch.Switch (lookupStreamHandler, newSwitch, setStreamHandler)
import Network.LibP2P.Switch.Types (Switch (..))
import System.Timeout (timeout)
import Test.Hspec
import Data.Time.Clock (getCurrentTime)

-- | Generate a test identity (PeerId, KeyPair).
mkTestIdentity :: IO (PeerId, KeyPair)
mkTestIdentity = do
  Right kp <- generateKeyPair
  let pid = fromPublicKey (publicKey kp)
  pure (pid, kp)

-- | Create a test Switch (no transport needed for handler unit tests).
mkTestSwitch :: IO (Switch, PeerId)
mkTestSwitch = do
  (pid, kp) <- mkTestIdentity
  sw <- newSwitch pid kp
  pure (sw, pid)

-- | Simple params with short heartbeat (1s) for testing.
testParams :: GossipSubParams
testParams = defaultGossipSubParams
  { paramHeartbeatInterval = 10.0  -- Long interval so heartbeat doesn't interfere
  }

-- | Empty RPC for testing.
emptyRPC :: RPC
emptyRPC = RPC
  { rpcSubscriptions = []
  , rpcPublish       = []
  , rpcControl       = Nothing
  }

-- | Create an RPC with a subscription announcement.
subscribeRPC :: Topic -> RPC
subscribeRPC topic = emptyRPC
  { rpcSubscriptions = [SubOpts True topic]
  }

-- | Create an RPC with an unsubscription announcement.
unsubscribeRPC :: Topic -> RPC
unsubscribeRPC topic = emptyRPC
  { rpcSubscriptions = [SubOpts False topic]
  }

spec :: Spec
spec = do
  describe "newGossipSubNode" $ do
    it "creates node with empty state" $ do
      (sw, _pid) <- mkTestSwitch
      node <- newGossipSubNode sw testParams
      streams <- atomically $ readTVar (gsnStreams node)
      Map.null streams `shouldBe` True
      hb <- atomically $ readTVar (gsnHeartbeat node)
      case hb of
        Nothing -> pure ()
        Just _  -> expectationFailure "heartbeat should not be running"

  describe "handleGossipSubStream" $ do
    it "processes subscription RPCs via memory streams" $ do
      (sw, _localPid) <- mkTestSwitch
      node <- newGossipSubNode sw testParams
      (remotePid, _kp) <- mkTestIdentity
      -- Create memory stream pair: remote side writes, handler reads
      (remoteStream, handlerStream) <- mkMemoryStreamPair
      -- Spawn handler in background (blocks on read loop)
      _ <- async $ handleGossipSubStream node handlerStream remotePid
      -- Send a subscription RPC from remote
      writeRPCMessage remoteStream (subscribeRPC "test-topic")
      -- Give handler time to process
      threadDelay 200000
      -- Verify peer is registered in router
      peers <- atomically $ readTVar (gsPeers (gsnRouter node))
      Map.member remotePid peers `shouldBe` True

    it "processes publish RPCs and delivers to onMessage callback" $ do
      (sw, _localPid) <- mkTestSwitch
      node <- newGossipSubNode sw testParams
      (remotePid, _kp) <- mkTestIdentity
      -- Set up message callback
      msgMVar <- newEmptyMVar
      atomically $ writeTVar (gsOnMessage (gsnRouter node))
        (\topic msg -> putMVar msgMVar (topic, msgData msg))
      -- Subscribe the router to the topic so it accepts messages
      gossipJoin node "pub-topic"
      -- Register remote peer and subscribe them
      now <- getCurrentTime
      addPeer (gsnRouter node) remotePid GossipSubPeer False now
      (remoteStream, handlerStream) <- mkMemoryStreamPair
      -- Spawn handler
      _ <- async $ handleGossipSubStream node handlerStream remotePid
      -- Send a publish RPC
      let msg = PubSubMessage
            { msgFrom      = Just (BS.pack [1,2,3])
            , msgData      = "hello gossipsub"
            , msgSeqNo     = Just (BS.pack [0,0,0,0,0,0,0,1])
            , msgTopic     = "pub-topic"
            , msgSignature = Nothing
            , msgKey       = Nothing
            }
      writeRPCMessage remoteStream (emptyRPC { rpcPublish = [msg] })
      -- Wait for callback
      result <- timeout 2000000 $ takeMVar msgMVar
      case result of
        Nothing -> expectationFailure "onMessage callback not invoked"
        Just (topic, dat) -> do
          topic `shouldBe` "pub-topic"
          dat `shouldBe` "hello gossipsub"

  describe "sendRPC" $ do
    it "opens outbound stream and writes framed RPC" $ do
      (sw, _localPid) <- mkTestSwitch
      node <- newGossipSubNode sw testParams
      (remotePid, _kp) <- mkTestIdentity
      -- Simulate: manually set up a mock connection in the pool
      -- For unit testing sendRPC, we inject a stream directly
      (nodeStream, remoteReadStream) <- mkMemoryStreamPair
      atomically $ do
        streams <- readTVar (gsnStreams node)
        writeTVar (gsnStreams node) (Map.insert remotePid nodeStream streams)
      -- Send RPC via the cached stream
      let rpc = subscribeRPC "outbound-topic"
      gsSendRPC (gsnRouter node) remotePid rpc
      -- Read from the other side
      result <- timeout 2000000 $ readRPCMessage remoteReadStream maxRPCSize
      case result of
        Nothing -> expectationFailure "timeout reading RPC"
        Just (Left err) -> expectationFailure $ "decode error: " ++ err
        Just (Right received) -> do
          rpcSubscriptions received `shouldBe` [SubOpts True "outbound-topic"]

    it "reuses cached stream on second call" $ do
      (sw, _localPid) <- mkTestSwitch
      node <- newGossipSubNode sw testParams
      (remotePid, _kp) <- mkTestIdentity
      -- Pre-cache a stream
      (nodeStream, remoteReadStream) <- mkMemoryStreamPair
      atomically $ do
        streams <- readTVar (gsnStreams node)
        writeTVar (gsnStreams node) (Map.insert remotePid nodeStream streams)
      -- Send two RPCs
      gsSendRPC (gsnRouter node) remotePid (subscribeRPC "topic1")
      gsSendRPC (gsnRouter node) remotePid (subscribeRPC "topic2")
      -- Read both from remote side
      r1 <- timeout 2000000 $ readRPCMessage remoteReadStream maxRPCSize
      r2 <- timeout 2000000 $ readRPCMessage remoteReadStream maxRPCSize
      case (r1, r2) of
        (Just (Right rpc1), Just (Right rpc2)) -> do
          rpcSubscriptions rpc1 `shouldBe` [SubOpts True "topic1"]
          rpcSubscriptions rpc2 `shouldBe` [SubOpts True "topic2"]
        _ -> expectationFailure "failed to read both RPCs"

  describe "startGossipSub" $ do
    it "registers handler and starts heartbeat" $ do
      (sw, _pid) <- mkTestSwitch
      node <- newGossipSubNode sw testParams
      -- Before start: no handler, no heartbeat
      handlerBefore <- lookupStreamHandler sw gossipSubProtocolId
      case handlerBefore of
        Nothing -> pure ()
        Just _  -> expectationFailure "handler should not be registered before start"
      hbBefore <- atomically $ readTVar (gsnHeartbeat node)
      case hbBefore of
        Nothing -> pure ()
        Just _  -> expectationFailure "heartbeat should not be running before start"
      -- Start
      startGossipSub node
      -- After start: handler registered, heartbeat running
      handlerAfter <- lookupStreamHandler sw gossipSubProtocolId
      case handlerAfter of
        Nothing -> expectationFailure "handler should be registered"
        Just _  -> pure ()
      hbAfter <- atomically $ readTVar (gsnHeartbeat node)
      case hbAfter of
        Nothing -> expectationFailure "heartbeat should be running"
        Just _  -> pure ()
      -- Cleanup
      stopGossipSub node

  describe "stopGossipSub" $ do
    it "cancels heartbeat and unregisters handler" $ do
      (sw, _pid) <- mkTestSwitch
      node <- newGossipSubNode sw testParams
      startGossipSub node
      -- Verify running state
      hbRunning <- atomically $ readTVar (gsnHeartbeat node)
      case hbRunning of
        Nothing -> expectationFailure "heartbeat should be running after start"
        Just _  -> pure ()
      -- Stop
      stopGossipSub node
      -- Verify stopped state
      handlerAfter <- lookupStreamHandler sw gossipSubProtocolId
      case handlerAfter of
        Nothing -> pure ()
        Just _  -> expectationFailure "handler should be unregistered after stop"
      hbAfter <- atomically $ readTVar (gsnHeartbeat node)
      case hbAfter of
        Nothing -> pure ()
        Just _  -> expectationFailure "heartbeat should not be running after stop"

  describe "Two-node exchange" $ do
    it "two nodes exchange subscription announcements via memory streams" $ do
      -- Node A
      (swA, pidA) <- mkTestSwitch
      nodeA <- newGossipSubNode swA testParams
      -- Node B
      (swB, pidB) <- mkTestSwitch
      nodeB <- newGossipSubNode swB testParams
      -- Create paired streams (A→B and B→A)
      (streamAtoB, streamBfromA) <- mkMemoryStreamPair
      (streamBtoA, streamAfromB) <- mkMemoryStreamPair
      -- Spawn stream handlers
      _ <- async $ handleGossipSubStream nodeB streamBfromA pidA
      _ <- async $ handleGossipSubStream nodeA streamAfromB pidB
      -- Inject cached outbound streams for sendRPC
      atomically $ do
        writeTVar (gsnStreams nodeA) (Map.singleton pidB streamAtoB)
        writeTVar (gsnStreams nodeB) (Map.singleton pidA streamBtoA)
      -- Node A subscribes to topic, which sends subscription RPCs
      gossipJoin nodeA "shared-topic"
      -- Give time for RPCs to propagate
      threadDelay 500000
      -- Node B should see A's subscription
      peersB <- atomically $ readTVar (gsPeers (gsnRouter nodeB))
      case Map.lookup pidA peersB of
        Nothing -> expectationFailure "nodeB should know about nodeA"
        Just _  -> pure ()  -- Peer registered by handleGossipSubStream

    it "publish on one node delivers message to the other" $ do
      -- Node A (publisher)
      (swA, pidA) <- mkTestSwitch
      nodeA <- newGossipSubNode swA testParams
      -- Node B (subscriber)
      (swB, pidB) <- mkTestSwitch
      nodeB <- newGossipSubNode swB testParams
      -- Set up message callback on Node B
      msgMVar <- newEmptyMVar
      atomically $ writeTVar (gsOnMessage (gsnRouter nodeB))
        (\topic msg -> putMVar msgMVar (topic, msgData msg))
      -- Create paired streams
      (streamAtoB, streamBfromA) <- mkMemoryStreamPair
      (streamBtoA, streamAfromB) <- mkMemoryStreamPair
      -- Spawn stream handlers (registers peers in each router)
      _ <- async $ handleGossipSubStream nodeB streamBfromA pidA
      _ <- async $ handleGossipSubStream nodeA streamAfromB pidB
      -- Inject cached outbound streams
      atomically $ do
        writeTVar (gsnStreams nodeA) (Map.singleton pidB streamAtoB)
        writeTVar (gsnStreams nodeB) (Map.singleton pidA streamBtoA)
      -- Let handlers register peers
      threadDelay 200000
      -- Both subscribe to the topic (this notifies known peers)
      gossipJoin nodeA "pubtest"
      gossipJoin nodeB "pubtest"
      threadDelay 500000  -- Let subscription RPCs propagate
      -- Verify: nodeA's router should know B subscribes to "pubtest"
      -- (because B's join sent subscription RPC to A via stream)
      -- Now publish from A. With floodPublish=True, publish sends to
      -- ALL peers subscribed to the topic. B should receive it.
      gossipPublish nodeA "pubtest" "hello from A"
      -- Wait for B to receive via handleRPC -> onMessage callback
      result <- timeout 3000000 $ takeMVar msgMVar
      case result of
        Nothing -> expectationFailure "nodeB did not receive message"
        Just (topic, dat) -> do
          topic `shouldBe` "pubtest"
          dat `shouldBe` "hello from A"
