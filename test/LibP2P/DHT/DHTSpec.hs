module LibP2P.DHT.DHTSpec
  ( spec
  , mkTestNode
  , mkPeerId
  , localPid
  ) where

import Test.Hspec

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import Data.Word (Word8)
import LibP2P.Crypto.Key (KeyPair (..), PublicKey (..), PrivateKey (..), KeyType (..))
import LibP2P.Crypto.PeerId (PeerId (..), peerIdBytes)
import LibP2P.DHT
import LibP2P.DHT.Distance (peerIdToKey, sortByDistance)
import LibP2P.DHT.Message
import LibP2P.DHT.RoutingTable (insertPeer)
import LibP2P.DHT.Types
import LibP2P.Multiaddr (Multiaddr, fromText, toBytes)
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..), negotiateResponder)
import LibP2P.Switch.ConnPool (addConn)
import LibP2P.Switch.Types
  ( ConnState (..)
  , Connection (..)
  , Direction (..)
  , MuxerSession (..)
  , Switch (..)
  )
import LibP2P.Switch.ResourceManager (ResourceManager, newResourceManager, DefaultLimits (..), noLimits)
import System.Timeout (timeout)

-- | Helper: create a PeerId from raw bytes.
mkPeerId :: BS.ByteString -> PeerId
mkPeerId = PeerId

-- | The local peer used for testing.
localPid :: PeerId
localPid = mkPeerId (BS.pack [0])

-- | Remote peer for handler tests.
remotePid :: PeerId
remotePid = mkPeerId (BS.pack [1])

-- | Create a minimal DHTNode for testing (no real Switch).
mkTestNode :: PeerId -> IO DHTNode
mkTestNode pid = do
  sw <- mkMockSwitch pid
  newDHTNode sw DHTServer

-- | Create a mock Switch with just a local peer ID.
mkMockSwitch :: PeerId -> IO Switch
mkMockSwitch pid = do
  transports <- newTVarIO []
  pool <- newTVarIO Map.empty
  protocols <- newTVarIO Map.empty
  events <- newBroadcastTChanIO
  closed <- newTVarIO False
  backoffs <- newTVarIO Map.empty
  pendingDials <- newTVarIO Map.empty
  resMgr <- mkMockResourceMgr
  peerStore <- newTVarIO Map.empty
  notifiers <- newTVarIO []
  listeners <- newTVarIO []
  pure Switch
    { swLocalPeerId  = pid
    , swIdentityKey  = dummyKeyPair
    , swTransports   = transports
    , swConnPool     = pool
    , swProtocols    = protocols
    , swEvents       = events
    , swClosed       = closed
    , swDialBackoffs = backoffs
    , swPendingDials = pendingDials
    , swResourceMgr  = resMgr
    , swPeerStore    = peerStore
    , swNotifiers    = notifiers
    , swListeners    = listeners
    }

-- | Create a mock resource manager with no limits (tests don't need resource enforcement).
mkMockResourceMgr :: IO ResourceManager
mkMockResourceMgr = newResourceManager (DefaultLimits noLimits noLimits)

-- | Independent SHA-256 (not via DHT.Distance) for asserting the spec metric.
sha256 :: BS.ByteString -> BS.ByteString
sha256 bs = convert (hash bs :: Digest SHA256)

-- | Dummy key pair for mock Switch (DHT never accesses identity key).
dummyKeyPair :: KeyPair
dummyKeyPair = KeyPair
  (PublicKey Ed25519 (BS.replicate 32 0))
  (PrivateKey Ed25519 (BS.replicate 64 0))

-- | A test multiaddr for address propagation assertions.
testAddr :: Multiaddr
testAddr = either error id (fromText "/ip4/127.0.0.1/tcp/4001")

-- | Create a stream pair for testing, with close/EOF support.
-- Closing one end makes reads on the other end fail once the in-flight
-- bytes are drained, mimicking a real muxer stream reaching EOF.
mkStreamPair :: IO (StreamIO, StreamIO)
mkStreamPair = do
  q1 <- newTQueueIO :: IO (TQueue Word8)  -- A -> B
  q2 <- newTQueueIO :: IO (TQueue Word8)  -- B -> A
  closedAtoB <- newTVarIO False
  closedBtoA <- newTVarIO False
  let writeAll q bs = mapM_ (\b -> atomically (writeTQueue q b)) (BS.unpack bs)
      readOrEOF q closedVar = atomically $ do
        mb <- tryReadTQueue q
        case mb of
          Just b -> pure b
          Nothing -> do
            closed <- readTVar closedVar
            if closed then throwSTM (userError "stream closed") else retry
      streamA = StreamIO
        { streamWrite = writeAll q1
        , streamReadByte = readOrEOF q2 closedBtoA
        , streamClose = atomically (writeTVar closedAtoB True)
        }
      streamB = StreamIO
        { streamWrite = writeAll q2
        , streamReadByte = readOrEOF q1 closedAtoB
        , streamClose = atomically (writeTVar closedBtoA True)
        }
  pure (streamA, streamB)

-- | A mock Connection that hands out the given stream on the first
-- muxOpenStream call and counts opens; later opens fail so tests can
-- assert that the sender reuses one stream per peer.
mkMockConnection :: PeerId -> StreamIO -> TVar Int -> IO Connection
mkMockConnection pid stream openCountVar = do
  stateVar <- newTVarIO ConnOpen
  handedOut <- newTVarIO False
  let openOnce = do
        first <- atomically $ do
          done <- readTVar handedOut
          if done
            then pure False
            else do
              writeTVar handedOut True
              modifyTVar' openCountVar (+ 1)
              pure True
        if first then pure stream else fail "mock: stream already opened"
  pure Connection
    { connPeerId     = pid
    , connDirection  = Outbound
    , connLocalAddr  = testAddr
    , connRemoteAddr = testAddr
    , connSecurity   = "/noise"
    , connMuxer      = "/yamux/1.0.0"
    , connSession    = MuxerSession
        { muxOpenStream   = openOnce
        , muxAcceptStream = fail "mock: no inbound streams"
        , muxClose        = pure ()
        }
    , connState      = stateVar
    }

spec :: Spec
spec = do
  describe "handleDHTRequest" $ do
    it "FIND_NODE returns closest peers" $ do
      node <- mkTestNode localPid
      now <- getCurrentTime
      let peers = [mkPeerId (BS.pack [i]) | i <- [2..10]]
          entries = map (\pid -> BucketEntry pid (peerIdToKey pid) [] now NotConnected) peers
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        foldl (\r e -> fst (insertPeer e r)) rt entries

      (clientStream, serverStream) <- mkStreamPair
      let request = emptyDHTMessage
            { msgType = FindNode
            , msgKey = BS.pack [42, 42, 42]
            }
      writeFramedMessage clientStream request
      streamClose clientStream
      handleDHTRequest node serverStream remotePid

      result <- readFramedMessage clientStream maxDHTMessageSize
      case result of
        Right resp -> do
          msgType resp `shouldBe` FindNode
          length (msgCloserPeers resp) `shouldSatisfy` (> 0)
        Left err -> expectationFailure $ "Failed to read response: " ++ err

    it "FIND_NODE with unknown target returns whatever is closest" $ do
      node <- mkTestNode localPid
      now <- getCurrentTime
      let pid2 = mkPeerId (BS.pack [2])
          entry = BucketEntry pid2 (peerIdToKey pid2) [] now NotConnected
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        fst (insertPeer entry rt)

      (clientStream, serverStream) <- mkStreamPair
      let request = emptyDHTMessage
            { msgType = FindNode
            , msgKey = BS.pack [0xFF, 0xFF]
            }
      writeFramedMessage clientStream request
      streamClose clientStream
      handleDHTRequest node serverStream remotePid

      result <- readFramedMessage clientStream maxDHTMessageSize
      case result of
        -- Issue #173: the old assertion here was `length >= 0`, a
        -- tautology. The only entry in the table is pid2, and an unknown
        -- target must still yield it as the closest peer.
        Right resp -> map dhtPeerId (msgCloserPeers resp) `shouldBe` [peerIdBytes pid2]
        Left err -> expectationFailure $ "Failed: " ++ err

    -- Per specs/kad-dht: "the distance between two keys is
    -- XOR(sha256(key1), sha256(key2))". The wire key arrives raw and must be
    -- hashed before comparing against routing-table entries (which cache
    -- sha256 of the peer ID). Wrapping raw wire bytes in DHTKey (the old
    -- behaviour) sorts by a different, non-spec metric.
    it "FIND_NODE orders closerPeers by XOR distance over SHA-256 of the wire key" $ do
      node <- mkTestNode localPid
      now <- getCurrentTime
      let peers = [mkPeerId (BS.pack [i]) | i <- [2..10]]
          entries = map (\pid -> BucketEntry pid (peerIdToKey pid) [] now NotConnected) peers
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        foldl (\r e -> fst (insertPeer e r)) rt entries

      -- Raw wire key, as a remote peer would send it (a binary peer ID).
      let wireKey = BS.pack [42, 42, 42]
          hashedTarget = DHTKey (sha256 wireKey)
          expectedOrder = map (peerIdBytes . entryPeerId) (sortByDistance hashedTarget entries)

      (clientStream, serverStream) <- mkStreamPair
      let request = emptyDHTMessage { msgType = FindNode, msgKey = wireKey }
      writeFramedMessage clientStream request
      streamClose clientStream
      handleDHTRequest node serverStream remotePid

      result <- readFramedMessage clientStream maxDHTMessageSize
      case result of
        Right resp -> map dhtPeerId (msgCloserPeers resp) `shouldBe` expectedOrder
        Left err -> expectationFailure $ "Failed: " ++ err

    it "GET_VALUE orders closerPeers by XOR distance over SHA-256 of the wire key" $ do
      node <- mkTestNode localPid
      now <- getCurrentTime
      let peers = [mkPeerId (BS.pack [i]) | i <- [2..10]]
          entries = map (\pid -> BucketEntry pid (peerIdToKey pid) [] now NotConnected) peers
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        foldl (\r e -> fst (insertPeer e r)) rt entries

      let wireKey = BS.pack [0xCA, 0xFE, 0x01]
          hashedTarget = DHTKey (sha256 wireKey)
          expectedOrder = map (peerIdBytes . entryPeerId) (sortByDistance hashedTarget entries)

      (clientStream, serverStream) <- mkStreamPair
      let request = emptyDHTMessage { msgType = GetValue, msgKey = wireKey }
      writeFramedMessage clientStream request
      streamClose clientStream
      handleDHTRequest node serverStream remotePid

      result <- readFramedMessage clientStream maxDHTMessageSize
      case result of
        Right resp -> map dhtPeerId (msgCloserPeers resp) `shouldBe` expectedOrder
        Left err -> expectationFailure $ "Failed: " ++ err

    it "GET_PROVIDERS orders closerPeers by XOR distance over SHA-256 of the wire key" $ do
      node <- mkTestNode localPid
      now <- getCurrentTime
      let peers = [mkPeerId (BS.pack [i]) | i <- [2..10]]
          entries = map (\pid -> BucketEntry pid (peerIdToKey pid) [] now NotConnected) peers
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        foldl (\r e -> fst (insertPeer e r)) rt entries

      let wireKey = BS.pack [0xDD, 0xEE]
          hashedTarget = DHTKey (sha256 wireKey)
          expectedOrder = map (peerIdBytes . entryPeerId) (sortByDistance hashedTarget entries)

      (clientStream, serverStream) <- mkStreamPair
      let request = emptyDHTMessage { msgType = GetProviders, msgKey = wireKey }
      writeFramedMessage clientStream request
      streamClose clientStream
      handleDHTRequest node serverStream remotePid

      result <- readFramedMessage clientStream maxDHTMessageSize
      case result of
        Right resp -> map dhtPeerId (msgCloserPeers resp) `shouldBe` expectedOrder
        Left err -> expectationFailure $ "Failed: " ++ err

    it "GET_VALUE with stored record returns it" $ do
      node <- mkTestNode localPid
      let key = BS.pack [0xCA, 0xFE]
          rec = DHTRecord key (BS.pack [0xDE, 0xAD]) "2024-01-01T00:00:00Z"
      storeRecord node rec

      (clientStream, serverStream) <- mkStreamPair
      let request = emptyDHTMessage { msgType = GetValue, msgKey = key }
      writeFramedMessage clientStream request
      streamClose clientStream
      handleDHTRequest node serverStream remotePid

      result <- readFramedMessage clientStream maxDHTMessageSize
      case result of
        Right resp -> msgRecord resp `shouldBe` Just rec
        Left err -> expectationFailure $ "Failed: " ++ err

    it "GET_VALUE without record returns closerPeers only" $ do
      node <- mkTestNode localPid
      (clientStream, serverStream) <- mkStreamPair
      let request = emptyDHTMessage { msgType = GetValue, msgKey = BS.pack [1, 2, 3] }
      writeFramedMessage clientStream request
      streamClose clientStream
      handleDHTRequest node serverStream remotePid

      result <- readFramedMessage clientStream maxDHTMessageSize
      case result of
        Right resp -> do
          msgRecord resp `shouldBe` Nothing
          msgType resp `shouldBe` GetValue
        Left err -> expectationFailure $ "Failed: " ++ err

    it "PUT_VALUE stores record" $ do
      node <- mkTestNode localPid
      let key = BS.pack [0xBE, 0xEF]
          rec = DHTRecord key (BS.pack [1, 2, 3]) "2024-06-15T12:00:00Z"

      (clientStream, serverStream) <- mkStreamPair
      let request = emptyDHTMessage
            { msgType = PutValue
            , msgKey = key
            , msgRecord = Just rec
            }
      writeFramedMessage clientStream request
      streamClose clientStream
      handleDHTRequest node serverStream remotePid
      _ <- readFramedMessage clientStream maxDHTMessageSize

      stored <- lookupRecord node key
      stored `shouldBe` Just rec

    it "ADD_PROVIDER rejects mismatched sender" $ do
      node <- mkTestNode localPid
      let key = BS.pack [0xAA]
          fakePeer = DHTPeer (BS.pack [99, 99]) [] Connected

      (clientStream, serverStream) <- mkStreamPair
      let request = emptyDHTMessage
            { msgType = AddProvider
            , msgKey = key
            , msgProviderPeers = [fakePeer]
            }
      writeFramedMessage clientStream request
      streamClose clientStream
      handleDHTRequest node serverStream remotePid

      result <- readFramedMessage clientStream maxDHTMessageSize
      case result of
        Right resp -> msgType resp `shouldBe` AddProvider
        Left err -> expectationFailure $ "Failed: " ++ err

      -- Mismatched provider must NOT be stored
      stored <- getProviders node key
      stored `shouldBe` []

    it "ADD_PROVIDER with valid sender persists provider record" $ do
      node <- mkTestNode localPid
      let key = BS.pack [0xAA, 0xBB]
          -- remotePid has raw bytes [1], so dhtPeerId must match
          validPeer = DHTPeer (peerIdBytes remotePid) [] Connected

      (clientStream, serverStream) <- mkStreamPair
      let request = emptyDHTMessage
            { msgType = AddProvider
            , msgKey = key
            , msgProviderPeers = [validPeer]
            }
      writeFramedMessage clientStream request
      streamClose clientStream
      handleDHTRequest node serverStream remotePid

      result <- readFramedMessage clientStream maxDHTMessageSize
      case result of
        Right resp -> msgType resp `shouldBe` AddProvider
        Left err -> expectationFailure $ "Failed: " ++ err

      -- Provider should now be persisted
      stored <- getProviders node key
      length stored `shouldBe` 1
      peProvider (head stored) `shouldBe` remotePid

    it "ADD_PROVIDER round-trip via GET_PROVIDERS" $ do
      node <- mkTestNode localPid
      let key = BS.pack [0xCC, 0xDD]
          validPeer = DHTPeer (peerIdBytes remotePid) [] Connected

      -- Send ADD_PROVIDER
      (clientStream1, serverStream1) <- mkStreamPair
      let addReq = emptyDHTMessage
            { msgType = AddProvider
            , msgKey = key
            , msgProviderPeers = [validPeer]
            }
      writeFramedMessage clientStream1 addReq
      streamClose clientStream1
      handleDHTRequest node serverStream1 remotePid
      _ <- readFramedMessage clientStream1 maxDHTMessageSize

      -- Send GET_PROVIDERS for the same key
      (clientStream2, serverStream2) <- mkStreamPair
      let getReq = emptyDHTMessage
            { msgType = GetProviders
            , msgKey = key
            }
      writeFramedMessage clientStream2 getReq
      streamClose clientStream2
      handleDHTRequest node serverStream2 remotePid

      result <- readFramedMessage clientStream2 maxDHTMessageSize
      case result of
        Right resp -> do
          msgType resp `shouldBe` GetProviders
          length (msgProviderPeers resp) `shouldBe` 1
          dhtPeerId (head (msgProviderPeers resp)) `shouldBe` peerIdBytes remotePid
        Left err -> expectationFailure $ "Failed: " ++ err

    -- Issue #147: specs/kad-dht requires handling additional RPC request
    -- messages on the same inbound stream — go-libp2p keeps one long-lived
    -- stream per peer and pipelines requests over it.
    it "serves multiple consecutive requests on a single stream" $ do
      node <- mkTestNode localPid
      let key = BS.pack [0xCA, 0xFE]
          rec = DHTRecord key (BS.pack [0xDE, 0xAD]) "2024-01-01T00:00:00Z"
      storeRecord node rec

      (clientStream, serverStream) <- mkStreamPair
      handler <- async (handleDHTRequest node serverStream remotePid)

      writeFramedMessage clientStream
        (emptyDHTMessage { msgType = FindNode, msgKey = BS.pack [1] })
      r1 <- timeout 2000000 (readFramedMessage clientStream maxDHTMessageSize)
      case r1 of
        Just (Right resp) -> msgType resp `shouldBe` FindNode
        _ -> expectationFailure "no response to first request"

      writeFramedMessage clientStream
        (emptyDHTMessage { msgType = GetValue, msgKey = key })
      r2 <- timeout 2000000 (readFramedMessage clientStream maxDHTMessageSize)
      case r2 of
        Just (Right resp) -> msgRecord resp `shouldBe` Just rec
        _ -> expectationFailure
               "handler did not serve a second request on the same stream"

      streamClose clientStream
      wait handler

    -- Issue #147: Peer records must carry the peer's known multiaddrs so
    -- the requester can dial them (go-libp2p filters address-less peers).
    it "FIND_NODE returns closerPeers with their known multiaddrs" $ do
      node <- mkTestNode localPid
      now <- getCurrentTime
      let pid2 = mkPeerId (BS.pack [2])
          entry = BucketEntry pid2 (peerIdToKey pid2) [testAddr] now Connected
      atomically $ modifyTVar' (dhtRoutingTable node) $ \rt ->
        fst (insertPeer entry rt)

      (clientStream, serverStream) <- mkStreamPair
      let request = emptyDHTMessage { msgType = FindNode, msgKey = BS.pack [42] }
      writeFramedMessage clientStream request
      streamClose clientStream
      handleDHTRequest node serverStream remotePid

      result <- readFramedMessage clientStream maxDHTMessageSize
      case result of
        Right resp -> map dhtPeerAddrs (msgCloserPeers resp)
                        `shouldBe` [[toBytes testAddr]]
        Left err -> expectationFailure $ "Failed: " ++ err

    it "GET_PROVIDERS returns provider peers with their multiaddrs" $ do
      node <- mkTestNode localPid
      now <- getCurrentTime
      let key = BS.pack [0xAB]
      addProvider node key (ProviderEntry (mkPeerId (BS.pack [7])) [testAddr] now)

      (clientStream, serverStream) <- mkStreamPair
      let request = emptyDHTMessage { msgType = GetProviders, msgKey = key }
      writeFramedMessage clientStream request
      streamClose clientStream
      handleDHTRequest node serverStream remotePid

      result <- readFramedMessage clientStream maxDHTMessageSize
      case result of
        Right resp -> map dhtPeerAddrs (msgProviderPeers resp)
                        `shouldBe` [[toBytes testAddr]]
        Left err -> expectationFailure $ "Failed: " ++ err

    it "ADD_PROVIDER decodes provider multiaddrs into the provider store" $ do
      node <- mkTestNode localPid
      let key = BS.pack [0xAA, 0xCC]
          validPeer = DHTPeer (peerIdBytes remotePid) [toBytes testAddr] Connected

      (clientStream, serverStream) <- mkStreamPair
      let request = emptyDHTMessage
            { msgType = AddProvider
            , msgKey = key
            , msgProviderPeers = [validPeer]
            }
      writeFramedMessage clientStream request
      streamClose clientStream
      handleDHTRequest node serverStream remotePid

      stored <- getProviders node key
      map peAddrs stored `shouldBe` [[testAddr]]

    it "registerDHTHandler registers protocol on Switch" $ do
      node <- mkTestNode localPid
      registerDHTHandler node
      protos <- readTVarIO (swProtocols (dhtSwitch node))
      Map.member dhtProtocolId protos `shouldBe` True

  -- Issue #168: newDHTNode must wire an outbound sender that opens a
  -- /ipfs/kad/1.0.0 stream via the Switch. Previously the default sender
  -- was a permanent failure and only a test mock ever assigned the field.
  describe "dhtSendRequest (production wiring)" $ do
    it "opens a stream via the Switch, negotiates the protocol, and exchanges framed messages, reusing the stream" $ do
      clientNode <- mkTestNode localPid
      serverNode <- mkTestNode remotePid
      (clientEnd, serverEnd) <- mkStreamPair
      openCountVar <- newTVarIO (0 :: Int)
      conn <- mkMockConnection remotePid clientEnd openCountVar
      atomically $ addConn (swConnPool (dhtSwitch clientNode)) conn

      -- Remote side: multistream-select responder, then the DHT handler.
      server <- async $ do
        _ <- negotiateResponder serverEnd [dhtProtocolId]
        handleDHTRequest serverNode serverEnd localPid

      let req1 = emptyDHTMessage { msgType = FindNode, msgKey = BS.pack [42] }
      r1 <- timeout 2000000 (dhtSendRequest clientNode remotePid req1)
      case r1 of
        Just (Right resp) -> msgType resp `shouldBe` FindNode
        Just (Left err) -> expectationFailure $ "send failed: " ++ err
        Nothing -> expectationFailure "send timed out"

      -- A second request must reuse the cached stream: the mock connection
      -- only allows a single muxOpenStream call.
      let key = BS.pack [0xCA]
          rec = DHTRecord key (BS.pack [1]) "2024-01-01T00:00:00Z"
      storeRecord serverNode rec
      let req2 = emptyDHTMessage { msgType = GetValue, msgKey = key }
      r2 <- timeout 2000000 (dhtSendRequest clientNode remotePid req2)
      case r2 of
        Just (Right resp) -> msgRecord resp `shouldBe` Just rec
        Just (Left err) -> expectationFailure $ "second send failed: " ++ err
        Nothing -> expectationFailure "second send timed out"

      opens <- readTVarIO openCountVar
      opens `shouldBe` 1

      streamClose clientEnd
      wait server

    it "fails with Left when there is no connection to the peer" $ do
      node <- mkTestNode localPid
      let request = emptyDHTMessage { msgType = FindNode, msgKey = BS.pack [1] }
      result <- dhtSendRequest node (mkPeerId (BS.pack [9])) request
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected failure without a connection"

  describe "Store operations" $ do
    it "storeRecord + lookupRecord round-trip" $ do
      node <- mkTestNode localPid
      let key = BS.pack [1, 2, 3]
          rec = DHTRecord key (BS.pack [4, 5, 6]) "2024-01-01T00:00:00Z"
      storeRecord node rec
      result <- lookupRecord node key
      result `shouldBe` Just rec

    it "lookupRecord for missing key returns Nothing" $ do
      node <- mkTestNode localPid
      result <- lookupRecord node (BS.pack [99])
      result `shouldBe` Nothing

    it "addProvider + getProviders round-trip" $ do
      node <- mkTestNode localPid
      now <- getCurrentTime
      let key = BS.pack [0xAA, 0xBB]
          provider = ProviderEntry (mkPeerId (BS.pack [5])) [] now
      addProvider node key provider
      result <- getProviders node key
      length result `shouldBe` 1

    it "getProviders for missing key returns []" $ do
      node <- mkTestNode localPid
      result <- getProviders node (BS.pack [0xFF])
      result `shouldBe` []
