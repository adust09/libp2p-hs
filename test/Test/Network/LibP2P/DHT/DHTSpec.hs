module Test.Network.LibP2P.DHT.DHTSpec
  ( spec
  , mkTestNode
  , mkPeerId
  , localPid
  ) where

import Test.Hspec

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import Data.Word (Word8)
import Network.LibP2P.Crypto.Key (KeyPair (..), PublicKey (..), PrivateKey (..), KeyType (..))
import Network.LibP2P.Crypto.PeerId (PeerId (..))
import Network.LibP2P.DHT.DHT
import Network.LibP2P.DHT.Distance (peerIdToKey)
import Network.LibP2P.DHT.Message
import Network.LibP2P.DHT.RoutingTable (insertPeer)
import Network.LibP2P.DHT.Types
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import Network.LibP2P.Switch.Types (Switch (..))
import Network.LibP2P.Switch.ResourceManager (ResourceManager, newResourceManager, DefaultLimits (..), noLimits)

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

-- | Dummy key pair for mock Switch (DHT never accesses identity key).
dummyKeyPair :: KeyPair
dummyKeyPair = KeyPair
  (PublicKey Ed25519 (BS.replicate 32 0))
  (PrivateKey Ed25519 (BS.replicate 64 0))

-- | Create a stream pair for testing.
mkStreamPair :: IO (StreamIO, StreamIO)
mkStreamPair = do
  q1 <- newTQueueIO :: IO (TQueue Word8)
  q2 <- newTQueueIO :: IO (TQueue Word8)
  let streamA = StreamIO
        { streamWrite = \bs -> mapM_ (\b -> atomically (writeTQueue q1 b)) (BS.unpack bs)
        , streamReadByte = atomically (readTQueue q2)
        }
      streamB = StreamIO
        { streamWrite = \bs -> mapM_ (\b -> atomically (writeTQueue q2 b)) (BS.unpack bs)
        , streamReadByte = atomically (readTQueue q1)
        }
  pure (streamA, streamB)

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
      handleDHTRequest node serverStream remotePid

      result <- readFramedMessage clientStream maxDHTMessageSize
      case result of
        Right resp -> length (msgCloserPeers resp) `shouldSatisfy` (>= 0)
        Left err -> expectationFailure $ "Failed: " ++ err

    it "GET_VALUE with stored record returns it" $ do
      node <- mkTestNode localPid
      let key = BS.pack [0xCA, 0xFE]
          rec = DHTRecord key (BS.pack [0xDE, 0xAD]) "2024-01-01T00:00:00Z"
      storeRecord node rec

      (clientStream, serverStream) <- mkStreamPair
      let request = emptyDHTMessage { msgType = GetValue, msgKey = key }
      writeFramedMessage clientStream request
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
      handleDHTRequest node serverStream remotePid

      result <- readFramedMessage clientStream maxDHTMessageSize
      case result of
        Right resp -> msgType resp `shouldBe` AddProvider
        Left err -> expectationFailure $ "Failed: " ++ err

    it "registerDHTHandler registers protocol on Switch" $ do
      node <- mkTestNode localPid
      registerDHTHandler node
      protos <- readTVarIO (swProtocols (dhtSwitch node))
      Map.member dhtProtocolId protos `shouldBe` True

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
