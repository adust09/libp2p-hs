module Test.Network.LibP2P.Protocol.Identify.IdentifySpec (spec) where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
  ( TQueue
  , TMVar
  , atomically
  , newEmptyTMVarIO
  , newTQueueIO
  , putTMVar
  , readTQueue
  , readTVar
  , tryReadTMVar
  , writeTQueue
  )
import Control.Exception (throwIO)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Network.LibP2P.Crypto.Ed25519 (generateKeyPair)
import Network.LibP2P.Crypto.Key (kpPublic)
import Network.LibP2P.Crypto.PeerId (PeerId (..), fromPublicKey)
import Network.LibP2P.Crypto.Protobuf (encodePublicKey)
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import Network.LibP2P.Protocol.Identify.Identify
import Network.LibP2P.Protocol.Identify.Message (IdentifyInfo (..), decodeIdentify, encodeIdentify)
import Network.LibP2P.Switch.Switch (newSwitch, setStreamHandler)
import Network.LibP2P.Switch.Types (Switch (..))
import Data.Word (Word8)
import System.IO.Error (mkIOError, eofErrorType)
import Test.Hspec

-- | Create a test Switch with a key pair.
mkTestSwitch :: IO Switch
mkTestSwitch = do
  Right kp <- generateKeyPair
  let pid = fromPublicKey (kpPublic kp)
  sw <- newSwitch pid kp
  setStreamHandler sw "/test/1.0.0" (\_ _ -> pure ())
  setStreamHandler sw "/test/2.0.0" (\_ _ -> pure ())
  pure sw

-- | Create a stream pair where the writer can signal EOF via streamClose.
-- After close is called, reads on the other side throw an IOError.
mkClosableStreamPair :: IO (StreamIO, StreamIO)
mkClosableStreamPair = do
  qAtoB <- newTQueueIO :: IO (TQueue (Maybe Word8))
  qBtoA <- newTQueueIO :: IO (TQueue (Maybe Word8))
  closedA <- newEmptyTMVarIO :: IO (TMVar ())
  closedB <- newEmptyTMVarIO :: IO (TMVar ())
  let writeQ q closed bs = do
        c <- atomically $ tryReadTMVar closed
        case c of
          Just () -> throwIO (mkIOError eofErrorType "stream closed" Nothing Nothing)
          Nothing -> mapM_ (\b -> atomically $ writeTQueue q (Just b)) (BS.unpack bs)
      readQ q = do
        mv <- atomically $ readTQueue q
        case mv of
          Just b  -> pure b
          Nothing -> throwIO (mkIOError eofErrorType "EOF" Nothing Nothing)
      closeWriter q closed = atomically $ do
        putTMVar closed ()
        writeTQueue q Nothing  -- sentinel for EOF
      streamA = StreamIO
        { streamWrite    = writeQ qAtoB closedA
        , streamReadByte = readQ qBtoA
        , streamClose    = closeWriter qAtoB closedA
        }
      streamB = StreamIO
        { streamWrite    = writeQ qBtoA closedB
        , streamReadByte = readQ qAtoB
        , streamClose    = closeWriter qBtoA closedB
        }
  pure (streamA, streamB)

spec :: Spec
spec = do
  describe "Identify protocol" $ do
    it "buildLocalIdentify includes version strings" $ do
      sw <- mkTestSwitch
      info <- buildLocalIdentify sw Nothing
      idProtocolVersion info `shouldBe` Just "ipfs/0.1.0"
      idAgentVersion info `shouldBe` Just "libp2p-hs/0.1.0"

    it "buildLocalIdentify includes registered protocols" $ do
      sw <- mkTestSwitch
      info <- buildLocalIdentify sw Nothing
      let protos = idProtocols info
      protos `shouldSatisfy` (\ps -> "/test/1.0.0" `elem` ps)
      protos `shouldSatisfy` (\ps -> "/test/2.0.0" `elem` ps)

    it "buildLocalIdentify includes public key" $ do
      sw <- mkTestSwitch
      info <- buildLocalIdentify sw Nothing
      let expectedPubKey = encodePublicKey (kpPublic (swIdentityKey sw))
      idPublicKey info `shouldBe` Just expectedPubKey

    it "handleIdentify sends decodable protobuf over stream" $ do
      sw <- mkTestSwitch
      (streamA, streamB) <- mkClosableStreamPair
      -- handleIdentify writes protobuf to streamA and closes it (EOF)
      writer <- async $ handleIdentify sw streamA (PeerId "remote")
      -- Read all bytes from streamB until EOF
      bytesOrErr <- readUntilEOF streamB 65536
      wait writer
      case bytesOrErr of
        Left err -> expectationFailure $ "readUntilEOF failed: " ++ err
        Right bs -> case decodeIdentify bs of
          Left parseErr -> expectationFailure $ "Decode failed: " ++ show parseErr
          Right info -> do
            idProtocolVersion info `shouldBe` Just "ipfs/0.1.0"
            idAgentVersion info `shouldBe` Just "libp2p-hs/0.1.0"

    it "handleIdentify closes stream after writing (signals EOF)" $ do
      sw <- mkTestSwitch
      closeCalledRef <- newIORef False
      qAtoB <- newTQueueIO :: IO (TQueue (Maybe Word8))
      closedA <- newEmptyTMVarIO :: IO (TMVar ())
      let writeQ q closed bs = do
            c <- atomically $ tryReadTMVar closed
            case c of
              Just () -> throwIO (mkIOError eofErrorType "stream closed" Nothing Nothing)
              Nothing -> mapM_ (\b -> atomically $ writeTQueue q (Just b)) (BS.unpack bs)
          readQ q = do
            mv <- atomically $ readTQueue q
            case mv of
              Just b  -> pure b
              Nothing -> throwIO (mkIOError eofErrorType "EOF" Nothing Nothing)
          closeWriter q closed = atomically $ do
            putTMVar closed ()
            writeTQueue q Nothing
          testStream = StreamIO
            { streamWrite    = writeQ qAtoB closedA
            , streamReadByte = fail "not used in this test"
            , streamClose    = do
                writeIORef closeCalledRef True
                closeWriter qAtoB closedA
            }
          readerStream = StreamIO
            { streamWrite    = \_ -> fail "not used"
            , streamReadByte = readQ qAtoB
            , streamClose    = pure ()
            }
      -- handleIdentify should write + close the stream
      writer <- async $ handleIdentify sw testStream (PeerId "remote")
      bytesOrErr <- readUntilEOF readerStream 65536
      wait writer
      -- Stream close should have been called by handleIdentify
      closeCalled <- readIORef closeCalledRef
      closeCalled `shouldBe` True
      -- And the data should be readable
      case bytesOrErr of
        Right _ -> pure ()
        Left err -> expectationFailure $ "readUntilEOF failed: " ++ err

    it "handleIdentifyPush stores info in peer store" $ do
      sw <- mkTestSwitch
      let remotePeerId = PeerId "push-peer"
      (streamA, streamB) <- mkClosableStreamPair
      let testInfo = IdentifyInfo
            { idProtocolVersion = Just "test/1.0"
            , idAgentVersion    = Just "test-agent/0.1"
            , idPublicKey       = Nothing
            , idListenAddrs     = []
            , idObservedAddr    = Nothing
            , idProtocols       = ["/test/proto"]
            }
      -- Write encoded message then signal EOF
      let encoded = encodeIdentify testInfo
      streamWrite streamA encoded
      streamClose streamA
      -- handleIdentifyPush reads from streamB
      handleIdentifyPush sw streamB remotePeerId
      -- Check peer store
      store <- atomically $ readTVar (swPeerStore sw)
      case Map.lookup remotePeerId store of
        Nothing -> expectationFailure "Expected peer in store"
        Just storedInfo -> do
          idProtocolVersion storedInfo `shouldBe` Just "test/1.0"
          idAgentVersion storedInfo `shouldBe` Just "test-agent/0.1"

    it "registerIdentifyHandlers registers both protocol handlers" $ do
      sw <- mkTestSwitch
      registerIdentifyHandlers sw
      protos <- atomically $ readTVar (swProtocols sw)
      Map.member identifyProtocolId protos `shouldBe` True
      Map.member identifyPushProtocolId protos `shouldBe` True
