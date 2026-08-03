module LibP2P.Protocol.Identify.IdentifySpec (spec) where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
  ( TQueue
  , TMVar
  , atomically
  , newEmptyTMVarIO
  , newTQueueIO
  , newTVarIO
  , putTMVar
  , readTQueue
  , readTVar
  , tryReadTMVar
  , writeTQueue
  )
import Control.Exception (SomeException, catch, throwIO)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (kpPublic)
import LibP2P.Crypto.PeerId (PeerId (..), fromPublicKey)
import LibP2P.Crypto.Protobuf (encodePublicKey)
import LibP2P.Core.Varint (decodeUvarint, encodeUvarint)
import LibP2P.Multiaddr (Multiaddr (..))
import LibP2P.Multiaddr.Codec (encodeProtocols)
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import LibP2P.Protocol.Identify
import LibP2P.Protocol.Identify.Message (IdentifyInfo (..), decodeIdentify, encodeIdentify, maxIdentifySize)
import LibP2P.Switch (newSwitch, setStreamHandler)
import LibP2P.Switch.Types
  ( ConnState (..)
  , Connection (..)
  , Direction (..)
  , MuxerSession (..)
  , Switch (..)
  )
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

-- | Create a dummy upgraded Connection with a known remote multiaddr.
mkTestConnection :: PeerId -> Multiaddr -> IO Connection
mkTestConnection pid remoteAddr = do
  stateVar <- newTVarIO ConnOpen
  pure Connection
    { connPeerId     = pid
    , connDirection  = Inbound
    , connLocalAddr  = Multiaddr [IP4 0x7f000001, TCP 0]
    , connRemoteAddr = remoteAddr
    , connSecurity   = "/noise"
    , connMuxer      = "/yamux/1.0.0"
    , connSession    = MuxerSession
        { muxOpenStream   = fail "test connection: no muxer"
        , muxAcceptStream = fail "test connection: no muxer"
        , muxClose        = pure ()
        }
    , connState      = stateVar
    }

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

-- | Read all raw bytes from a StreamIO until EOF (test helper).
readAllBytes :: StreamIO -> IO (Either String BS.ByteString)
readAllBytes stream = go []
  where
    go acc = do
      result <- (Right <$> streamReadByte stream) `catch`
                (\(_ :: SomeException) -> pure (Left ()))
      case result of
        Left () -> pure (Right (BS.pack (reverse acc)))
        Right b -> go (b : acc)

-- | Prepend the uvarint length prefix to an encoded message (test helper).
frame :: BS.ByteString -> BS.ByteString
frame payload = encodeUvarint (fromIntegral (BS.length payload)) <> payload

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

    it "handleIdentify writes a varint-length-prefixed protobuf" $ do
      sw <- mkTestSwitch
      (streamA, streamB) <- mkClosableStreamPair
      conn <- mkTestConnection (PeerId "remote") (Multiaddr [IP4 0x7f000001, TCP 4001])
      -- handleIdentify writes the framed protobuf to streamA and closes it (EOF)
      writer <- async $ handleIdentify sw conn streamA
      -- Read all raw bytes from streamB until EOF
      bytesOrErr <- readAllBytes streamB
      wait writer
      case bytesOrErr of
        Left err -> expectationFailure $ "reading stream failed: " ++ err
        Right bs -> case decodeUvarint bs of
          Left err -> expectationFailure $ "no varint length prefix: " ++ err
          Right (len, payload) -> do
            -- The varint prefix must describe exactly the protobuf payload
            fromIntegral len `shouldBe` BS.length payload
            case decodeIdentify payload of
              Left parseErr -> expectationFailure $ "Decode failed: " ++ show parseErr
              Right info -> do
                idProtocolVersion info `shouldBe` Just "ipfs/0.1.0"
                idAgentVersion info `shouldBe` Just "libp2p-hs/0.1.0"

    it "handleIdentify populates observedAddr with the connection's remote address" $ do
      sw <- mkTestSwitch
      (streamA, streamB) <- mkClosableStreamPair
      -- The address of the remote peer as seen by us (specs/identify: observedAddr)
      let observedProtos = [IP4 0x7f000001, TCP 45678]
      conn <- mkTestConnection (PeerId "remote") (Multiaddr observedProtos)
      writer <- async $ handleIdentify sw conn streamA
      bytesOrErr <- readAllBytes streamB
      wait writer
      case bytesOrErr of
        Left err -> expectationFailure $ "reading stream failed: " ++ err
        Right bs -> case decodeUvarint bs of
          Left err -> expectationFailure $ "no varint length prefix: " ++ err
          Right (_len, payload) -> case decodeIdentify payload of
            Left parseErr -> expectationFailure $ "Decode failed: " ++ show parseErr
            Right info ->
              idObservedAddr info `shouldBe` Just (encodeProtocols observedProtos)

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
      conn <- mkTestConnection (PeerId "remote") (Multiaddr [IP4 0x7f000001, TCP 4001])
      writer <- async $ handleIdentify sw conn testStream
      bytesOrErr <- readAllBytes readerStream
      wait writer
      -- Stream close should have been called by handleIdentify
      closeCalled <- readIORef closeCalledRef
      closeCalled `shouldBe` True
      -- And the data should be readable
      case bytesOrErr of
        Right _ -> pure ()
        Left err -> expectationFailure $ "reading stream failed: " ++ err

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
      -- Write varint-length-prefixed message then signal EOF
      let encoded = encodeIdentify testInfo
      streamWrite streamA (frame encoded)
      streamClose streamA
      -- handleIdentifyPush reads from streamB
      conn <- mkTestConnection remotePeerId (Multiaddr [IP4 0x7f000001, TCP 4001])
      handleIdentifyPush sw conn streamB
      -- Check peer store
      store <- atomically $ readTVar (swPeerStore sw)
      case Map.lookup remotePeerId store of
        Nothing -> expectationFailure "Expected peer in store"
        Just storedInfo -> do
          idProtocolVersion storedInfo `shouldBe` Just "test/1.0"
          idAgentVersion storedInfo `shouldBe` Just "test-agent/0.1"

    it "readFramedIdentify parses a varint-length-prefixed message" $ do
      (streamA, streamB) <- mkClosableStreamPair
      let testInfo = IdentifyInfo
            { idProtocolVersion = Just "ipfs/0.1.0"
            , idAgentVersion    = Just "framed-agent/1.0"
            , idPublicKey       = Nothing
            , idListenAddrs     = []
            , idObservedAddr    = Nothing
            , idProtocols       = ["/framed/1.0.0"]
            }
      streamWrite streamA (frame (encodeIdentify testInfo))
      result <- readFramedIdentify streamB maxIdentifySize
      result `shouldBe` Right testInfo

    it "readFramedIdentify round-trips encodeFramedIdentify" $ do
      (streamA, streamB) <- mkClosableStreamPair
      let testInfo = IdentifyInfo
            { idProtocolVersion = Just "ipfs/0.1.0"
            , idAgentVersion    = Just "roundtrip/1.0"
            , idPublicKey       = Just (BS.pack [1, 2, 3])
            , idListenAddrs     = [BS.pack [4, 7, 0, 0, 0, 1]]
            , idObservedAddr    = Nothing
            , idProtocols       = ["/a/1.0.0", "/b/1.0.0"]
            }
      streamWrite streamA (encodeFramedIdentify testInfo)
      result <- readFramedIdentify streamB maxIdentifySize
      result `shouldBe` Right testInfo

    it "readFramedIdentify rejects an oversized length prefix" $ do
      (streamA, streamB) <- mkClosableStreamPair
      -- Announce a length just above the limit, without sending a payload
      streamWrite streamA (encodeUvarint (fromIntegral (maxIdentifySize + 1)))
      result <- readFramedIdentify streamB maxIdentifySize
      case result of
        Left err -> err `shouldSatisfy` (not . null)
        Right _  -> expectationFailure "expected oversized message to be rejected"

    it "registerIdentifyHandlers registers both protocol handlers" $ do
      sw <- mkTestSwitch
      registerIdentifyHandlers sw
      protos <- atomically $ readTVar (swProtocols sw)
      Map.member identifyProtocolId protos `shouldBe` True
      Map.member identifyPushProtocolId protos `shouldBe` True
