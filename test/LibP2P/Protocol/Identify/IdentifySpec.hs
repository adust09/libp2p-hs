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
  , writeTVar
  )
import Control.Exception (SomeException, catch, throwIO)
import qualified Data.ByteString as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (kpPublic)
import LibP2P.Crypto.PeerId (PeerId (..), fromPublicKey)
import LibP2P.Crypto.Protobuf (encodePublicKey)
import LibP2P.Core.Varint (decodeUvarint, encodeUvarint)
import LibP2P.Multiaddr (Multiaddr (..))
import LibP2P.Multiaddr.Codec (encodeProtocols)
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.MultistreamSelect.Negotiation
  ( NegotiationResult (..)
  , StreamIO (..)
  , negotiateResponder
  )
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

-- | Wrap a StreamIO so closing it flips the flag (before delegating).
recordClose :: IORef Bool -> StreamIO -> StreamIO
recordClose ref s = s { streamClose = writeIORef ref True >> streamClose s }

-- | An IdentifyInfo with every optional field absent and every
-- repeated field empty (what an empty push message decodes to).
emptyInfo :: IdentifyInfo
emptyInfo = IdentifyInfo
  { idProtocolVersion = Nothing
  , idAgentVersion    = Nothing
  , idPublicKey       = Nothing
  , idListenAddrs     = []
  , idObservedAddr    = Nothing
  , idProtocols       = []
  }

-- | Build an outbound Connection whose muxer hands out the given
-- streams (one per muxOpenStream call), for driving pushIdentify.
mkPushConnection :: PeerId -> Multiaddr -> IO StreamIO -> IO Connection
mkPushConnection pid remoteAddr openStream = do
  stateVar <- newTVarIO ConnOpen
  pure Connection
    { connPeerId     = pid
    , connDirection  = Outbound
    , connLocalAddr  = Multiaddr [IP4 0x7f000001, TCP 0]
    , connRemoteAddr = remoteAddr
    , connSecurity   = "/noise"
    , connMuxer      = "/yamux/1.0.0"
    , connSession    = MuxerSession
        { muxOpenStream   = openStream
        , muxAcceptStream = fail "test connection: no inbound streams"
        , muxClose        = pure ()
        }
    , connState      = stateVar
    }

-- | Insert a connection into the Switch's connection pool.
addConn :: Switch -> Connection -> IO ()
addConn sw conn = atomically $ do
  pool <- readTVar (swConnPool sw)
  writeTVar (swConnPool sw) (Map.insert (connPeerId conn) [conn] pool)

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

    it "buildLocalIdentify advertises exactly the registered protocol set" $ do
      sw <- mkTestSwitch
      info <- buildLocalIdentify sw Nothing
      -- Set equality, not membership: over-advertising protocols we do
      -- not actually handle must fail this test too.
      sort (idProtocols info) `shouldBe` ["/test/1.0.0", "/test/2.0.0"]

    it "buildLocalIdentify omits observedAddr without a connection context" $ do
      sw <- mkTestSwitch
      info <- buildLocalIdentify sw Nothing
      idObservedAddr info `shouldBe` Nothing

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

    it "handleIdentifyPush merges a partial update instead of replacing the entry" $ do
      -- specs/identify: "missing fields should be ignored, as peers may
      -- choose to send partial updates". go-libp2p sends address-only
      -- pushes; they must not wipe the publicKey/protocols/agentVersion
      -- we already know.
      sw <- mkTestSwitch
      let remotePeerId = PeerId "merge-peer"
          knownInfo = IdentifyInfo
            { idProtocolVersion = Just "ipfs/0.1.0"
            , idAgentVersion    = Just "go-libp2p/0.36.0"
            , idPublicKey       = Just (BS.pack [1, 2, 3])
            , idListenAddrs     = [encodeProtocols [IP4 0x7f000001, TCP 4001]]
            , idObservedAddr    = Nothing
            , idProtocols       = ["/ipfs/id/1.0.0", "/ipfs/ping/1.0.0"]
            }
      atomically $ do
        store <- readTVar (swPeerStore sw)
        writeTVar (swPeerStore sw) (Map.insert remotePeerId knownInfo store)
      -- Address-only push: every other field is absent
      let newAddrs = [encodeProtocols [IP4 0x7f000001, TCP 9999]]
          partialPush = IdentifyInfo
            { idProtocolVersion = Nothing
            , idAgentVersion    = Nothing
            , idPublicKey       = Nothing
            , idListenAddrs     = newAddrs
            , idObservedAddr    = Nothing
            , idProtocols       = []
            }
      (streamA, streamB) <- mkClosableStreamPair
      streamWrite streamA (frame (encodeIdentify partialPush))
      streamClose streamA
      conn <- mkTestConnection remotePeerId (Multiaddr [IP4 0x7f000001, TCP 4001])
      handleIdentifyPush sw conn streamB
      store <- atomically $ readTVar (swPeerStore sw)
      case Map.lookup remotePeerId store of
        Nothing -> expectationFailure "Expected peer in store"
        Just merged -> do
          -- Fields carried by the push are updated
          idListenAddrs merged `shouldBe` newAddrs
          -- Fields absent from the push keep their known values
          idPublicKey merged `shouldBe` Just (BS.pack [1, 2, 3])
          idProtocols merged `shouldBe` ["/ipfs/id/1.0.0", "/ipfs/ping/1.0.0"]
          idAgentVersion merged `shouldBe` Just "go-libp2p/0.36.0"
          idProtocolVersion merged `shouldBe` Just "ipfs/0.1.0"

    it "mergeIdentify replaces present optional fields and non-empty repeated fields wholesale" $ do
      -- specs/identify: only *missing* fields are ignored. A field that
      -- is present in the update wins, and repeated fields are replaced
      -- wholesale — never unioned or appended (matching go-libp2p).
      let known = IdentifyInfo
            { idProtocolVersion = Just "ipfs/0.1.0"
            , idAgentVersion    = Just "old-agent/1.0"
            , idPublicKey       = Just (BS.pack [1, 2, 3])
            , idListenAddrs     = [encodeProtocols [IP4 0x7f000001, TCP 4001]]
            , idObservedAddr    = Nothing
            , idProtocols       = ["/old/1.0.0", "/old/2.0.0"]
            }
          update = IdentifyInfo
            { idProtocolVersion = Just "ipfs/0.2.0"
            , idAgentVersion    = Just "new-agent/2.0"
            , idPublicKey       = Just (BS.pack [9, 9])
            , idListenAddrs     = [encodeProtocols [IP4 0x7f000001, TCP 9999]]
            , idObservedAddr    = Just (encodeProtocols [IP4 0x7f000001, TCP 5555])
            , idProtocols       = ["/new/1.0.0"]
            }
      -- Every field of the result comes from the update; in particular
      -- idProtocols is exactly the update's one-element list, not a
      -- union with the two known entries.
      mergeIdentify known update `shouldBe` update

    it "mergeIdentify keeps every known field when the update is empty" $ do
      let known = IdentifyInfo
            { idProtocolVersion = Just "ipfs/0.1.0"
            , idAgentVersion    = Just "go-libp2p/0.36.0"
            , idPublicKey       = Just (BS.pack [1, 2, 3])
            , idListenAddrs     = [encodeProtocols [IP4 0x7f000001, TCP 4001]]
            , idObservedAddr    = Just (encodeProtocols [IP4 0x7f000001, TCP 5555])
            , idProtocols       = ["/ipfs/id/1.0.0"]
            }
      mergeIdentify known emptyInfo `shouldBe` known

    it "handleIdentifyPush with an empty message leaves the peer entry untouched" $ do
      -- specs/identify: "missing fields should be ignored". An empty
      -- push carries no information, so the stored entry must survive
      -- byte-identical.
      sw <- mkTestSwitch
      let remotePeerId = PeerId "empty-push-peer"
          knownInfo = IdentifyInfo
            { idProtocolVersion = Just "ipfs/0.1.0"
            , idAgentVersion    = Just "go-libp2p/0.36.0"
            , idPublicKey       = Just (BS.pack [1, 2, 3])
            , idListenAddrs     = [encodeProtocols [IP4 0x7f000001, TCP 4001]]
            , idObservedAddr    = Nothing
            , idProtocols       = ["/ipfs/id/1.0.0", "/ipfs/ping/1.0.0"]
            }
      atomically $ do
        store <- readTVar (swPeerStore sw)
        writeTVar (swPeerStore sw) (Map.insert remotePeerId knownInfo store)
      (streamA, streamB) <- mkClosableStreamPair
      streamWrite streamA (frame (encodeIdentify emptyInfo))
      streamClose streamA
      conn <- mkTestConnection remotePeerId (Multiaddr [IP4 0x7f000001, TCP 4001])
      handleIdentifyPush sw conn streamB
      store <- atomically $ readTVar (swPeerStore sw)
      Map.lookup remotePeerId store `shouldBe` Just knownInfo

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

    it "readFramedIdentify rejects a truncated frame" $ do
      (streamA, streamB) <- mkClosableStreamPair
      -- The length prefix promises 100 bytes but only 40 arrive before
      -- EOF: the reader must return Left, never a partial Right.
      streamWrite streamA (encodeUvarint 100 <> BS.replicate 40 0x08)
      streamClose streamA
      result <- readFramedIdentify streamB maxIdentifySize
      case result of
        Left err -> err `shouldSatisfy` (not . null)
        Right info -> expectationFailure $
          "expected truncated frame to be rejected, got: " ++ show info

    it "readFramedIdentify rejects an oversized length prefix" $ do
      (streamA, streamB) <- mkClosableStreamPair
      -- Announce a length just above the limit, without sending a payload
      streamWrite streamA (encodeUvarint (fromIntegral (maxIdentifySize + 1)))
      result <- readFramedIdentify streamB maxIdentifySize
      case result of
        Left err -> err `shouldSatisfy` (not . null)
        Right _  -> expectationFailure "expected oversized message to be rejected"

    it "pushIdentify sends a framed identify message on the push protocol to connected peers" $ do
      -- specs/identify: the push variant opens a stream to each remote
      -- peer using /ipfs/id/push/1.0.0, sends one Identify message and
      -- closes the stream.
      --
      -- The switch is built without setStreamHandler: that API itself
      -- fires a background push, which would race with this test's
      -- single mock stream. Protocols are registered directly instead.
      Right kp <- generateKeyPair
      sw <- newSwitch (fromPublicKey (kpPublic kp)) kp
      let noopHandler _ _ = pure ()
      atomically $ do
        protos <- readTVar (swProtocols sw)
        writeTVar (swProtocols sw)
          (Map.insert "/test/1.0.0" noopHandler
            (Map.insert "/test/2.0.0" noopHandler protos))
      (streamA, streamB) <- mkClosableStreamPair
      openCountRef <- newIORef (0 :: Int)
      stateVar <- newTVarIO ConnOpen
      let remotePid  = PeerId "push-target"
          remoteAddr = Multiaddr [IP4 0x7f000001, TCP 45678]
          conn = Connection
            { connPeerId     = remotePid
            , connDirection  = Outbound
            , connLocalAddr  = Multiaddr [IP4 0x7f000001, TCP 0]
            , connRemoteAddr = remoteAddr
            , connSecurity   = "/noise"
            , connMuxer      = "/yamux/1.0.0"
            , connSession    = MuxerSession
                { muxOpenStream   = do
                    modifyIORef' openCountRef (+ 1)
                    pure streamA
                , muxAcceptStream = fail "not used in this test"
                , muxClose        = pure ()
                }
            , connState      = stateVar
            }
      atomically $ do
        pool <- readTVar (swConnPool sw)
        writeTVar (swConnPool sw) (Map.insert remotePid [conn] pool)
      -- Remote side: accept the push protocol negotiation, then read
      -- one varint-length-prefixed identify message.
      remote <- async $ do
        negResult <- negotiateResponder streamB [identifyPushProtocolId]
        infoOrErr <- readFramedIdentify streamB maxIdentifySize
        pure (negResult, infoOrErr)
      pushIdentify sw
      (negResult, infoOrErr) <- wait remote
      negResult `shouldBe` Accepted identifyPushProtocolId
      case infoOrErr of
        Left err -> expectationFailure $ "push message decode failed: " ++ err
        Right info -> do
          sort (idProtocols info) `shouldBe` ["/test/1.0.0", "/test/2.0.0"]
          idObservedAddr info `shouldBe` Just (encodeProtocols [IP4 0x7f000001, TCP 45678])
          idPublicKey info `shouldBe` Just (encodePublicKey (kpPublic (swIdentityKey sw)))
      -- Exactly one push stream was opened on the connection
      opens <- readIORef openCountRef
      opens `shouldBe` 1

    it "pushIdentify sends each connection its own observedAddr and closes every push stream" $ do
      -- specs/identify: observedAddr is the address of the *remote*
      -- peer as seen from this connection, so a push fan-out must build
      -- a distinct message per connection — and close each push stream
      -- after the single message (the length prefix is the message
      -- boundary, the close releases the stream).
      Right kp <- generateKeyPair
      sw <- newSwitch (fromPublicKey (kpPublic kp)) kp
      (streamA1, streamB1) <- mkClosableStreamPair
      (streamA2, streamB2) <- mkClosableStreamPair
      closed1 <- newIORef False
      closed2 <- newIORef False
      let addr1 = [IP4 0x7f000001, TCP 1111]
          addr2 = [IP4 0x7f000001, TCP 2222]
      conn1 <- mkPushConnection (PeerId "push-1") (Multiaddr addr1)
                 (pure (recordClose closed1 streamA1))
      conn2 <- mkPushConnection (PeerId "push-2") (Multiaddr addr2)
                 (pure (recordClose closed2 streamA2))
      addConn sw conn1
      addConn sw conn2
      let remoteSide streamB = async $ do
            negResult <- negotiateResponder streamB [identifyPushProtocolId]
            negResult `shouldBe` Accepted identifyPushProtocolId
            readFramedIdentify streamB maxIdentifySize
      remote1 <- remoteSide streamB1
      remote2 <- remoteSide streamB2
      pushIdentify sw
      info1 <- wait remote1
      info2 <- wait remote2
      fmap idObservedAddr info1 `shouldBe` Right (Just (encodeProtocols addr1))
      fmap idObservedAddr info2 `shouldBe` Right (Just (encodeProtocols addr2))
      readIORef closed1 `shouldReturn` True
      readIORef closed2 `shouldReturn` True

    it "pushIdentify keeps pushing to the remaining peers when one connection fails" $ do
      -- Per-peer failures are ignored: a dead muxer on one connection
      -- must not prevent the push from reaching the other peers.
      Right kp <- generateKeyPair
      sw <- newSwitch (fromPublicKey (kpPublic kp)) kp
      (streamA, streamB) <- mkClosableStreamPair
      deadConn <- mkPushConnection (PeerId "push-dead") (Multiaddr [IP4 0x7f000001, TCP 1111])
                    (throwIO (userError "muxer session is dead"))
      liveConn <- mkPushConnection (PeerId "push-live") (Multiaddr [IP4 0x7f000001, TCP 2222])
                    (pure streamA)
      addConn sw deadConn
      addConn sw liveConn
      remote <- async $ do
        negResult <- negotiateResponder streamB [identifyPushProtocolId]
        negResult `shouldBe` Accepted identifyPushProtocolId
        readFramedIdentify streamB maxIdentifySize
      pushIdentify sw
      infoOrErr <- wait remote
      case infoOrErr of
        Left err -> expectationFailure $ "push to the live peer failed: " ++ err
        Right info ->
          idObservedAddr info `shouldBe` Just (encodeProtocols [IP4 0x7f000001, TCP 2222])

    it "registerIdentifyHandlers registers both protocol handlers" $ do
      sw <- mkTestSwitch
      registerIdentifyHandlers sw
      protos <- atomically $ readTVar (swProtocols sw)
      Map.member identifyProtocolId protos `shouldBe` True
      Map.member identifyPushProtocolId protos `shouldBe` True
