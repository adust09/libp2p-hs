module LibP2P.Protocol.Ping.PingSpec (spec) where

import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.STM
  ( TMVar
  , TQueue
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
import Control.Exception (throwIO, try)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (kpPublic)
import LibP2P.Crypto.PeerId (PeerId (..), fromPublicKey)
import LibP2P.Multiaddr (Multiaddr (..))
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.MultistreamSelect.Negotiation
  ( NegotiationResult (..)
  , StreamIO (..)
  , negotiateResponder
  )
import LibP2P.Protocol.Ping
import LibP2P.Switch (newSwitch)
import LibP2P.Switch.Types
  ( ConnState (..)
  , Connection (..)
  , Direction (..)
  , MuxerSession (..)
  , Switch (..)
  )
import System.IO.Error (mkIOError, eofErrorType)
import System.Timeout (timeout)
import Test.Hspec

-- | Create a closable stream pair where the writer can signal EOF.
mkClosableStreamPair :: IO (StreamIO, IO (), StreamIO)
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
        writeTQueue q Nothing
      streamA = StreamIO (writeQ qAtoB closedA) (readQ qBtoA) (closeWriter qAtoB closedA)
      streamB = StreamIO (writeQ qBtoA closedB) (readQ qAtoB) (closeWriter qBtoA closedB)
  pure (streamA, closeWriter qAtoB closedA, streamB)

-- | Wrap a StreamIO so every write chunk is recorded (most recent first).
recordWrites :: IORef [BS.ByteString] -> StreamIO -> StreamIO
recordWrites ref s = s
  { streamWrite = \bs -> modifyIORef' ref (bs :) >> streamWrite s bs }

-- | Wrap a StreamIO so closing it flips the flag (before delegating).
recordClose :: IORef Bool -> StreamIO -> StreamIO
recordClose ref s = s { streamClose = writeIORef ref True >> streamClose s }

-- | Stream opener that counts how many streams have been handed out.
countingOpen :: IORef Int -> StreamIO -> IO StreamIO
countingOpen ref stream = modifyIORef' ref (+ 1) >> pure stream

-- | A Switch whose only role here is stream resource accounting for
-- the ping initiator ('sendPing' reserves stream slots through it).
mkTestSwitch :: IO Switch
mkTestSwitch = do
  Right kp <- generateKeyPair
  let pid = fromPublicKey (kpPublic kp)
  newSwitch pid kp

-- | Build an outbound Connection whose muxer runs the given stream opener.
-- This is what lets tests drive the real 'sendPing' initiator path.
mkPingConnection :: IO StreamIO -> IO Connection
mkPingConnection openStream = do
  stateVar <- newTVarIO ConnOpen
  pure Connection
    { connPeerId     = PeerId "ping-remote"
    , connDirection  = Outbound
    , connLocalAddr  = Multiaddr [IP4 0x7f000001, TCP 0]
    , connRemoteAddr = Multiaddr [IP4 0x7f000001, TCP 4001]
    , connSecurity   = "/noise"
    , connMuxer      = "/yamux/1.0.0"
    , connSession    = MuxerSession
        { muxOpenStream   = openStream
        , muxAcceptStream = fail "test connection: no inbound streams"
        , muxClose        = pure ()
        }
    , connState      = stateVar
    }

-- | Read exactly n bytes from a stream.
readNBytes :: StreamIO -> Int -> IO BS.ByteString
readNBytes stream n = BS.pack <$> mapM (const (streamReadByte stream)) [1..n]

-- | Responder that accepts the ping protocol and runs the echo loop.
pingResponder :: StreamIO -> IO ()
pingResponder stream = do
  negResult <- negotiateResponder stream [pingProtocolId]
  negResult `shouldBe` Accepted pingProtocolId
  handlePing stream (PeerId "initiator")

spec :: Spec
spec = do
  describe "Ping responder (handlePing)" $ do
    it "handlePing echoes 32 bytes" $ do
      (streamA, closeA, streamB) <- mkClosableStreamPair
      handler <- async $ handlePing streamB (PeerId "test-peer")
      let payload = BS.pack [1..32]
      streamWrite streamA payload
      response <- readNBytes streamA pingSize
      response `shouldBe` payload
      closeA
      wait handler

    it "handlePing echoes multiple pings on same stream" $ do
      (streamA, closeA, streamB) <- mkClosableStreamPair
      handler <- async $ handlePing streamB (PeerId "test-peer")
      -- Ping 1
      let payload1 = BS.pack [1..32]
      streamWrite streamA payload1
      resp1 <- readNBytes streamA pingSize
      resp1 `shouldBe` payload1
      -- Ping 2 (different data)
      let payload2 = BS.pack [33..64]
      streamWrite streamA payload2
      resp2 <- readNBytes streamA pingSize
      resp2 `shouldBe` payload2
      closeA
      wait handler

    it "handlePing exits on stream close" $ do
      (_streamA, closeA, streamB) <- mkClosableStreamPair
      handler <- async $ handlePing streamB (PeerId "test-peer")
      closeA
      -- Handler should exit gracefully (not hang or crash)
      wait handler

    it "handlePing closes its side of the stream after the loop exits" $ do
      -- ping.md: the listening peer SHOULD exit the loop and close the
      -- stream once the dialing peer closes its write side.
      (_streamA, closeA, streamB) <- mkClosableStreamPair
      closedRef <- newIORef False
      handler <- async $ handlePing (recordClose closedRef streamB) (PeerId "test-peer")
      closeA
      wait handler
      closed <- readIORef closedRef
      closed `shouldBe` True

    it "handlePing exits without echoing when the final read is short" $ do
      (streamA, closeA, streamB) <- mkClosableStreamPair
      handler <- async $ handlePing streamB (PeerId "test-peer")
      -- One byte short of a full ping payload, then EOF (ping.md: the
      -- payload is exactly 32 bytes; a truncated read must not be echoed).
      streamWrite streamA (BS.pack [1..31])
      closeA
      wait handler
      -- The responder must not have written anything back: reading from
      -- its side either blocks (timeout) or hits the EOF it signalled by
      -- closing the stream — never an echoed byte.
      echoed <- timeout 100000 (try (streamReadByte streamA))
      case echoed of
        Nothing            -> pure ()  -- no data, read blocked
        Just (Left (_ :: IOError)) -> pure ()  -- EOF from responder close
        Just (Right b)     -> expectationFailure $
          "responder echoed a byte from a short read: " ++ show b

  describe "Ping initiator (sendPing)" $ do
    it "sendPing round-trips against handlePing and returns a non-negative RTT" $ do
      (streamA, _closeA, streamB) <- mkClosableStreamPair
      sw <- mkTestSwitch
      conn <- mkPingConnection (pure streamA)
      responder <- async $ pingResponder streamB
      result <- sendPing sw conn
      case result of
        Left err -> expectationFailure $ "sendPing failed: " ++ show err
        Right (PingResult rtt) -> rtt `shouldSatisfy` (>= 0)
      -- Regression for #163: sendPing must close the stream, so the
      -- responder's echo loop sees EOF and exits on its own.
      done <- timeout 1000000 (wait responder)
      done `shouldBe` Just ()

    it "sendPing closes the ping stream after the ping completes" $ do
      (streamA, _closeA, streamB) <- mkClosableStreamPair
      closedRef <- newIORef False
      sw <- mkTestSwitch
      conn <- mkPingConnection (pure (recordClose closedRef streamA))
      responder <- async $ pingResponder streamB
      result <- sendPing sw conn
      case result of
        Left err -> expectationFailure $ "sendPing failed: " ++ show err
        Right _  -> pure ()
      closed <- readIORef closedRef
      closed `shouldBe` True
      done <- timeout 1000000 (wait responder)
      done `shouldBe` Just ()

    it "two pings on one session reuse a single stream and close it at the end" $ do
      -- ping.md: the dialing peer MUST NOT keep more than one outbound
      -- ping stream per peer, and MAY send further payloads on the same
      -- stream.
      (streamA, _closeA, streamB) <- mkClosableStreamPair
      opensRef <- newIORef (0 :: Int)
      closedRef <- newIORef False
      sw <- mkTestSwitch
      conn <- mkPingConnection (countingOpen opensRef (recordClose closedRef streamA))
      responder <- async $ pingResponder streamB
      result <- withPingSession sw conn $ \sess -> do
        r1 <- ping sess
        r2 <- ping sess
        pure (r1, r2)
      case result of
        Left err -> expectationFailure $ "ping session failed to open: " ++ show err
        Right (r1, r2) -> do
          case r1 of
            Left err -> expectationFailure $ "ping 1 failed: " ++ show err
            Right (PingResult rtt1) -> rtt1 `shouldSatisfy` (>= 0)
          case r2 of
            Left err -> expectationFailure $ "ping 2 failed: " ++ show err
            Right (PingResult rtt2) -> rtt2 `shouldSatisfy` (>= 0)
      opens <- readIORef opensRef
      opens `shouldBe` 1
      closed <- readIORef closedRef
      closed `shouldBe` True
      done <- timeout 1000000 (wait responder)
      done `shouldBe` Just ()

    it "ping fails on a session that has been closed" $ do
      (streamA, _closeA, streamB) <- mkClosableStreamPair
      sw <- mkTestSwitch
      conn <- mkPingConnection (pure streamA)
      responder <- async $ pingResponder streamB
      opened <- openPingSession sw conn
      case opened of
        Left err -> expectationFailure $ "ping session failed to open: " ++ show err
        Right sess -> do
          closePingSession sess
          result <- ping sess
          case result of
            Left (PingStreamError _) -> pure ()
            other -> expectationFailure $
              "expected Left PingStreamError, got: " ++ show other
      done <- timeout 1000000 (wait responder)
      done `shouldBe` Just ()

    it "pingWithTimeout returns Left PingTimeout and closes the stream when no echo arrives" $ do
      (streamA, _closeA, streamB) <- mkClosableStreamPair
      closedRef <- newIORef False
      sw <- mkTestSwitch
      conn <- mkPingConnection (pure (recordClose closedRef streamA))
      responder <- async $ do
        negResult <- negotiateResponder streamB [pingProtocolId]
        negResult `shouldBe` Accepted pingProtocolId
        -- Swallow the payload and never echo.
        _ <- readNBytes streamB pingSize
        pure ()
      opened <- openPingSession sw conn
      case opened of
        Left err -> expectationFailure $ "ping session failed to open: " ++ show err
        Right sess -> do
          result <- pingWithTimeout 200000 sess
          result `shouldBe` Left PingTimeout
          closed <- readIORef closedRef
          closed `shouldBe` True
      wait responder

    it "sendPing returns Left PingMismatch and closes the stream when the echo is corrupted" $ do
      (streamA, _closeA, streamB) <- mkClosableStreamPair
      closedRef <- newIORef False
      sw <- mkTestSwitch
      conn <- mkPingConnection (pure (recordClose closedRef streamA))
      responder <- async $ do
        negResult <- negotiateResponder streamB [pingProtocolId]
        negResult `shouldBe` Accepted pingProtocolId
        payload <- readNBytes streamB pingSize
        -- Corrupt the first byte before echoing (ping.md: the echo MUST
        -- match the sent payload).
        let corrupted = BS.cons (BS.head payload `xor` 0xFF) (BS.tail payload)
        streamWrite streamB corrupted
      result <- sendPing sw conn
      wait responder
      result `shouldBe` Left PingMismatch
      closed <- readIORef closedRef
      closed `shouldBe` True

    it "sendPing returns Left PingStreamError on mid-payload EOF" $ do
      (streamA, _closeA, streamB) <- mkClosableStreamPair
      sw <- mkTestSwitch
      conn <- mkPingConnection (pure streamA)
      responder <- async $ do
        negResult <- negotiateResponder streamB [pingProtocolId]
        negResult `shouldBe` Accepted pingProtocolId
        payload <- readNBytes streamB pingSize
        -- Echo only half the payload, then close: the initiator must
        -- surface a stream error instead of hanging or succeeding.
        streamWrite streamB (BS.take 16 payload)
        streamClose streamB
      result <- sendPing sw conn
      wait responder
      case result of
        Left (PingStreamError _) -> pure ()
        other -> expectationFailure $
          "expected Left PingStreamError, got: " ++ show other

    it "sendPing returns Left PingStreamError and closes the stream when the responder rejects the protocol" $ do
      (streamA, _closeA, streamB) <- mkClosableStreamPair
      closedRef <- newIORef False
      sw <- mkTestSwitch
      conn <- mkPingConnection (pure (recordClose closedRef streamA))
      -- Responder speaks multistream-select but supports no protocols, so
      -- it answers "na" to /ipfs/ping/1.0.0 and keeps waiting for further
      -- proposals (which never come) — hence cancel, not wait.
      responder <- async $ negotiateResponder streamB []
      result <- sendPing sw conn
      cancel responder
      case result of
        Left (PingStreamError _) -> pure ()
        other -> expectationFailure $
          "expected Left PingStreamError, got: " ++ show other
      closed <- readIORef closedRef
      closed `shouldBe` True

    it "sendPing writes a fresh random 32-byte payload per ping, unframed" $ do
      let runRecordedPing = do
            (streamA, _closeA, streamB) <- mkClosableStreamPair
            writesRef <- newIORef ([] :: [BS.ByteString])
            sw <- mkTestSwitch
            conn <- mkPingConnection (pure (recordWrites writesRef streamA))
            responder <- async $ pingResponder streamB
            result <- sendPing sw conn
            case result of
              Left err -> expectationFailure $ "sendPing failed: " ++ show err
              Right _  -> pure ()
            -- sendPing closed the stream, so the responder exits on EOF.
            wait responder
            readIORef writesRef
      chunks1 <- runRecordedPing
      -- The initiator writes exactly three chunks: the multistream header,
      -- the protocol proposal, and the ping payload (most recent first).
      length chunks1 `shouldBe` 3
      let payload1 = head chunks1
      -- ping.md: the payload is 32 raw bytes — no varint prefix, no
      -- framing. A length-prefixed write would be 33+ bytes here.
      BS.length payload1 `shouldBe` pingSize
      -- A second ping must use a freshly generated random payload.
      chunks2 <- runRecordedPing
      let payload2 = head chunks2
      BS.length payload2 `shouldBe` pingSize
      payload2 `shouldNotBe` payload1

  describe "Ping registration" $ do
    it "registerPingHandler adds handler to switch" $ do
      Right kp <- generateKeyPair
      let pid = fromPublicKey (kpPublic kp)
      sw <- newSwitch pid kp
      registerPingHandler sw
      protos <- atomically $ readTVar (swProtocols sw)
      Map.member pingProtocolId protos `shouldBe` True
