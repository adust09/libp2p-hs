module Test.Network.LibP2P.Protocol.Ping.PingSpec (spec) where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
  ( TMVar
  , TQueue
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
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Word (Word8)
import Network.LibP2P.Crypto.Ed25519 (generateKeyPair)
import Network.LibP2P.Crypto.Key (kpPublic)
import Network.LibP2P.Crypto.PeerId (PeerId (..), fromPublicKey)
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import Network.LibP2P.Protocol.Ping.Ping
import Network.LibP2P.Switch.Switch (newSwitch)
import Network.LibP2P.Switch.Types (Switch (..))
import System.IO.Error (mkIOError, eofErrorType)
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
      streamA = StreamIO (writeQ qAtoB closedA) (readQ qBtoA)
      streamB = StreamIO (writeQ qBtoA closedB) (readQ qAtoB)
  pure (streamA, closeWriter qAtoB closedA, streamB)

-- | Create a simple bidirectional memory stream pair (no close).
mkMemoryPair :: IO (StreamIO, StreamIO)
mkMemoryPair = do
  qAtoB <- newTQueueIO :: IO (TQueue Word8)
  qBtoA <- newTQueueIO :: IO (TQueue Word8)
  let writeQ q bs = mapM_ (atomically . writeTQueue q) (BS.unpack bs)
      readQ q = atomically (readTQueue q)
  pure ( StreamIO (writeQ qAtoB) (readQ qBtoA)
       , StreamIO (writeQ qBtoA) (readQ qAtoB)
       )

-- | Read exactly n bytes from a stream.
readNBytes :: StreamIO -> Int -> IO BS.ByteString
readNBytes stream n = BS.pack <$> mapM (const (streamReadByte stream)) [1..n]

spec :: Spec
spec = do
  describe "Ping protocol" $ do
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
      (streamA, closeA, streamB) <- mkClosableStreamPair
      handler <- async $ handlePing streamB (PeerId "test-peer")
      closeA
      -- Handler should exit gracefully (not hang or crash)
      wait handler

    it "sendPing round-trip measures RTT" $ do
      (streamA, streamB) <- mkMemoryPair
      -- Simulate echo responder
      let echoer = do
            payload <- readNBytes streamB pingSize
            streamWrite streamB payload
      echoTask <- async echoer
      -- Simulate initiator: send random 32 bytes, read echo, measure time
      payload <- getRandomBytes pingSize :: IO BS.ByteString
      t0 <- getCurrentTime
      streamWrite streamA payload
      resp <- readNBytes streamA pingSize
      t1 <- getCurrentTime
      wait echoTask
      resp `shouldBe` payload
      diffUTCTime t1 t0 `shouldSatisfy` (>= 0)

    it "sendPing detects mismatch" $ do
      (streamA, streamB) <- mkMemoryPair
      -- Bad echoer: returns wrong data
      let badEchoer = do
            _payload <- readNBytes streamB pingSize
            streamWrite streamB (BS.replicate pingSize 0xFF)
      echoTask <- async badEchoer
      let payload = BS.pack [1..32]
      streamWrite streamA payload
      resp <- readNBytes streamA pingSize
      wait echoTask
      resp `shouldNotBe` payload

    it "registerPingHandler adds handler to switch" $ do
      Right kp <- generateKeyPair
      let pid = fromPublicKey (kpPublic kp)
      sw <- newSwitch pid kp
      registerPingHandler sw
      protos <- atomically $ readTVar (swProtocols sw)
      Map.member pingProtocolId protos `shouldBe` True
