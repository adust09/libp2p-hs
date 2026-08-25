module LibP2P.Protocol.Perf.PerfSpec (spec) where

import Control.Concurrent.Async (async, wait, withAsync)
import Control.Concurrent.STM (atomically, readTVar)
import Control.Exception (try)
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (kpPublic)
import LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import LibP2P.EofStream (mkEofStreamPair)
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import LibP2P.Protocol.Perf
import LibP2P.Switch (newSwitch)
import LibP2P.Switch.Types (Switch (..))
import System.Timeout (timeout)
import Test.Hspec

-- | Read exactly n bytes from a stream (test helper).
readNBytes :: StreamIO -> Int -> IO BS.ByteString
readNBytes s n = BS.pack <$> mapM (const (streamReadByte s)) [1 .. n]

-- | Expect EOF on the next read.
expectEof :: StreamIO -> Expectation
expectEof s = do
  result <- try (streamReadByte s) :: IO (Either IOError Word8)
  case result of
    Left _  -> pure ()
    Right b -> expectationFailure ("expected EOF, got byte " ++ show b)

mkTestPeerId :: IO PeerId
mkTestPeerId = do
  Right kp <- generateKeyPair
  pure (fromPublicKey (kpPublic kp))

spec :: Spec
spec = do
  describe "handlePerf (server)" $ do
    it "should send back the requested number of bytes when the client half-closes" $ do
      (client, server) <- mkEofStreamPair
      pid <- mkTestPeerId
      serverA <- async (handlePerf server pid)
      -- 8-byte big-endian download size = 5, plus a 3-byte upload
      streamWrite client (BS.pack [0, 0, 0, 0, 0, 0, 0, 5])
      streamWrite client (BS.pack [1, 2, 3])
      streamClose client
      response <- readNBytes client 5
      BS.length response `shouldBe` 5
      expectEof client
      wait serverA

    it "should decode the download size as big-endian" $ do
      (client, server) <- mkEofStreamPair
      pid <- mkTestPeerId
      serverA <- async (handlePerf server pid)
      -- 0x0100 = 256; a little-endian reading would be 2^48 instead
      streamWrite client (BS.pack [0, 0, 0, 0, 0, 0, 1, 0])
      streamClose client
      response <- readNBytes client 256
      BS.length response `shouldBe` 256
      expectEof client
      wait serverA

    it "should send nothing when the requested download size is zero" $ do
      (client, server) <- mkEofStreamPair
      pid <- mkTestPeerId
      serverA <- async (handlePerf server pid)
      streamWrite client (BS.replicate 8 0)
      streamClose client
      expectEof client
      wait serverA

    it "should close without responding when the client closes before the header" $ do
      (client, server) <- mkEofStreamPair
      pid <- mkTestPeerId
      serverA <- async (handlePerf server pid)
      streamClose client
      expectEof client
      wait serverA

  describe "perfOnStream (client)" $ do
    it "should complete an upload/download exchange against handlePerf" $ do
      (client, server) <- mkEofStreamPair
      pid <- mkTestPeerId
      withAsync (handlePerf server pid) $ \_ -> do
        result <- perfOnStream client 1000 2000
        case result of
          Left err -> expectationFailure ("perfOnStream failed: " ++ show err)
          Right r -> perfElapsed r `shouldSatisfy` (>= 0)

    it "should complete a latency-shaped exchange (1 byte each way)" $ do
      (client, server) <- mkEofStreamPair
      pid <- mkTestPeerId
      withAsync (handlePerf server pid) $ \_ -> do
        result <- perfOnStream client 1 1
        result `shouldSatisfy` either (const False) (const True)

    it "should complete a zero-byte exchange" $ do
      (client, server) <- mkEofStreamPair
      pid <- mkTestPeerId
      withAsync (handlePerf server pid) $ \_ -> do
        result <- perfOnStream client 0 0
        result `shouldSatisfy` either (const False) (const True)

    it "should send the download size as an 8-byte big-endian header" $ do
      (client, server) <- mkEofStreamPair
      pid <- mkTestPeerId
      writes <- newIORef []
      let recording = client
            { streamWrite = \bs -> modifyIORef' writes (bs :) >> streamWrite client bs }
      withAsync (handlePerf server pid) $ \_ -> do
        _ <- perfOnStream recording 0 256
        chunks <- reverse <$> readIORef writes
        BS.take 8 (BS.concat chunks) `shouldBe` BS.pack [0, 0, 0, 0, 0, 0, 1, 0]

    it "should report a stream error when the server disappears mid-download" $ do
      (client, server) <- mkEofStreamPair
      -- Fake server: reads nothing, immediately closes without sending
      streamClose server
      result <- timeout 1000000 (perfOnStream client 0 100)
      case result of
        Nothing -> expectationFailure "perfOnStream hung"
        Just (Left (PerfStreamError _)) -> pure ()
        Just other -> expectationFailure ("expected PerfStreamError, got " ++ show other)

  describe "registerPerfHandler" $ do
    it "should add the perf handler to the switch protocol map" $ do
      Right kp <- generateKeyPair
      let pid = fromPublicKey (kpPublic kp)
      sw <- newSwitch pid kp
      registerPerfHandler sw
      protos <- atomically $ readTVar (swProtocols sw)
      Map.member perfProtocolId protos `shouldBe` True
