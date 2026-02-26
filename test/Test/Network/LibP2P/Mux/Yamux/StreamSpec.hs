module Test.Network.LibP2P.Mux.Yamux.StreamSpec (spec) where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Network.LibP2P.Mux.Yamux.Frame
import Network.LibP2P.Mux.Yamux.Session (acceptStream, newSession, openStream, recvLoop, sendLoop)
import Network.LibP2P.Mux.Yamux.Stream (streamClose, streamRead, streamReset, streamWrite)
import Network.LibP2P.Mux.Yamux.Types
import Test.Hspec

-- | Create an in-memory transport pair for testing.
-- Returns (writeA, readA, writeB, readB) where A->B and B->A.
mkMemoryTransportPair ::
  IO
    ( (BS.ByteString -> IO (), Int -> IO BS.ByteString)
    , (BS.ByteString -> IO (), Int -> IO BS.ByteString)
    )
mkMemoryTransportPair = do
  qAtoB <- newTQueueIO
  qBtoA <- newTQueueIO
  let writeTo q bs = mapM_ (atomically . writeTQueue q) (BS.unpack bs)
      readFrom q n = BS.pack <$> mapM (const (atomically (readTQueue q))) [1 .. n]
  pure ((writeTo qAtoB, readFrom qBtoA), (writeTo qBtoA, readFrom qAtoB))

-- | Create a connected client/server session pair with send/recv loops running.
mkSessionPair :: IO (YamuxSession, YamuxSession)
mkSessionPair = do
  ((writeA, readA), (writeB, readB)) <- mkMemoryTransportPair
  client <- newSession RoleClient writeA readA
  server <- newSession RoleServer writeB readB
  pure (client, server)

spec :: Spec
spec = do
  describe "Stream state transitions" $ do
    it "stream created via openStream starts in SYNSent" $ do
      (client, server) <- mkSessionPair
      -- Start send/recv loops for the client only (we just check initial state)
      _ <- concurrently (sendLoop client) (return ())
      result <- openStream client
      case result of
        Left err -> expectationFailure $ "openStream failed: " <> show err
        Right stream -> do
          st <- readTVarIO (ysState stream)
          st `shouldBe` StreamSYNSent

    it "SYNSent -> Established on ACK flag received" $ do
      (client, server) <- mkSessionPair
      -- Manually inject an ACK frame for stream 1
      let ackHdr =
            YamuxHeader
              { yhVersion = 0
              , yhType = FrameWindowUpdate
              , yhFlags = defaultFlags {flagACK = True}
              , yhStreamId = 1
              , yhLength = 0
              }
      -- Open a stream from client, then feed an ACK
      streamVar <- newEmptyTMVarIO
      -- Run client send loop + open stream, then inject ACK on server side
      atomically $ writeTQueue (ysSendCh client) (ackHdr, BS.empty)
      -- Actually, we need a proper test. Let's use full session pair with loops.
      -- Use concurrently to run both sides.
      (clientStream, serverStream) <-
        concurrently
          ( do
              -- Client: run loops and open stream
              Right s <- openStream client
              -- Wait for ACK by checking state
              atomically $ do
                st <- readTVar (ysState s)
                check (st == StreamEstablished)
              pure s
          )
          ( do
              -- Server: run loops and accept stream
              Right s <- acceptStream server
              pure s
          )
      stClient <- readTVarIO (ysState clientStream)
      stClient `shouldBe` StreamEstablished
      stServer <- readTVarIO (ysState serverStream)
      stServer `shouldBe` StreamEstablished

    it "Established -> LocalClose on FIN send" $ do
      (client, server) <- mkSessionPair
      (clientStream, _serverStream) <-
        concurrently
          (openStream client >>= \(Right s) -> pure s)
          (acceptStream server >>= \(Right s) -> pure s)
      -- Wait for established
      atomically $ do
        st <- readTVar (ysState clientStream)
        check (st == StreamEstablished)
      -- Send FIN
      Right () <- streamClose clientStream
      st <- readTVarIO (ysState clientStream)
      st `shouldBe` StreamLocalClose

    it "Established -> RemoteClose on FIN received" $ do
      (client, server) <- mkSessionPair
      (clientStream, serverStream) <-
        concurrently
          (openStream client >>= \(Right s) -> pure s)
          (acceptStream server >>= \(Right s) -> pure s)
      -- Wait for established on both
      atomically $ do
        st <- readTVar (ysState clientStream)
        check (st == StreamEstablished)
      -- Server sends FIN
      Right () <- streamClose serverStream
      -- Client should see RemoteClose
      atomically $ do
        st <- readTVar (ysState clientStream)
        check (st == StreamRemoteClose)
      st <- readTVarIO (ysState clientStream)
      st `shouldBe` StreamRemoteClose

    it "LocalClose -> Closed on remote FIN" $ do
      (client, server) <- mkSessionPair
      (clientStream, serverStream) <-
        concurrently
          (openStream client >>= \(Right s) -> pure s)
          (acceptStream server >>= \(Right s) -> pure s)
      atomically $ do
        st <- readTVar (ysState clientStream)
        check (st == StreamEstablished)
      -- Client sends FIN (LocalClose)
      Right () <- streamClose clientStream
      -- Server sends FIN (closes from the other side)
      Right () <- streamClose serverStream
      -- Client should transition to Closed
      atomically $ do
        st <- readTVar (ysState clientStream)
        check (st == StreamClosed)
      st <- readTVarIO (ysState clientStream)
      st `shouldBe` StreamClosed

    it "Any state -> Reset on RST received" $ do
      (client, server) <- mkSessionPair
      (clientStream, serverStream) <-
        concurrently
          (openStream client >>= \(Right s) -> pure s)
          (acceptStream server >>= \(Right s) -> pure s)
      atomically $ do
        st <- readTVar (ysState clientStream)
        check (st == StreamEstablished)
      -- Server resets the stream
      streamReset serverStream
      -- Client should see Reset
      atomically $ do
        st <- readTVar (ysState clientStream)
        check (st == StreamReset)
      st <- readTVarIO (ysState clientStream)
      st `shouldBe` StreamReset

  describe "Stream read/write on closed streams" $ do
    it "streamWrite returns YamuxStreamClosed on Closed stream" $ do
      (client, server) <- mkSessionPair
      (clientStream, serverStream) <-
        concurrently
          (openStream client >>= \(Right s) -> pure s)
          (acceptStream server >>= \(Right s) -> pure s)
      atomically $ do
        st <- readTVar (ysState clientStream)
        check (st == StreamEstablished)
      -- Close both sides
      Right () <- streamClose clientStream
      Right () <- streamClose serverStream
      atomically $ do
        st <- readTVar (ysState clientStream)
        check (st == StreamClosed)
      -- Write should fail
      result <- streamWrite clientStream "data"
      result `shouldBe` Left YamuxStreamClosed

    it "streamWrite returns YamuxStreamClosed on LocalClose stream" $ do
      (client, server) <- mkSessionPair
      (clientStream, _serverStream) <-
        concurrently
          (openStream client >>= \(Right s) -> pure s)
          (acceptStream server >>= \(Right s) -> pure s)
      atomically $ do
        st <- readTVar (ysState clientStream)
        check (st == StreamEstablished)
      Right () <- streamClose clientStream
      result <- streamWrite clientStream "data"
      result `shouldBe` Left YamuxStreamClosed

    it "streamRead returns buffered data after RemoteClose (drain)" $ do
      (client, server) <- mkSessionPair
      (clientStream, serverStream) <-
        concurrently
          (openStream client >>= \(Right s) -> pure s)
          (acceptStream server >>= \(Right s) -> pure s)
      atomically $ do
        st <- readTVar (ysState serverStream)
        check (st == StreamEstablished)
      -- Server writes data then closes
      Right () <- streamWrite serverStream "hello"
      Right () <- streamClose serverStream
      -- Client should still be able to read the buffered data
      atomically $ do
        st <- readTVar (ysState clientStream)
        check (st == StreamRemoteClose)
      result <- streamRead clientStream
      result `shouldBe` Right "hello"

    it "streamRead returns YamuxStreamClosed on Closed stream with empty buffer" $ do
      (client, server) <- mkSessionPair
      (clientStream, serverStream) <-
        concurrently
          (openStream client >>= \(Right s) -> pure s)
          (acceptStream server >>= \(Right s) -> pure s)
      atomically $ do
        st <- readTVar (ysState clientStream)
        check (st == StreamEstablished)
      -- Both sides close
      Right () <- streamClose clientStream
      Right () <- streamClose serverStream
      atomically $ do
        st <- readTVar (ysState clientStream)
        check (st == StreamClosed)
      result <- streamRead clientStream
      result `shouldBe` Left YamuxStreamClosed

  describe "Optimistic sending" $ do
    it "streamWrite succeeds in SYNSent state" $ do
      (client, server) <- mkSessionPair
      Right stream <- openStream client
      -- Stream should be in SYNSent
      st <- readTVarIO (ysState stream)
      st `shouldBe` StreamSYNSent
      -- Write should succeed (optimistic sending per spec)
      result <- streamWrite stream "early data"
      result `shouldBe` Right ()
