module Test.Network.LibP2P.Mux.Yamux.StreamSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, concurrently, withAsync)
import Control.Concurrent.STM
import qualified Data.ByteString as BS
import Network.LibP2P.Mux.Yamux.Frame (initialWindowSize)
import Network.LibP2P.Mux.Yamux.Session (acceptStream, newSession, openStream, recvLoop, sendLoop)
import Network.LibP2P.Mux.Yamux.Stream (streamClose, streamRead, streamReset, streamWrite)
import Network.LibP2P.Mux.Yamux.Types
import Test.Hspec

-- | Create an in-memory transport pair for testing.
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

-- | Create a session pair and run action with all background loops running.
withSessionPair :: ((YamuxSession, YamuxSession) -> IO a) -> IO a
withSessionPair action = do
  ((writeA, readA), (writeB, readB)) <- mkMemoryTransportPair
  client <- newSession RoleClient writeA readA
  server <- newSession RoleServer writeB readB
  withAsync (sendLoop client) $ \_ ->
    withAsync (recvLoop client) $ \_ ->
      withAsync (sendLoop server) $ \_ ->
        withAsync (recvLoop server) $ \_ ->
          action (client, server)

spec :: Spec
spec = do
  describe "Stream state transitions" $ do
    it "stream created via openStream starts in SYNSent" $ do
      withSessionPair $ \(client, _server) -> do
        Right stream <- openStream client
        st <- readTVarIO (ysState stream)
        st `shouldBe` StreamSYNSent

    it "SYNSent -> Established on ACK flag received" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, _serverStream) <-
          concurrently
            ( do
                Right s <- openStream client
                -- Wait for ACK
                atomically $ do
                  st <- readTVar (ysState s)
                  check (st == StreamEstablished)
                pure s
            )
            (acceptStream server >>= \(Right s) -> pure s)
        stClient <- readTVarIO (ysState clientStream)
        stClient `shouldBe` StreamEstablished

    it "Established -> LocalClose on FIN send" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, _serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        Right () <- streamClose clientStream
        st <- readTVarIO (ysState clientStream)
        st `shouldBe` StreamLocalClose

    it "Established -> RemoteClose on FIN received" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        Right () <- streamClose serverStream
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamRemoteClose)
        st <- readTVarIO (ysState clientStream)
        st `shouldBe` StreamRemoteClose

    it "LocalClose -> Closed on remote FIN" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        Right () <- streamClose clientStream
        Right () <- streamClose serverStream
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamClosed)
        st <- readTVarIO (ysState clientStream)
        st `shouldBe` StreamClosed

    it "Any state -> Reset on RST received" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        streamReset serverStream
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamReset)
        st <- readTVarIO (ysState clientStream)
        st `shouldBe` StreamReset

  describe "Stream read/write on closed streams" $ do
    it "streamWrite returns YamuxStreamClosed on Closed stream" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        Right () <- streamClose clientStream
        Right () <- streamClose serverStream
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamClosed)
        result <- streamWrite clientStream "data"
        result `shouldBe` Left YamuxStreamClosed

    it "streamWrite returns YamuxStreamClosed on LocalClose stream" $ do
      withSessionPair $ \(client, server) -> do
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
      withSessionPair $ \(client, server) -> do
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
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        Right () <- streamClose clientStream
        Right () <- streamClose serverStream
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamClosed)
        result <- streamRead clientStream
        result `shouldBe` Left YamuxStreamClosed

  describe "Optimistic sending" $ do
    it "streamWrite succeeds in SYNSent state" $ do
      withSessionPair $ \(client, _server) -> do
        Right stream <- openStream client
        st <- readTVarIO (ysState stream)
        st `shouldBe` StreamSYNSent
        result <- streamWrite stream "early data"
        result `shouldBe` Right ()

  describe "Flow control" $ do
    it "initial send window is 262144" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, _serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        w <- readTVarIO (ysSendWindow clientStream)
        w `shouldBe` initialWindowSize

    it "initial recv window is 262144" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, _serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        w <- readTVarIO (ysRecvWindow clientStream)
        w `shouldBe` initialWindowSize

    it "send window decreases by payload length after write" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, _serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        let payload = BS.replicate 1000 0x42
        Right () <- streamWrite clientStream payload
        w <- readTVarIO (ysSendWindow clientStream)
        w `shouldBe` (initialWindowSize - 1000)

    it "send window increases by delta on WindowUpdate received" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        -- Client writes data
        let payload = BS.replicate 1000 0x42
        Right () <- streamWrite clientStream payload
        -- Server reads it (triggers WindowUpdate back to client)
        Right _ <- streamRead serverStream
        -- Client's send window should be restored
        -- Give a moment for the WindowUpdate to arrive
        atomically $ do
          w <- readTVar (ysSendWindow clientStream)
          check (w == initialWindowSize)
        w <- readTVarIO (ysSendWindow clientStream)
        w `shouldBe` initialWindowSize

    it "streamWrite blocks when send window is 0" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, _serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        -- Manually set send window to 0
        atomically $ writeTVar (ysSendWindow clientStream) 0
        -- Start a write in another thread - it should block
        writeStarted <- newTVarIO False
        writeDone <- newTVarIO False
        _ <- async $ do
          atomically $ writeTVar writeStarted True
          _ <- streamWrite clientStream "blocked"
          atomically $ writeTVar writeDone True
        -- Wait for write to start, then verify it hasn't completed
        atomically $ readTVar writeStarted >>= check
        threadDelay 10000 -- 10ms
        done <- readTVarIO writeDone
        done `shouldBe` False
        -- Unblock by restoring window (STM retry watches ysSendWindow)
        atomically $ writeTVar (ysSendWindow clientStream) 1000
        -- Now the write should complete
        atomically $ readTVar writeDone >>= check

    it "streamWrite resumes after WindowUpdate arrives" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        -- Drain the send window
        atomically $ writeTVar (ysSendWindow clientStream) 0
        -- Start a write in another thread
        writeDone <- newTVarIO False
        _ <- async $ do
          _ <- streamWrite clientStream "resumed"
          atomically $ writeTVar writeDone True
        threadDelay 10000
        done1 <- readTVarIO writeDone
        done1 `shouldBe` False
        -- Simulate a WindowUpdate by increasing window (STM retry watches ysSendWindow)
        atomically $ writeTVar (ysSendWindow clientStream) initialWindowSize
        atomically $ readTVar writeDone >>= check
        -- Server should be able to read it
        Right received <- streamRead serverStream
        received `shouldBe` "resumed"

    it "recv window decreases on incoming data" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState serverStream)
          check (st == StreamEstablished)
        -- Client writes data
        let payload = BS.replicate 500 0x42
        Right () <- streamWrite clientStream payload
        -- Wait for server to receive the data (recv window should decrease)
        atomically $ do
          w <- readTVar (ysRecvWindow serverStream)
          check (w < initialWindowSize)
        w <- readTVarIO (ysRecvWindow serverStream)
        w `shouldBe` (initialWindowSize - 500)

    it "WindowUpdate frame sent automatically after streamRead" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState serverStream)
          check (st == StreamEstablished)
        -- Client writes data
        let payload = BS.replicate 1000 0x42
        Right () <- streamWrite clientStream payload
        -- Server reads (this sends WindowUpdate automatically)
        Right _ <- streamRead serverStream
        -- Client's send window should be restored via WindowUpdate
        atomically $ do
          w <- readTVar (ysSendWindow clientStream)
          check (w == initialWindowSize)

    it "cannot send payload larger than current send window in single frame" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        -- Set window to 10 bytes
        atomically $ writeTVar (ysSendWindow clientStream) 10
        -- Write 20 bytes in background - first 10 sent, then blocks
        let payload = BS.replicate 20 0x42
        writeDone <- newTVarIO False
        _ <- async $ do
          _ <- streamWrite clientStream payload
          atomically $ writeTVar writeDone True
        -- Wait for writer to block (first chunk sent, second blocked)
        threadDelay 10000
        done <- readTVarIO writeDone
        done `shouldBe` False
        -- Server reads first chunk (10 bytes) — triggers WindowUpdate, unblocking writer
        Right chunk1 <- streamRead serverStream
        BS.length chunk1 `shouldBe` 10
        -- Read remaining chunk (writer now unblocked by WindowUpdate)
        Right chunk2 <- streamRead serverStream
        BS.length chunk2 `shouldBe` 10
        atomically $ readTVar writeDone >>= check
