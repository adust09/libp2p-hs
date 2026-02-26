module Test.Network.LibP2P.Mux.Yamux.StreamSpec (spec) where

import Control.Concurrent.Async (concurrently, withAsync)
import Control.Concurrent.STM
import qualified Data.ByteString as BS
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
