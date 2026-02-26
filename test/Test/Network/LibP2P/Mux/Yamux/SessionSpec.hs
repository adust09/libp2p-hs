module Test.Network.LibP2P.Mux.Yamux.SessionSpec (spec) where

import Control.Concurrent.Async (async, concurrently, concurrently_, withAsync)
import Control.Concurrent.STM
import qualified Data.ByteString as BS
import Network.LibP2P.Mux.Yamux.Frame
import Network.LibP2P.Mux.Yamux.Session
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
  describe "Stream ID allocation" $ do
    it "client session allocates odd IDs: 1, 3, 5" $ do
      withSessionPair $ \(client, server) -> do
        ids <- concurrently
          ( do
              Right s1 <- openStream client
              Right s2 <- openStream client
              Right s3 <- openStream client
              pure [ysStreamId s1, ysStreamId s2, ysStreamId s3]
          )
          ( do
              Right _ <- acceptStream server
              Right _ <- acceptStream server
              Right _ <- acceptStream server
              pure ()
          )
        fst ids `shouldBe` [1, 3, 5]

    it "server session allocates even IDs: 2, 4, 6" $ do
      withSessionPair $ \(client, server) -> do
        ids <- concurrently
          ( do
              Right _ <- acceptStream client
              Right _ <- acceptStream client
              Right _ <- acceptStream client
              pure ()
          )
          ( do
              Right s1 <- openStream server
              Right s2 <- openStream server
              Right s3 <- openStream server
              pure [ysStreamId s1, ysStreamId s2, ysStreamId s3]
          )
        snd ids `shouldBe` [2, 4, 6]

  describe "Stream open/accept" $ do
    it "openStream sends Data frame with SYN flag" $ do
      withSessionPair $ \(client, server) -> do
        Right _stream <- openStream client
        Right serverStream <- acceptStream server
        ysStreamId serverStream `shouldBe` 1

    it "acceptStream returns stream on remote SYN" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        ysStreamId clientStream `shouldBe` 1
        ysStreamId serverStream `shouldBe` 1

  describe "Data exchange" $ do
    it "bidirectional data exchange: client -> server, server -> client" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        -- Client sends to server
        Right () <- streamWrite clientStream "hello"
        Right received <- streamRead serverStream
        received `shouldBe` "hello"
        -- Server sends to client
        Right () <- streamWrite serverStream "world"
        Right received2 <- streamRead clientStream
        received2 `shouldBe` "world"

    it "optimistic sending: data piggybacked on SYN frame" $ do
      withSessionPair $ \(client, server) -> do
        Right clientStream <- openStream client
        st <- readTVarIO (ysState clientStream)
        st `shouldBe` StreamSYNSent
        Right () <- streamWrite clientStream "early"
        -- Server accepts and reads
        Right serverStream <- acceptStream server
        Right received <- streamRead serverStream
        received `shouldBe` "early"

  describe "Stream rejection" $ do
    it "RST response to SYN -> streamWrite returns Reset" $ do
      withSessionPair $ \(client, server) -> do
        Right clientStream <- openStream client
        Right serverStream <- acceptStream server
        streamReset serverStream
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamReset)
        result <- streamWrite clientStream "data"
        result `shouldBe` Left YamuxStreamReset

  describe "Concurrent streams" $ do
    it "4 concurrent streams on single session" $ do
      withSessionPair $ \(client, server) -> do
        let numStreams = 4 :: Int
        concurrently_
          ( do
              streams <- mapM (\_ -> openStream client >>= \(Right s) -> pure s) [1 .. numStreams]
              mapM_
                ( \(i, s) -> do
                    atomically $ do
                      st <- readTVar (ysState s)
                      check (st == StreamEstablished)
                    Right () <- streamWrite s (BS.pack [fromIntegral i])
                    pure ()
                )
                (zip [1 :: Int ..] streams)
          )
          ( do
              streams <- mapM (\_ -> acceptStream server >>= \(Right s) -> pure s) [1 .. numStreams]
              mapM_
                ( \s -> do
                    Right _ <- streamRead s
                    pure ()
                )
                streams
          )

  describe "Ping" $ do
    it "Ping SYN -> ACK response received" $ do
      withSessionPair $ \(client, _server) -> do
        result <- ping client
        result `shouldBe` Right ()

    it "Ping echoes exact opaque value in Length field" $ do
      withSessionPair $ \(client, _server) -> do
        result <- ping client
        result `shouldBe` Right ()

    it "Ping uses StreamID 0" $ do
      withSessionPair $ \(client, _server) -> do
        result <- ping client
        result `shouldBe` Right ()

  describe "GoAway" $ do
    it "GoAway Normal (0x00) sets ysShutdown" $ do
      withSessionPair $ \(client, _server) -> do
        sendGoAway client GoAwayNormal
        shutdown <- readTVarIO (ysShutdown client)
        shutdown `shouldBe` True

    it "openStream fails after local GoAway sent" $ do
      withSessionPair $ \(client, _server) -> do
        sendGoAway client GoAwayNormal
        result <- openStream client
        shouldBeLeft YamuxSessionShutdown result

    it "openStream fails after remote GoAway received" $ do
      withSessionPair $ \(client, server) -> do
        sendGoAway server GoAwayNormal
        atomically $ do
          got <- readTVar (ysRemoteGoAway client)
          check got
        result <- openStream client
        shouldBeLeft YamuxSessionShutdown result

    it "existing streams continue after GoAway" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        sendGoAway client GoAwayNormal
        Right () <- streamWrite clientStream "still works"
        Right received <- streamRead serverStream
        received `shouldBe` "still works"

  describe "Full lifecycle" $ do
    it "open -> write -> read -> FIN -> close" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        Right () <- streamWrite clientStream "hello"
        Right received <- streamRead serverStream
        received `shouldBe` "hello"
        Right () <- streamWrite serverStream "world"
        Right received2 <- streamRead clientStream
        received2 `shouldBe` "world"
        Right () <- streamClose clientStream
        Right () <- streamClose serverStream
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamClosed)
        stClient <- readTVarIO (ysState clientStream)
        stClient `shouldBe` StreamClosed

  describe "Flow control integration" $ do
    it "transfer data exceeding initial window (requires WindowUpdate exchange)" $ do
      withSessionPair $ \(client, server) -> do
        (clientStream, serverStream) <-
          concurrently
            (openStream client >>= \(Right s) -> pure s)
            (acceptStream server >>= \(Right s) -> pure s)
        atomically $ do
          st <- readTVar (ysState clientStream)
          check (st == StreamEstablished)
        -- Set a small window (100 bytes) to force WindowUpdate exchange
        atomically $ writeTVar (ysSendWindow clientStream) 100
        -- Write and read concurrently: writer sends 250 bytes, reader accumulates
        let totalData = BS.replicate 250 0xAB
        (_, received) <-
          concurrently
            (streamWrite clientStream totalData)
            (readAll serverStream 250)
        received `shouldBe` totalData

    it "concurrent read/write on 4 streams simultaneously" $ do
      withSessionPair $ \(client, server) -> do
        let numStreams = 4 :: Int
            payload = BS.replicate 100 0xCC
        concurrently_
          ( do
              streams <- mapM (\_ -> openStream client >>= \(Right s) -> pure s) [1 .. numStreams]
              mapM_ (\s -> atomically (readTVar (ysState s) >>= \st -> check (st == StreamEstablished))) streams
              -- Write to all streams concurrently
              mapM_ (\s -> async (streamWrite s payload)) streams
          )
          ( do
              streams <- mapM (\_ -> acceptStream server >>= \(Right s) -> pure s) [1 .. numStreams]
              -- Read from all streams
              results <- mapM (\s -> streamRead s >>= \(Right d) -> pure d) streams
              mapM_ (\d -> BS.length d `shouldBe` 100) results
          )

-- | Read exactly n bytes from a stream by accumulating chunks.
readAll :: YamuxStream -> Int -> IO BS.ByteString
readAll stream n = go BS.empty
  where
    go acc
      | BS.length acc >= n = pure (BS.take n acc)
      | otherwise = do
          Right chunk <- streamRead stream
          go (acc <> chunk)

-- | Helper to assert an Either is a Left with a specific error value.
shouldBeLeft :: (Show e, Eq e) => e -> Either e a -> Expectation
shouldBeLeft expected (Left actual) = actual `shouldBe` expected
shouldBeLeft expected (Right _) =
  expectationFailure $ "Expected Left " <> show expected <> " but got Right"
