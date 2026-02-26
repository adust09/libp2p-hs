-- | Yamux stream operations: read, write, close, reset.
--
-- Implements per-stream data transfer with flow control
-- per HashiCorp yamux spec.md §Data/WindowUpdate/Stream Close.
module Network.LibP2P.Mux.Yamux.Stream
  ( streamWrite
  , streamRead
  , streamClose
  , streamReset
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.LibP2P.Mux.Yamux.Frame
import Network.LibP2P.Mux.Yamux.Types

-- | Write data to a stream. Blocks when send window is 0.
-- Writable states: SYNSent (optimistic), Established, RemoteClose.
-- Returns YamuxStreamClosed on LocalClose/Closed.
-- Returns YamuxStreamReset on Reset.
streamWrite :: YamuxStream -> ByteString -> IO (Either YamuxError ())
streamWrite stream payload
  | BS.null payload = pure (Right ())
  | otherwise = do
      st <- readTVarIO (ysState stream)
      case st of
        StreamClosed -> pure (Left YamuxStreamClosed)
        StreamLocalClose -> pure (Left YamuxStreamClosed)
        StreamReset -> pure (Left YamuxStreamReset)
        _ -> writeChunked stream payload

-- | Write payload in chunks respecting the send window.
writeChunked :: YamuxStream -> ByteString -> IO (Either YamuxError ())
writeChunked stream payload
  | BS.null payload = pure (Right ())
  | otherwise = do
      -- Wait for available send window (blocks via STM retry if 0)
      chunk <- atomically $ do
        st <- readTVar (ysState stream)
        case st of
          StreamReset -> pure BS.empty
          StreamClosed -> pure BS.empty
          _ -> do
            window <- readTVar (ysSendWindow stream)
            if window == 0
              then do
                -- Block until WindowUpdate arrives
                _ <- takeTMVar (ysSendNotify stream)
                window' <- readTVar (ysSendWindow stream)
                let chunkSize = min (fromIntegral window') (BS.length payload)
                let (c, _) = BS.splitAt chunkSize payload
                writeTVar (ysSendWindow stream) (window' - fromIntegral chunkSize)
                pure c
              else do
                let chunkSize = min (fromIntegral window) (BS.length payload)
                let (c, _) = BS.splitAt chunkSize payload
                writeTVar (ysSendWindow stream) (window - fromIntegral chunkSize)
                pure c
      if BS.null chunk
        then do
          -- Re-check state to determine error
          st <- readTVarIO (ysState stream)
          case st of
            StreamReset -> pure (Left YamuxStreamReset)
            StreamClosed -> pure (Left YamuxStreamClosed)
            _ -> pure (Right ()) -- shouldn't happen
        else do
          -- Enqueue Data frame
          let hdr =
                YamuxHeader
                  { yhVersion = 0
                  , yhType = FrameData
                  , yhFlags = defaultFlags
                  , yhStreamId = ysStreamId stream
                  , yhLength = fromIntegral (BS.length chunk)
                  }
          let sess = ysSession stream
          atomically $ writeTQueue (ysSendCh sess) (hdr, chunk)
          -- Continue with remaining data
          let remaining = BS.drop (BS.length chunk) payload
          writeChunked stream remaining

-- | Read data from a stream. Blocks on empty buffer.
-- Readable states: SYNSent, SYNReceived, Established, LocalClose, RemoteClose (draining).
-- Returns YamuxStreamClosed on Closed/RemoteClose with empty buffer.
-- Returns YamuxStreamReset on Reset.
streamRead :: YamuxStream -> IO (Either YamuxError ByteString)
streamRead stream = do
  result <- atomically $ do
    -- Try to read from buffer first (non-blocking check)
    mData <- tryReadTQueue (ysRecvBuf stream)
    case mData of
      Just payload -> pure (Right payload)
      Nothing -> do
        -- Buffer empty, check state
        st <- readTVar (ysState stream)
        case st of
          StreamClosed -> pure (Left YamuxStreamClosed)
          StreamReset -> pure (Left YamuxStreamReset)
          StreamRemoteClose -> pure (Left YamuxStreamClosed)
          _ -> retry -- block until data arrives
  case result of
    Right payload -> do
      -- Send WindowUpdate to replenish the recv window
      let consumed = fromIntegral (BS.length payload)
      let hdr =
            YamuxHeader
              { yhVersion = 0
              , yhType = FrameWindowUpdate
              , yhFlags = defaultFlags
              , yhStreamId = ysStreamId stream
              , yhLength = consumed
              }
      let sess = ysSession stream
      atomically $ do
        writeTQueue (ysSendCh sess) (hdr, BS.empty)
        -- Increment recv window
        w <- readTVar (ysRecvWindow stream)
        writeTVar (ysRecvWindow stream) (w + consumed)
      pure (Right payload)
    Left err -> pure (Left err)

-- | Half-close the stream by sending FIN flag.
-- Only valid in Established or RemoteClose states.
streamClose :: YamuxStream -> IO (Either YamuxError ())
streamClose stream = do
  result <- atomically $ do
    st <- readTVar (ysState stream)
    case st of
      StreamEstablished -> do
        writeTVar (ysState stream) StreamLocalClose
        pure (Right ())
      StreamRemoteClose -> do
        writeTVar (ysState stream) StreamClosed
        pure (Right ())
      StreamSYNSent -> do
        writeTVar (ysState stream) StreamLocalClose
        pure (Right ())
      StreamSYNReceived -> do
        writeTVar (ysState stream) StreamLocalClose
        pure (Right ())
      StreamClosed -> pure (Left YamuxStreamClosed)
      StreamLocalClose -> pure (Left YamuxStreamClosed)
      StreamReset -> pure (Left YamuxStreamReset)
  case result of
    Right () -> do
      -- Send FIN frame
      let hdr =
            YamuxHeader
              { yhVersion = 0
              , yhType = FrameData
              , yhFlags = defaultFlags {flagFIN = True}
              , yhStreamId = ysStreamId stream
              , yhLength = 0
              }
      let sess = ysSession stream
      atomically $ writeTQueue (ysSendCh sess) (hdr, BS.empty)
      pure (Right ())
    Left err -> pure (Left err)

-- | Reset the stream by sending RST flag.
streamReset :: YamuxStream -> IO ()
streamReset stream = do
  atomically $ writeTVar (ysState stream) StreamReset
  -- Send RST frame
  let hdr =
        YamuxHeader
          { yhVersion = 0
          , yhType = FrameData
          , yhFlags = defaultFlags {flagRST = True}
          , yhStreamId = ysStreamId stream
          , yhLength = 0
          }
  let sess = ysSession stream
  atomically $ writeTQueue (ysSendCh sess) (hdr, BS.empty)
