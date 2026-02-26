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

import Data.ByteString (ByteString)
import Network.LibP2P.Mux.Yamux.Types (YamuxError, YamuxStream)

-- | Write data to a stream. Blocks when send window is 0.
-- Returns YamuxStreamClosed on closed/reset stream.
streamWrite :: YamuxStream -> ByteString -> IO (Either YamuxError ())
streamWrite = error "TODO: streamWrite"

-- | Read data from a stream. Blocks on empty buffer.
-- Returns buffered data after RemoteClose (drain).
-- Returns YamuxStreamClosed on fully closed stream with empty buffer.
streamRead :: YamuxStream -> IO (Either YamuxError ByteString)
streamRead = error "TODO: streamRead"

-- | Half-close the stream by sending FIN flag.
streamClose :: YamuxStream -> IO (Either YamuxError ())
streamClose = error "TODO: streamClose"

-- | Reset the stream by sending RST flag.
streamReset :: YamuxStream -> IO ()
streamReset = error "TODO: streamReset"
