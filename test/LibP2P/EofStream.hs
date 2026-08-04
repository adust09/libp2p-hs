-- | An EOF-capable in-memory stream pair: the deterministic sibling of
-- 'mkMemoryStreamPair' (#172).
--
-- 'mkMemoryStreamPair' cannot model disconnection: both halves get
-- @streamClose = pure ()@ and a read on an empty queue blocks forever,
-- so the whole "peer hangs up mid-conversation" test class is
-- inexpressible with it. This pair models a TCP half-close
-- (@shutdown(SHUT_WR)@): 'streamClose' closes the caller's OUTGOING
-- direction, after which the peer's reads first drain any buffered
-- bytes and then throw an EOF 'IOError' — exactly like a socket that
-- received FIN. Writes into a direction the writer already closed also
-- fail, like writing to a shut-down socket.
--
-- Real-socket disconnect behaviour (whole-node survival, dial
-- teardown) is covered over TCP in 'LibP2P.FaultInjectionSpec'; this
-- harness exists for deterministic, in-process assertions on the exact
-- negotiation/handshake results a peer disconnect must produce.
module LibP2P.EofStream (mkEofStreamPair) where

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import Data.Word (Word8)
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import System.IO.Error (eofErrorType, mkIOError)

-- | One direction of the duplex pair: a byte queue plus a closed flag
-- set by the writing side's 'streamClose'.
data HalfDuplex = HalfDuplex
  { hdQueue  :: TQueue Word8
  , hdClosed :: TVar Bool
  }

newHalfDuplex :: IO HalfDuplex
newHalfDuplex = HalfDuplex <$> newTQueueIO <*> newTVarIO False

-- | Create an in-memory stream pair where each side can signal EOF.
-- Closing side A makes side B's reads throw an EOF 'IOError' once the
-- bytes already written have been drained, and vice versa.
mkEofStreamPair :: IO (StreamIO, StreamIO)
mkEofStreamPair = do
  aToB <- newHalfDuplex
  bToA <- newHalfDuplex
  pure (mkEnd aToB bToA, mkEnd bToA aToB)
  where
    mkEnd outgoing incoming = StreamIO
      { streamWrite    = writeHalf outgoing
      , streamReadByte = readHalf incoming
      , streamClose    = atomically (writeTVar (hdClosed outgoing) True)
      }

    writeHalf hd bs = do
      ok <- atomically $ do
        closed <- readTVar (hdClosed hd)
        if closed
          then pure False
          else True <$ mapM_ (writeTQueue (hdQueue hd)) (BS.unpack bs)
      if ok
        then pure ()
        else ioError (mkIOError eofErrorType "write to closed stream" Nothing Nothing)

    readHalf hd = do
      -- Buffered bytes drain first; EOF only surfaces on an empty queue
      -- whose writer has closed — matching TCP FIN semantics.
      r <- atomically $
        (Just <$> readTQueue (hdQueue hd))
          `orElse` (do
            closed <- readTVar (hdClosed hd)
            check closed
            pure Nothing)
      case r of
        Just b  -> pure b
        Nothing -> ioError (mkIOError eofErrorType "end of stream" Nothing Nothing)
