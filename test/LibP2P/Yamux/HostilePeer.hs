-- | Hostile-peer test harness for Yamux (issue #171).
--
-- Runs our session implementation on one side and gives the test raw
-- byte-level control of the other side, so tests can inject arbitrary
-- frames (including ones our own sender never emits) and decode every
-- frame the session writes.
--
-- This replaces the symmetric self-pair harness for adversarial and
-- state-machine tests: with two of our sessions wired together, any
-- frame shape a conformant-but-differently-written peer may send is
-- structurally unreachable.
module LibP2P.Yamux.HostilePeer
  ( HostilePeer (..)
  , withHostilePeer
  , injectFrame
  , expectFrame
  , acceptWithin
  , awaitState
  , awaitTrue
  ) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import LibP2P.Yamux.Frame
import LibP2P.Yamux.Session
import LibP2P.Yamux.Types
import System.Timeout (timeout)
import Test.Hspec

-- | Our session under test plus raw byte access to both directions.
data HostilePeer = HostilePeer
  { hpSession :: YamuxSession
  -- ^ The session under test (recvLoop/sendLoop already running)
  , hpInject :: ByteString -> IO ()
  -- ^ Write raw bytes into the session's receive side
  , hpNextFrame :: IO (YamuxHeader, ByteString)
  -- ^ Decode the next frame the session wrote (header + Data payload)
  }

-- | Run an action against a session wired to a raw byte peer.
withHostilePeer :: SessionRole -> (HostilePeer -> IO a) -> IO a
withHostilePeer role action = do
  toSession <- newTQueueIO
  fromSession <- newTQueueIO
  let writeTo q bs = mapM_ (atomically . writeTQueue q) (BS.unpack bs)
      readFrom q n = BS.pack <$> mapM (const (atomically (readTQueue q))) [1 .. n]
  sess <- newSession role (writeTo fromSession) (readFrom toSession)
  let nextFrame = do
        hdrBytes <- readFrom fromSession headerSize
        case decodeHeader hdrBytes of
          Left err -> fail ("frame recorder: " <> err)
          Right hdr -> do
            payload <-
              if yhType hdr == FrameData && yhLength hdr > 0
                then readFrom fromSession (fromIntegral (yhLength hdr))
                else pure BS.empty
            pure (hdr, payload)
  withAsync (sendLoop sess) $ \_ ->
    withAsync (recvLoop sess) $ \_ ->
      action (HostilePeer sess (writeTo toSession) nextFrame)

-- | Inject a frame (header plus optional Data payload) as raw bytes.
injectFrame :: HostilePeer -> YamuxHeader -> ByteString -> IO ()
injectFrame hp hdr payload = hpInject hp (encodeHeader hdr <> payload)

-- | Read the session's next outbound frame, failing loudly after 1s
-- instead of hanging the suite.
expectFrame :: HostilePeer -> IO (YamuxHeader, ByteString)
expectFrame hp = do
  mFrame <- timeout 1000000 (hpNextFrame hp)
  case mFrame of
    Just frame -> pure frame
    Nothing -> fail "expected an outbound frame within 1s, got none"

-- | Accept an inbound stream, failing loudly after 1s.
acceptWithin :: HostilePeer -> IO YamuxStream
acceptWithin hp = do
  mRes <- timeout 1000000 (acceptStream (hpSession hp))
  case mRes of
    Just (Right s) -> pure s
    Just (Left err) -> fail ("acceptStream: " <> show err)
    Nothing -> fail "acceptStream timed out after 1s"

-- | Block until the stream reaches the given state, failing after 1s.
awaitState :: YamuxStream -> StreamState -> Expectation
awaitState stream expected = do
  result <- timeout 1000000 $ atomically $ do
    st <- readTVar (ysState stream)
    check (st == expected)
  case result of
    Just () -> pure ()
    Nothing -> do
      actual <- readTVarIO (ysState stream)
      expectationFailure $
        "expected stream state " <> show expected <> " but stuck at " <> show actual

-- | Block until the TVar becomes True, failing after 1s.
awaitTrue :: TVar Bool -> Expectation
awaitTrue var = do
  result <- timeout 1000000 $ atomically (readTVar var >>= check)
  case result of
    Just () -> pure ()
    Nothing -> expectationFailure "expected TVar to become True within 1s"
