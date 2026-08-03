-- | Connection lifecycle for the Switch.
--
-- Provides the single teardown path for upgraded connections and a
-- resource-accounted outbound stream opener. Teardown fires from the
-- stream accept loop exit (remote disconnect or session death), from
-- explicit 'closeConnection' calls, and from 'switchClose' via
-- 'closeAllConnections'.
module LibP2P.Switch.Connection
  ( closeConnection
  , closeAllConnections
  , newStream
  ) where

import Control.Concurrent.STM (atomically, readTVar, writeTChan, writeTVar)
import Control.Exception (SomeException, catch, finally, onException)
import Control.Monad (unless, when)
import Data.IORef (atomicModifyIORef', newIORef)
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import LibP2P.Switch.ConnPool (allConns, removeConn)
import LibP2P.Switch.ResourceManager
  ( Direction (..)
  , ResourceError
  , releaseConnection
  , releasePeerStream
  , reservePeerStream
  )
import LibP2P.Switch.Types
  ( ConnState (..)
  , Connection (..)
  , MuxerSession (..)
  , Switch (..)
  , SwitchEvent (..)
  )

-- | Tear down a connection: remove it from the pool, release its
-- resource reservation, publish a Disconnected event, and close the
-- muxer session together with the underlying transport.
--
-- Idempotent: the state transition to ConnClosed is atomic, so
-- concurrent calls (accept loop exit, explicit close, switchClose)
-- perform the teardown exactly once.
closeConnection :: Switch -> Connection -> IO ()
closeConnection sw conn = do
  shouldClose <- atomically $ do
    st <- readTVar (connState conn)
    if st == ConnClosed
      then pure False
      else do
        writeTVar (connState conn) ConnClosed
        removeConn (swConnPool sw) conn
        releaseConnection (swResourceMgr sw) (connPeerId conn) (connDirection conn)
        writeTChan (swEvents sw)
          (Disconnected (connPeerId conn) (connDirection conn) (connRemoteAddr conn))
        pure True
  when shouldClose $
    muxClose (connSession conn) `catch` \(_ :: SomeException) -> pure ()

-- | Tear down every pooled connection (used by switchClose).
closeAllConnections :: Switch -> IO ()
closeAllConnections sw = do
  conns <- atomically $ allConns (swConnPool sw)
  mapM_ (closeConnection sw) conns

-- | Open an outbound stream on a connection, reserving a stream slot
-- against the peer's resource scope. The slot is released when the
-- returned stream is closed (exactly once, even on double close).
newStream :: Switch -> Connection -> IO (Either ResourceError StreamIO)
newStream sw conn = do
  let pid = connPeerId conn
      release = atomically $ releasePeerStream (swResourceMgr sw) pid Outbound
  reserved <- atomically $ reservePeerStream (swResourceMgr sw) pid Outbound
  case reserved of
    Left err -> pure (Left err)
    Right () -> do
      stream <- muxOpenStream (connSession conn) `onException` release
      releasedRef <- newIORef False
      let releaseOnce = do
            already <- atomicModifyIORef' releasedRef (\r -> (True, r))
            unless already release
      pure (Right stream { streamClose = streamClose stream `finally` releaseOnce })
