-- | Dial logic for the Switch (docs/08-switch.md §Dialing).
--
-- Implements connection reuse, exponential backoff, dial deduplication,
-- and parallel staggered dialing (Happy Eyeballs, RFC 8305).
--
-- Dial flow:
--   1. Check connection pool for existing Open connection
--   2. Check per-peer backoff (reject if recently failed)
--   3. Deduplication: join pending dial if another thread is already dialing
--   4. Select transport per address, staggered parallel dial
--   5. Upgrade first successful raw connection
--   6. Add to pool / record backoff on failure
module Network.LibP2P.Switch.Dial
  ( -- * Main entry point
    dial
    -- * Backoff management
  , checkBackoff
  , recordBackoff
  , clearBackoff
    -- * Constants (exported for testing)
  , initialBackoffSeconds
  , maxBackoffSeconds
  , staggerDelayUs
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, waitAnyCatch)
import Control.Concurrent.STM
  ( STM
  , TMVar
  , TVar
  , atomically
  , newEmptyTMVar
  , putTMVar
  , readTMVar
  , readTVar
  , writeTVar
  )
import Control.Exception (SomeException)
import Control.Monad (forM, when)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (NominalDiffTime, addUTCTime, getCurrentTime)
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr)
import Network.LibP2P.Switch.ConnPool (addConn, lookupConn)
import Network.LibP2P.Switch.Listen (streamAcceptLoop)
import Network.LibP2P.Switch.ResourceManager (Direction (..), releaseConnection, reserveConnection)
import Network.LibP2P.Switch.Types
  ( BackoffEntry (..)
  , Connection
  , DialError (..)
  , Switch (..)
  )
import Network.LibP2P.Switch.Upgrade (upgradeOutbound)
import Network.LibP2P.Transport.Transport (Transport (..))

-- | Initial backoff duration after first failure: 5 seconds.
initialBackoffSeconds :: NominalDiffTime
initialBackoffSeconds = 5

-- | Maximum backoff duration: 300 seconds (5 minutes).
maxBackoffSeconds :: NominalDiffTime
maxBackoffSeconds = 300

-- | Stagger delay between parallel dial attempts: 250ms (RFC 8305).
staggerDelayUs :: Int
staggerDelayUs = 250000

-- | Check if a peer is currently in dial backoff.
-- Returns Right () if no backoff is active or the backoff has expired.
-- Expired entries are cleaned up atomically.
checkBackoff :: TVar (Map.Map PeerId BackoffEntry) -> PeerId -> IO (Either DialError ())
checkBackoff backoffsVar pid = do
  now <- getCurrentTime
  atomically $ do
    boffs <- readTVar backoffsVar
    case Map.lookup pid boffs of
      Nothing -> pure (Right ())
      Just be
        | beExpiry be <= now -> do
            -- Expired, clean up
            writeTVar backoffsVar (Map.delete pid boffs)
            pure (Right ())
        | otherwise -> pure (Left DialBackoff)

-- | Record a backoff after a failed dial.
-- First failure: 5s. Each subsequent: duration * 2, capped at 300s.
-- Backoff formula: min(initialBackoff * 2^(attempts-1), maxBackoff)
recordBackoff :: TVar (Map.Map PeerId BackoffEntry) -> PeerId -> IO ()
recordBackoff backoffsVar pid = do
  now <- getCurrentTime
  atomically $ do
    boffs <- readTVar backoffsVar
    let attempts = case Map.lookup pid boffs of
          Nothing -> 1
          Just be -> beAttempts be + 1
        -- Exponential backoff: 5s, 10s, 20s, 40s, ..., capped at 300s
        duration = min maxBackoffSeconds
                       (initialBackoffSeconds * fromIntegral (2 ^ (attempts - 1) :: Int))
        entry = BackoffEntry
          { beExpiry   = addUTCTime duration now
          , beAttempts = attempts
          }
    writeTVar backoffsVar (Map.insert pid entry boffs)

-- | Clear backoff for a peer (called on successful connection).
clearBackoff :: TVar (Map.Map PeerId BackoffEntry) -> PeerId -> IO ()
clearBackoff backoffsVar pid = atomically $ do
  boffs <- readTVar backoffsVar
  writeTVar backoffsVar (Map.delete pid boffs)

-- | Result of checking for a pending dial (used internally).
data PendingCheck
  = JoinExisting !(TMVar (Either DialError Connection))
  | StartNew     !(TMVar (Either DialError Connection))

-- | Dial a peer, reusing existing connections or establishing new ones.
--
-- Implements the full dial flow from docs/08-switch.md §Dial Flow:
--   1. Pool reuse: return existing Open connection if available
--   2. Backoff check: reject if peer recently failed
--   3. Deduplication: coalesce concurrent dials to same peer via TMVar
--   4. Staggered parallel dial with 250ms delay (Happy Eyeballs)
--   5. First success: upgrade, add to pool, return
--   6. All fail: record backoff, return error
dial :: Switch -> PeerId -> [Multiaddr] -> IO (Either DialError Connection)
dial sw remotePeerId addrs = do
  -- 0. Check switch is open
  closed <- atomically $ readTVar (swClosed sw)
  if closed
    then pure (Left DialSwitchClosed)
    else do
      -- 1. Check connection pool for existing Open connection
      existing <- atomically $ lookupConn (swConnPool sw) remotePeerId
      case existing of
        Just conn -> pure (Right conn)
        Nothing -> do
          -- 2. Check backoff
          backoffResult <- checkBackoff (swDialBackoffs sw) remotePeerId
          case backoffResult of
            Left err -> pure (Left err)
            Right () -> do
              -- 3. Deduplication: check for pending dial
              joinOrCreate <- atomically $ checkPendingDial sw remotePeerId
              case joinOrCreate of
                JoinExisting tmvar ->
                  -- Another thread is already dialing; wait for its result
                  atomically $ readTMVar tmvar
                StartNew tmvar ->
                  -- We own this dial; execute and broadcast result
                  dialNewAndBroadcast sw remotePeerId addrs tmvar

-- | Atomically check for an existing pending dial or create one.
checkPendingDial :: Switch -> PeerId -> STM PendingCheck
checkPendingDial sw pid = do
  pending <- readTVar (swPendingDials sw)
  case Map.lookup pid pending of
    Just tmvar -> pure (JoinExisting tmvar)
    Nothing -> do
      tmvar <- newEmptyTMVar
      writeTVar (swPendingDials sw) (Map.insert pid tmvar pending)
      pure (StartNew tmvar)

-- | Execute the dial, broadcast the result, and clean up.
dialNewAndBroadcast
  :: Switch -> PeerId -> [Multiaddr]
  -> TMVar (Either DialError Connection)
  -> IO (Either DialError Connection)
dialNewAndBroadcast sw remotePeerId addrs tmvar = do
  -- Check resource limits before attempting dial
  resCheck <- atomically $ reserveConnection (swResourceMgr sw) remotePeerId Outbound
  case resCheck of
    Left resErr -> do
      let result = Left (DialResourceLimit resErr)
      atomically $ putTMVar tmvar result
      atomically $ do
        pending <- readTVar (swPendingDials sw)
        writeTVar (swPendingDials sw) (Map.delete remotePeerId pending)
      pure result
    Right () -> do
      result <- dialNewInner sw addrs
      -- Broadcast result to any waiting threads
      atomically $ putTMVar tmvar result
      -- Clean up pending dials map
      atomically $ do
        pending <- readTVar (swPendingDials sw)
        writeTVar (swPendingDials sw) (Map.delete remotePeerId pending)
      -- Record backoff on failure, clear on success, add to pool
      case result of
        Right conn -> do
          clearBackoff (swDialBackoffs sw) remotePeerId
          atomically $ addConn (swConnPool sw) conn
          -- Start accepting inbound streams on the dialer side
          _ <- async $ streamAcceptLoop sw conn
          -- Notify connection listeners (e.g. GossipSub auto-stream open)
          notifiers <- atomically $ readTVar (swNotifiers sw)
          mapM_ (\f -> async $ f conn) notifiers
          pure (Right conn)
        Left _ -> do
          -- Release the reserved connection since dial failed
          atomically $ releaseConnection (swResourceMgr sw) remotePeerId Outbound
          recordBackoff (swDialBackoffs sw) remotePeerId
          pure result

-- | Inner dial logic: transport selection and staggered parallel dial.
dialNewInner :: Switch -> [Multiaddr] -> IO (Either DialError Connection)
dialNewInner _sw [] = pure (Left DialNoAddresses)
dialNewInner sw addrs = do
  transports <- atomically $ readTVar (swTransports sw)
  -- Find a transport for each address
  let dialable = filterMap (\addr ->
        case find (\t -> transportCanDial t addr) transports of
          Just t  -> Just (addr, t)
          Nothing -> Nothing) addrs
  case dialable of
    []    -> pure (Left (DialNoTransport (Prelude.head addrs)))
    pairs -> staggeredDial sw pairs

-- | Filter and map a list, keeping only Just results.
filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap f (x:xs) = case f x of
  Nothing -> filterMap f xs
  Just y  -> y : filterMap f xs

-- | Attempt to dial using staggered parallel attempts (Happy Eyeballs, RFC 8305).
--
-- Addresses are tried with 250ms delay between each attempt.
-- The first successful connection wins; remaining attempts are cancelled.
staggeredDial :: Switch -> [(Multiaddr, Transport)] -> IO (Either DialError Connection)
staggeredDial sw pairs = do
  -- Spawn workers with staggered delays: 0ms, 250ms, 500ms, ...
  workers <- forM (zip [0 :: Int ..] pairs) $ \(i, (addr, transport)) ->
    async $ do
      when (i > 0) $ threadDelay (i * staggerDelayUs)
      rawConn <- transportDial transport addr
      upgradeOutbound (swIdentityKey sw) rawConn
  -- Wait for first success or collect all failures
  collectResults workers []

-- | Wait for the first successful async result, cancelling the rest.
-- If all fail, return DialAllFailed with all error messages.
collectResults :: [Async Connection] -> [String] -> IO (Either DialError Connection)
collectResults [] errs = pure (Left (DialAllFailed (reverse errs)))
collectResults workers errs = do
  (completed, result) <- waitAnyCatch workers
  let remaining = filter (/= completed) workers
  case result of
    Right conn -> do
      mapM_ cancel remaining
      pure (Right conn)
    Left (ex :: SomeException) ->
      collectResults remaining (show ex : errs)
