-- | DCUtR (Direct Connection Upgrade through Relay) protocol.
--
-- Protocol: /libp2p/dcutr
-- Coordinates hole punching over a relayed connection using a 3-message exchange
-- with RTT-based timing synchronization.
--
-- Message flow:
--   B (initiator) sends CONNECT with B's observed addresses
--   A (handler) sends CONNECT with A's observed addresses
--   B sends SYNC
--   B waits RTT/2, then dials A's addresses
--   A receives SYNC, then dials B's addresses immediately
--   Both peers attempt direct connections at approximately the same time
--
-- Per the spec, every exchanged address is dialled in parallel (hole punching
-- depends on near-simultaneous packets), and on failure the whole exchange is
-- re-run from the CONNECT step so RTT is re-measured (3 attempts total).
module LibP2P.NAT.DCUtR
  ( -- * Types
    DCUtRConfig (..)
  , DCUtRResult (..)
    -- * Protocol operations
  , initiateDCUtR
  , handleDCUtR
    -- * Variants for testing
  , initiateDCUtRWithRTT
  , initiateDCUtRCapture
  , handleDCUtRCapture
  ) where

import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, writeIORef)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, waitAny, cancel)
import Control.Exception (bracket)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import LibP2P.NAT.DCUtR.Message
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import LibP2P.Multiaddr (Multiaddr, toBytes, fromBytes)

-- | DCUtR configuration.
data DCUtRConfig = DCUtRConfig
  { dcMaxAttempts :: !Int
    -- ^ Total number of hole punch attempts; the full CONNECT/SYNC exchange
    -- is re-run for each attempt so RTT is re-measured (spec: 3 total =
    -- 1 initial + 2 retries)
  , dcDialer     :: !(Multiaddr -> IO (Either String ()))
    -- ^ Injectable dial function for testing
  }

-- | DCUtR result.
data DCUtRResult = DCUtRSuccess | DCUtRFailed String
  deriving (Show, Eq)

-- | Outcome of a single hole punch attempt. Only dial failures are
-- retryable; protocol errors abort the upgrade immediately.
data AttemptError = FatalError String | DialError String

-- | Peer B (initiator): run the DCUtR exchange over a relayed stream.
--
-- Flow (repeated up to 'dcMaxAttempts' times while the hole punch fails):
--   1. Send CONNECT with own observed addresses
--   2. Read A's CONNECT (measure RTT)
--   3. Send SYNC
--   4. Wait RTT/2, then dial all of A's addresses in parallel
initiateDCUtR :: DCUtRConfig -> StreamIO -> [Multiaddr] -> IO DCUtRResult
initiateDCUtR config stream addrs = do
  rttRef <- newIORef Nothing
  initiateDCUtRWithRTT config stream addrs rttRef

-- | Initiator variant that captures RTT for testing.
initiateDCUtRWithRTT :: DCUtRConfig -> StreamIO -> [Multiaddr] -> IORef (Maybe NominalDiffTime) -> IO DCUtRResult
initiateDCUtRWithRTT config stream addrs rttRef =
  runAttempts config (initiatorAttempt config stream addrs (Just rttRef) Nothing)

-- | Initiator variant that captures received addresses for testing.
initiateDCUtRCapture :: DCUtRConfig -> StreamIO -> [Multiaddr] -> IORef [BS.ByteString] -> IO DCUtRResult
initiateDCUtRCapture config stream addrs receivedRef =
  runAttempts config (initiatorAttempt config stream addrs Nothing (Just receivedRef))

-- | Peer A (handler): handle the DCUtR exchange over a relayed stream.
--
-- Flow (repeated up to 'dcMaxAttempts' times while the hole punch fails,
-- matching the initiator's retries of the exchange):
--   1. Read B's CONNECT
--   2. Send CONNECT with own observed addresses
--   3. Read SYNC
--   4. Dial all of B's addresses in parallel immediately
handleDCUtR :: DCUtRConfig -> StreamIO -> [Multiaddr] -> IO DCUtRResult
handleDCUtR config stream addrs =
  runAttempts config (handlerAttempt config stream addrs Nothing)

-- | Handler variant that captures received addresses for testing.
handleDCUtRCapture :: DCUtRConfig -> StreamIO -> [Multiaddr] -> IORef [BS.ByteString] -> IO DCUtRResult
handleDCUtRCapture config stream addrs receivedRef =
  runAttempts config (handlerAttempt config stream addrs (Just receivedRef))

-- Attempt loop

-- | Run hole punch attempts until one succeeds, a protocol error occurs, or
-- the attempt budget is exhausted.
runAttempts :: DCUtRConfig -> IO (Either AttemptError ()) -> IO DCUtRResult
runAttempts config attempt = go 1
  where
    maxAttempts = max 1 (dcMaxAttempts config)
    go n = do
      result <- attempt
      case result of
        Right () -> pure DCUtRSuccess
        Left (FatalError err) -> pure (DCUtRFailed err)
        Left (DialError err)
          | n >= maxAttempts -> pure (DCUtRFailed err)
          | otherwise -> go (n + 1)

-- | One initiator attempt: full CONNECT/CONNECT/SYNC exchange followed by the
-- synchronized parallel dial.
initiatorAttempt
  :: DCUtRConfig
  -> StreamIO
  -> [Multiaddr]
  -> Maybe (IORef (Maybe NominalDiffTime))
  -> Maybe (IORef [BS.ByteString])
  -> IO (Either AttemptError ())
initiatorAttempt config stream addrs mRttRef mCaptureRef = do
  let connectOut = HolePunchMessage { hpType = HPConnect, hpObsAddrs = map toBytes addrs }
  -- Step 1: Send CONNECT with our observed addresses. The spec starts the
  -- RTT timer when the CONNECT is sent, so take t0 before the (possibly
  -- blocking) relayed write.
  t0 <- getCurrentTime
  writeHolePunchMessage stream connectOut
  -- Step 2: Read A's CONNECT response (this measures RTT)
  result <- readHolePunchMessage stream maxDCUtRMessageSize
  case result of
    Left err -> pure (Left (FatalError ("failed to read CONNECT: " ++ err)))
    Right msg
      | hpType msg /= HPConnect -> pure (Left (FatalError "expected CONNECT message"))
      | otherwise -> do
          t1 <- getCurrentTime
          let rtt = diffUTCTime t1 t0
          mapM_ (`writeIORef` Just rtt) mRttRef
          mapM_ (`writeIORef` hpObsAddrs msg) mCaptureRef
          -- Step 3: Send SYNC
          writeHolePunchMessage stream (HolePunchMessage { hpType = HPSync, hpObsAddrs = [] })
          -- Step 4: Wait RTT/2, then dial all of A's addresses in parallel
          threadDelay (max 0 (round (rtt * 1000000 / 2)))
          dialAllConcurrently config (parseAddrs (hpObsAddrs msg))

-- | One handler attempt: answer the CONNECT/SYNC exchange, then dial all of
-- the initiator's addresses in parallel.
handlerAttempt
  :: DCUtRConfig
  -> StreamIO
  -> [Multiaddr]
  -> Maybe (IORef [BS.ByteString])
  -> IO (Either AttemptError ())
handlerAttempt config stream addrs mCaptureRef = do
  -- Step 1: Read B's CONNECT
  result <- readHolePunchMessage stream maxDCUtRMessageSize
  case result of
    Left err -> pure (Left (FatalError ("failed to read CONNECT: " ++ err)))
    Right msg
      | hpType msg /= HPConnect -> pure (Left (FatalError "expected CONNECT message"))
      | otherwise -> do
          mapM_ (`writeIORef` hpObsAddrs msg) mCaptureRef
          -- Step 2: Send our CONNECT response
          writeHolePunchMessage stream (HolePunchMessage { hpType = HPConnect, hpObsAddrs = map toBytes addrs })
          -- Step 3: Read SYNC
          syncResult <- readHolePunchMessage stream maxDCUtRMessageSize
          case syncResult of
            Left err -> pure (Left (FatalError ("failed to read SYNC: " ++ err)))
            Right syncMsg
              | hpType syncMsg /= HPSync -> pure (Left (FatalError "expected SYNC message"))
              | otherwise ->
                  -- Step 4: Dial all of B's addresses in parallel immediately
                  dialAllConcurrently config (parseAddrs (hpObsAddrs msg))

-- Helpers

-- | Parse binary multiaddr bytes into Multiaddrs, skipping invalid ones.
parseAddrs :: [BS.ByteString] -> [Multiaddr]
parseAddrs = foldr (\bs acc -> case fromBytes bs of Right a -> a : acc; Left _ -> acc) []

-- | Dial all addresses concurrently. The first successful dial wins and the
-- remaining dials are cancelled; if every dial fails the attempt is a
-- retryable 'DialError'.
dialAllConcurrently :: DCUtRConfig -> [Multiaddr] -> IO (Either AttemptError ())
dialAllConcurrently _config [] = pure (Left (DialError "no addresses to dial"))
dialAllConcurrently config addrs =
  bracket (mapM (async . dcDialer config) addrs) (mapM_ cancel) waitFirstSuccess
  where
    waitFirstSuccess [] = pure (Left (DialError "all dial attempts failed"))
    waitFirstSuccess pending = do
      (finished, result) <- waitAny pending
      case result of
        Right () -> pure (Right ())
        Left _err -> waitFirstSuccess (filter (/= finished) pending)
