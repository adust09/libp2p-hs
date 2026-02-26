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

import Control.Concurrent.STM (TVar)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (NominalDiffTime)
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr)
import Network.LibP2P.Switch.Types
  ( BackoffEntry
  , Connection
  , DialError (..)
  , Switch
  )

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
-- Returns Right () if no backoff is active, Left DialBackoff otherwise.
checkBackoff :: TVar (Map.Map PeerId BackoffEntry) -> PeerId -> IO (Either DialError ())
checkBackoff _backoffsVar _pid = pure (Left DialBackoff)  -- STUB

-- | Record a backoff after a failed dial.
-- First failure: 5s. Each subsequent: duration * 2, capped at 300s.
recordBackoff :: TVar (Map.Map PeerId BackoffEntry) -> PeerId -> IO ()
recordBackoff _backoffsVar _pid = pure ()  -- STUB

-- | Clear backoff for a peer (called on successful connection).
clearBackoff :: TVar (Map.Map PeerId BackoffEntry) -> PeerId -> IO ()
clearBackoff _backoffsVar _pid = pure ()  -- STUB

-- | Dial a peer, reusing existing connections or establishing new ones.
--
-- Implements the full dial flow from docs/08-switch.md §Dial Flow:
--   1. Pool reuse: return existing Open connection if available
--   2. Backoff check: reject if peer recently failed
--   3. Deduplication: coalesce concurrent dials to same peer
--   4. Staggered parallel dial with 250ms delay (Happy Eyeballs)
--   5. First success: upgrade, add to pool, return
--   6. All fail: record backoff, return error
dial :: Switch -> PeerId -> [Multiaddr] -> IO (Either DialError Connection)
dial _sw _remotePeerId _addrs = pure (Left (DialAllFailed ["not implemented"]))  -- STUB
