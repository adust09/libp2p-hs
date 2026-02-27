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
module Network.LibP2P.NAT.DCUtR.DCUtR
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
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Network.LibP2P.NAT.DCUtR.Message
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr, toBytes, fromBytes)

-- | DCUtR configuration.
data DCUtRConfig = DCUtRConfig
  { dcMaxRetries :: !Int
    -- ^ Maximum number of retry attempts (spec says 3 total = 1 initial + 2 retries)
  , dcDialer     :: !(Multiaddr -> IO (Either String ()))
    -- ^ Injectable dial function for testing
  }

-- | DCUtR result.
data DCUtRResult = DCUtRSuccess | DCUtRFailed String
  deriving (Show, Eq)

-- | Peer B (initiator): run the DCUtR exchange over a relayed stream.
--
-- Flow:
--   1. Send CONNECT with own observed addresses
--   2. Read A's CONNECT (measure RTT)
--   3. Send SYNC
--   4. Wait RTT/2, then dial A's addresses
initiateDCUtR :: DCUtRConfig -> StreamIO -> [Multiaddr] -> IO DCUtRResult
initiateDCUtR config stream addrs = do
  rttRef <- newIORef Nothing
  initiateDCUtRWithRTT config stream addrs rttRef

-- | Initiator variant that captures RTT for testing.
initiateDCUtRWithRTT :: DCUtRConfig -> StreamIO -> [Multiaddr] -> IORef (Maybe NominalDiffTime) -> IO DCUtRResult
initiateDCUtRWithRTT config stream addrs rttRef = do
  let addrBytes = map toBytes addrs
  -- Step 1: Send CONNECT with our observed addresses
  let connectOut = HolePunchMessage { hpType = HPConnect, hpObsAddrs = addrBytes }
  writeHolePunchMessage stream connectOut
  t0 <- getCurrentTime
  -- Step 2: Read A's CONNECT response (this measures RTT)
  result <- readHolePunchMessage stream maxDCUtRMessageSize
  case result of
    Left err -> pure (DCUtRFailed $ "failed to read CONNECT: " ++ err)
    Right msg
      | hpType msg /= HPConnect -> pure (DCUtRFailed "expected CONNECT message")
      | otherwise -> do
          t1 <- getCurrentTime
          let rtt = diffUTCTime t1 t0
          writeIORef rttRef (Just rtt)
          -- Step 3: Send SYNC
          let syncOut = HolePunchMessage { hpType = HPSync, hpObsAddrs = [] }
          writeHolePunchMessage stream syncOut
          -- Step 4: Wait RTT/2, then dial A's addresses
          let halfRTTMicros = max 0 (round (rtt * 1000000 / 2) :: Int)
          threadDelay halfRTTMicros
          let remoteAddrs = parseAddrs (hpObsAddrs msg)
          dialResult <- tryDialAddrs config remoteAddrs
          case dialResult of
            Right () -> pure DCUtRSuccess
            Left err -> pure (DCUtRFailed $ "dial failed: " ++ err)

-- | Initiator variant that captures received addresses for testing.
initiateDCUtRCapture :: DCUtRConfig -> StreamIO -> [Multiaddr] -> IORef [BS.ByteString] -> IO DCUtRResult
initiateDCUtRCapture config stream addrs receivedRef = do
  let addrBytes = map toBytes addrs
  let connectOut = HolePunchMessage { hpType = HPConnect, hpObsAddrs = addrBytes }
  writeHolePunchMessage stream connectOut
  result <- readHolePunchMessage stream maxDCUtRMessageSize
  case result of
    Left err -> pure (DCUtRFailed $ "failed to read CONNECT: " ++ err)
    Right msg
      | hpType msg /= HPConnect -> pure (DCUtRFailed "expected CONNECT message")
      | otherwise -> do
          writeIORef receivedRef (hpObsAddrs msg)
          let syncOut = HolePunchMessage { hpType = HPSync, hpObsAddrs = [] }
          writeHolePunchMessage stream syncOut
          let remoteAddrs = parseAddrs (hpObsAddrs msg)
          dialResult <- tryDialAddrs config remoteAddrs
          case dialResult of
            Right () -> pure DCUtRSuccess
            Left err -> pure (DCUtRFailed $ "dial failed: " ++ err)

-- | Peer A (handler): handle the DCUtR exchange over a relayed stream.
--
-- Flow:
--   1. Read B's CONNECT
--   2. Send CONNECT with own observed addresses
--   3. Read SYNC
--   4. Dial B's addresses immediately
handleDCUtR :: DCUtRConfig -> StreamIO -> [Multiaddr] -> IO DCUtRResult
handleDCUtR config stream addrs = do
  let addrBytes = map toBytes addrs
  -- Step 1: Read B's CONNECT
  result <- readHolePunchMessage stream maxDCUtRMessageSize
  case result of
    Left err -> pure (DCUtRFailed $ "failed to read CONNECT: " ++ err)
    Right msg
      | hpType msg /= HPConnect -> pure (DCUtRFailed "expected CONNECT message")
      | otherwise -> do
          -- Step 2: Send our CONNECT response
          let connectResp = HolePunchMessage { hpType = HPConnect, hpObsAddrs = addrBytes }
          writeHolePunchMessage stream connectResp
          -- Step 3: Read SYNC
          syncResult <- readHolePunchMessage stream maxDCUtRMessageSize
          case syncResult of
            Left err -> pure (DCUtRFailed $ "failed to read SYNC: " ++ err)
            Right syncMsg
              | hpType syncMsg /= HPSync -> pure (DCUtRFailed "expected SYNC message")
              | otherwise -> do
                  -- Step 4: Dial B's addresses immediately
                  let remoteAddrs = parseAddrs (hpObsAddrs msg)
                  dialResult <- tryDialAddrs config remoteAddrs
                  case dialResult of
                    Right () -> pure DCUtRSuccess
                    Left err -> pure (DCUtRFailed $ "dial failed: " ++ err)

-- | Handler variant that captures received addresses for testing.
handleDCUtRCapture :: DCUtRConfig -> StreamIO -> [Multiaddr] -> IORef [BS.ByteString] -> IO DCUtRResult
handleDCUtRCapture config stream addrs receivedRef = do
  let addrBytes = map toBytes addrs
  result <- readHolePunchMessage stream maxDCUtRMessageSize
  case result of
    Left err -> pure (DCUtRFailed $ "failed to read CONNECT: " ++ err)
    Right msg
      | hpType msg /= HPConnect -> pure (DCUtRFailed "expected CONNECT message")
      | otherwise -> do
          writeIORef receivedRef (hpObsAddrs msg)
          let connectResp = HolePunchMessage { hpType = HPConnect, hpObsAddrs = addrBytes }
          writeHolePunchMessage stream connectResp
          syncResult <- readHolePunchMessage stream maxDCUtRMessageSize
          case syncResult of
            Left err -> pure (DCUtRFailed $ "failed to read SYNC: " ++ err)
            Right syncMsg
              | hpType syncMsg /= HPSync -> pure (DCUtRFailed "expected SYNC message")
              | otherwise -> do
                  let remoteAddrs = parseAddrs (hpObsAddrs msg)
                  dialResult <- tryDialAddrs config remoteAddrs
                  case dialResult of
                    Right () -> pure DCUtRSuccess
                    Left err -> pure (DCUtRFailed $ "dial failed: " ++ err)

-- Helpers

-- | Parse binary multiaddr bytes into Multiaddrs, skipping invalid ones.
parseAddrs :: [BS.ByteString] -> [Multiaddr]
parseAddrs = foldr (\bs acc -> case fromBytes bs of Right a -> a : acc; Left _ -> acc) []

-- | Try to dial any of the given addresses. Returns Right () on first success.
tryDialAddrs :: DCUtRConfig -> [Multiaddr] -> IO (Either String ())
tryDialAddrs _config [] = pure (Left "no addresses to dial")
tryDialAddrs config (addr:rest) = do
  result <- dcDialer config addr
  case result of
    Right () -> pure (Right ())
    Left _err
      | null rest -> pure (Left "all dial attempts failed")
      | otherwise -> tryDialAddrs config rest
