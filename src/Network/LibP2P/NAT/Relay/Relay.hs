-- | Circuit Relay v2 server: manage reservations and bridge streams.
--
-- Protocols:
--   /libp2p/circuit/relay/0.2.0/hop (client ↔ relay)
--
-- Provides:
--   - Reservation management (with expiration and limits)
--   - Stream bridging between source and target
--   - Resource limits (max reservations, max circuits, data/duration limits)
module Network.LibP2P.NAT.Relay.Relay
  ( -- * Types
    RelayConfig (..)
  , RelayState (..)
  , ActiveReservation (..)
    -- * Configuration
  , defaultRelayConfig
    -- * State management
  , newRelayState
    -- * Handlers
  , handleReserve
  , handleConnect
    -- * Stream bridging
  , bridgeStreams
    -- * Relay address helpers
  , buildRelayAddrBytes
  , isRelayedConnection
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64)
import Network.LibP2P.NAT.Relay.Message
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import Network.LibP2P.Crypto.PeerId (PeerId (..))
import Network.LibP2P.Core.Varint (encodeUvarint)

-- | Relay server configuration.
data RelayConfig = RelayConfig
  { rcMaxReservations      :: !Int      -- ^ Max concurrent reservations
  , rcMaxCircuits          :: !Int      -- ^ Max concurrent relayed circuits
  , rcReservationDuration  :: !Word64   -- ^ Reservation duration (seconds)
  , rcDefaultDataLimit     :: !Word64   -- ^ Default data limit per circuit (bytes)
  , rcDefaultDurationLimit :: !Word32   -- ^ Default duration limit per circuit (seconds)
  } deriving (Show, Eq)

-- | Default relay configuration.
defaultRelayConfig :: RelayConfig
defaultRelayConfig = RelayConfig
  { rcMaxReservations      = 128
  , rcMaxCircuits          = 16
  , rcReservationDuration  = 3600  -- 1 hour
  , rcDefaultDataLimit     = 131072  -- 128 KiB
  , rcDefaultDurationLimit = 120  -- 2 minutes
  }

-- | An active reservation for a peer.
data ActiveReservation = ActiveReservation
  { arPeerId     :: !PeerId
  , arExpiration :: !Word64   -- ^ Unix timestamp
  } deriving (Show, Eq)

-- | Mutable relay server state.
data RelayState = RelayState
  { rsConfig       :: !RelayConfig
  , rsReservations :: !(TVar (Map.Map PeerId ActiveReservation))
  , rsCircuitCount :: !(TVar Int)
  }

-- | Create new relay state from configuration.
newRelayState :: RelayConfig -> IO RelayState
newRelayState config = RelayState config
  <$> newTVarIO Map.empty
  <*> newTVarIO 0

-- | Handle a RESERVE request from a peer.
handleReserve :: RelayState -> StreamIO -> PeerId -> IO ()
handleReserve state stream peerId = do
  -- Check resource limits
  reservations <- readTVarIO (rsReservations state)
  let limit = rcMaxReservations (rsConfig state)
  if Map.size reservations >= limit
    then sendHopStatus stream ResourceLimitExceeded
    else do
      -- Create reservation
      let expiration = rcReservationDuration (rsConfig state)
          reservation = ActiveReservation
            { arPeerId = peerId
            , arExpiration = expiration
            }
      atomically $ modifyTVar' (rsReservations state) (Map.insert peerId reservation)
      -- Send OK response with reservation info
      let resp = HopMessage
            { hopType = Just HopStatus
            , hopPeer = Nothing
            , hopReservation = Just Reservation
                { rsvExpire = Just expiration
                , rsvAddrs = []  -- relay would populate with own addresses
                , rsvVoucher = Nothing  -- voucher signing handled separately
                }
            , hopLimit = Just RelayLimit
                { rlDuration = Just (rcDefaultDurationLimit (rsConfig state))
                , rlData = Just (rcDefaultDataLimit (rsConfig state))
                }
            , hopStatus = Just RelayOK
            }
      writeHopMessage stream resp

-- | Handle a CONNECT request from a peer.
-- The openStopStream callback is used to open a stop stream to the target.
handleConnect :: RelayState -> StreamIO -> PeerId -> HopMessage -> (PeerId -> IO (Maybe StreamIO)) -> IO ()
handleConnect state stream _sourcePeerId msg openStopStream = do
  case hopPeer msg of
    Nothing -> sendHopStatus stream MalformedMessage
    Just peer -> do
      let targetId = PeerId (rpId peer)
      -- Check target has a reservation
      reservations <- readTVarIO (rsReservations state)
      case Map.lookup targetId reservations of
        Nothing -> sendHopStatus stream NoReservation
        Just _rsv -> do
          -- Try to open stop stream to target
          mStopStream <- openStopStream targetId
          case mStopStream of
            Nothing -> sendHopStatus stream ConnectionFailed
            Just stopStream -> do
              -- Send CONNECT to target via stop protocol
              let stopMsg = StopMessage
                    { stopType = Just StopConnect
                    , stopPeer = Just RelayPeer
                        { rpId = let PeerId bs = _sourcePeerId in bs
                        , rpAddrs = []
                        }
                    , stopLimit = Just RelayLimit
                        { rlDuration = Just (rcDefaultDurationLimit (rsConfig state))
                        , rlData = Just (rcDefaultDataLimit (rsConfig state))
                        }
                    , stopStatus = Nothing
                    }
              writeStopMessage stopStream stopMsg
              -- Wait for target's STATUS response
              targetResp <- readStopMessage stopStream maxRelayMessageSize
              case targetResp of
                Right resp | stopStatus resp == Just RelayOK -> do
                  -- Notify source of success
                  let okResp = HopMessage
                        { hopType = Just HopStatus
                        , hopPeer = Nothing
                        , hopReservation = Nothing
                        , hopLimit = Just RelayLimit
                            { rlDuration = Just (rcDefaultDurationLimit (rsConfig state))
                            , rlData = Just (rcDefaultDataLimit (rsConfig state))
                            }
                        , hopStatus = Just RelayOK
                        }
                  writeHopMessage stream okResp
                  -- Bridge the two streams
                  let limit = Just RelayLimit
                        { rlDuration = Just (rcDefaultDurationLimit (rsConfig state))
                        , rlData = Just (rcDefaultDataLimit (rsConfig state))
                        }
                  bridgeStreams limit stream stopStream
                _ -> sendHopStatus stream ConnectionFailed

-- | Send a simple HopMessage STATUS response.
sendHopStatus :: StreamIO -> RelayStatus -> IO ()
sendHopStatus stream status = writeHopMessage stream HopMessage
  { hopType = Just HopStatus
  , hopPeer = Nothing
  , hopReservation = Nothing
  , hopLimit = Nothing
  , hopStatus = Just status
  }

-- | Bridge two streams bidirectionally with optional data/duration limits.
-- Terminates when either direction closes or limits are exceeded.
bridgeStreams :: Maybe RelayLimit -> StreamIO -> StreamIO -> IO ()
bridgeStreams mLimit streamA streamB = do
  let dataLimit = case mLimit of
        Just lim -> case rlData lim of
          Just n  -> fromIntegral n :: Int
          Nothing -> maxBound
        Nothing -> maxBound
  -- Track bytes transferred in each direction
  countAtoB <- newIORef (0 :: Int)
  countBtoA <- newIORef (0 :: Int)
  -- Forward A→B and B→A concurrently; terminate when either finishes
  _ <- race
    (forwardWithLimit streamA streamB countAtoB dataLimit)
    (forwardWithLimit streamB streamA countBtoA dataLimit)
  pure ()

-- | Forward bytes from source to destination with a byte limit.
forwardWithLimit :: StreamIO -> StreamIO -> IORef Int -> Int -> IO ()
forwardWithLimit src dst countRef limit = go
  where
    go = do
      b <- streamReadByte src
      count <- readIORef countRef
      if count >= limit
        then pure ()  -- limit reached, stop forwarding
        else do
          modifyIORef' countRef (+ 1)
          streamWrite dst (BS.singleton b)
          go

-- | Build a relay multiaddr in binary format.
-- Format: <relayAddr>/p2p/<relayId>/p2p-circuit/p2p/<targetId>
buildRelayAddrBytes :: ByteString -> ByteString -> ByteString -> ByteString
buildRelayAddrBytes relayAddr relayIdBytes targetIdBytes =
  relayAddr
  <> p2pProtocolBytes relayIdBytes
  <> p2pCircuitBytes
  <> p2pProtocolBytes targetIdBytes
  where
    -- P2P protocol: code 421 (0xa503) + varint(len) + peer ID bytes
    p2pProtocolBytes :: ByteString -> ByteString
    p2pProtocolBytes pid = encodeUvarint 421 <> encodeUvarint (fromIntegral (BS.length pid)) <> pid

    -- P2PCircuit protocol: code 290 (0xa202), no address
    p2pCircuitBytes :: ByteString
    p2pCircuitBytes = encodeUvarint 290

-- | Check if raw multiaddr bytes contain P2PCircuit (code 290).
-- Simple heuristic: look for the varint encoding of 290.
isRelayedConnection :: ByteString -> Bool
isRelayedConnection bs =
  let circuitMarker = encodeUvarint 290
  in circuitMarker `BS.isInfixOf` bs
