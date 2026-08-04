-- | Circuit Relay v2 server: manage reservations and bridge streams.
--
-- Protocols:
--   /libp2p/circuit/relay/0.2.0/hop (client ↔ relay)
--
-- Provides:
--   - Reservation management (with expiration and limits)
--   - Stream bridging between source and target
--   - Resource limits (max reservations, max circuits, data/duration limits)
module LibP2P.NAT.Relay
  ( -- * Types
    RelayConfig (..)
  , RelayState (..)
  , ActiveReservation (..)
  , HopContext (..)
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
  , isRelayedAddr
    -- * Voucher constants
  , relayRsvpDomain
  , relayRsvpPayloadType
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32, Word64)
import LibP2P.NAT.Relay.Message
import LibP2P.Multiaddr (Multiaddr (..), fromBytes, toBytes)
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import LibP2P.Crypto.Key (KeyPair (..))
import LibP2P.Crypto.PeerId (PeerId (..), peerIdBytes)
import LibP2P.Crypto.SignedEnvelope (createEnvelope, encodeSignedEnvelope)
import LibP2P.Core.Varint (encodeUvarint)

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
  , arExpiration :: !Word64   -- ^ Absolute expiration time (UTC Unix time, seconds)
  } deriving (Show, Eq)

-- | Mutable relay server state.
data RelayState = RelayState
  { rsConfig        :: !RelayConfig
  , rsReservations  :: !(TVar (Map.Map PeerId ActiveReservation))
  , rsCircuitCounts :: !(TVar (Map.Map PeerId Int))
    -- ^ Active relayed circuits per peer (both initiator and target sides)
  }

-- | Create new relay state from configuration.
newRelayState :: RelayConfig -> IO RelayState
newRelayState config = RelayState config
  <$> newTVarIO Map.empty
  <*> newTVarIO Map.empty

-- | Per-request context for the hop handlers: the relay's own identity
-- (used to sign reservation vouchers), its public addresses (advertised in
-- the reservation), and the address the requesting connection arrived over
-- (used to refuse requests over already-relayed connections).
data HopContext = HopContext
  { hcRelayId    :: !PeerId       -- ^ Relay's own peer ID
  , hcRelayKey   :: !KeyPair      -- ^ Relay identity key, signs vouchers
  , hcRelayAddrs :: ![Multiaddr]
    -- ^ Relay public addresses including the trailing @/p2p/\<relay\>@
    -- component but without @/p2p-circuit@, per circuit-v2 reservation addrs
  , hcRemoteAddr :: !Multiaddr    -- ^ Address the request arrived over
  }

-- | Domain separation string for reservation voucher envelopes (circuit-v2).
relayRsvpDomain :: ByteString
relayRsvpDomain = "libp2p-relay-rsvp"

-- | Multicodec payload type for reservation vouchers (0x0302).
relayRsvpPayloadType :: ByteString
relayRsvpPayloadType = BS.pack [0x03, 0x02]

-- | Handle a RESERVE request from a peer.
handleReserve :: RelayState -> HopContext -> StreamIO -> PeerId -> IO ()
handleReserve state ctx stream peerId
  -- Per circuit-v2 spec, relays must not serve requests arriving over an
  -- already-relayed connection (no relay chains).
  | isRelayedAddr (hcRemoteAddr ctx) = sendHopStatus stream PermissionDenied
  | otherwise = do
      -- Per circuit-v2 spec, Reservation.expire is an absolute UTC Unix
      -- time in seconds, not a duration.
      now <- getPOSIXTime
      let expiration = floor now + rcReservationDuration (rsConfig state)
          reservation = ActiveReservation
            { arPeerId = peerId
            , arExpiration = expiration
            }
      granted <- atomically $ do
        reservations <- readTVar (rsReservations state)
        let limit = rcMaxReservations (rsConfig state)
            isRefresh = Map.member peerId reservations
        -- A refresh from an existing holder is not a new reservation: it
        -- must succeed even when the relay is at capacity.
        if not isRefresh && Map.size reservations >= limit
          then pure False
          else do
            modifyTVar' (rsReservations state) (Map.insert peerId reservation)
            pure True
      if not granted
        -- Per circuit-v2 spec, a reservation rejected because the relay is
        -- at capacity is RESERVATION_REFUSED (200); RESOURCE_LIMIT_EXCEEDED
        -- (201) is reserved for relayed-connection limits on CONNECT.
        then sendHopStatus stream ReservationRefused
        else do
          -- Send OK response with reservation info
          let resp = HopMessage
                { hopType = Just HopStatus
                , hopPeer = Nothing
                , hopReservation = Just Reservation
                    { rsvExpire = Just expiration
                    , rsvAddrs = map toBytes (hcRelayAddrs ctx)
                    , rsvVoucher = signVoucher ctx peerId expiration
                    }
                , hopLimit = Just RelayLimit
                    { rlDuration = Just (rcDefaultDurationLimit (rsConfig state))
                    , rlData = Just (rcDefaultDataLimit (rsConfig state))
                    }
                , hopStatus = Just RelayOK
                }
          writeHopMessage stream resp

-- | Sign a reservation voucher: a circuit-v2 Voucher payload wrapped in an
-- RFC 0002 signed envelope under the relay-rsvp domain.
signVoucher :: HopContext -> PeerId -> Word64 -> Maybe ByteString
signVoucher ctx peerId expiration =
  let payload = encodeVoucher Voucher
        { vRelay = peerIdBytes (hcRelayId ctx)
        , vPeer = peerIdBytes peerId
        , vExpiration = expiration
        }
      kp = hcRelayKey ctx
  in case createEnvelope (kpPrivate kp) (kpPublic kp) relayRsvpDomain relayRsvpPayloadType payload of
       Left _    -> Nothing  -- voucher is optional per spec; omit if signing fails
       Right env -> Just (encodeSignedEnvelope env)

-- | Handle a CONNECT request from a peer.
-- The openStopStream callback is used to open a stop stream to the target.
handleConnect :: RelayState -> HopContext -> StreamIO -> PeerId -> HopMessage -> (PeerId -> IO (Maybe StreamIO)) -> IO ()
handleConnect state ctx stream sourcePeerId msg openStopStream
  -- Per circuit-v2 spec, a CONNECT arriving over an already-relayed
  -- connection is refused: circuits cannot be chained through relays.
  | isRelayedAddr (hcRemoteAddr ctx) = sendHopStatus stream PermissionDenied
  | otherwise = case hopPeer msg of
    Nothing -> sendHopStatus stream MalformedMessage
    Just peer -> do
      let targetId = PeerId (rpId peer)
      now <- getPOSIXTime
      let nowSecs = floor now :: Word64
      -- Check target has an unexpired reservation; drop it if it has expired.
      hasReservation <- atomically $ do
        reservations <- readTVar (rsReservations state)
        case Map.lookup targetId reservations of
          Nothing -> pure False
          Just rsv
            | arExpiration rsv <= nowSecs -> do
                modifyTVar' (rsReservations state) (Map.delete targetId)
                pure False
            | otherwise -> pure True
      if not hasReservation
        then sendHopStatus stream NoReservation
        else do
          -- Acquire a circuit slot for both peers, enforcing the advertised
          -- per-peer circuit limit atomically with the check.
          acquired <- atomically $ acquireCircuitSlots state sourcePeerId targetId
          if not acquired
            then sendHopStatus stream ResourceLimitExceeded
            else establishCircuit state stream sourcePeerId targetId openStopStream
                   `finally` atomically (releaseCircuitSlots state sourcePeerId targetId)

-- | Reserve one circuit slot for each of the two peers if neither is at the
-- configured per-peer limit. Returns False without modifying state otherwise.
acquireCircuitSlots :: RelayState -> PeerId -> PeerId -> STM Bool
acquireCircuitSlots state sourceId targetId = do
  counts <- readTVar (rsCircuitCounts state)
  let limit = rcMaxCircuits (rsConfig state)
      countOf pid = Map.findWithDefault 0 pid counts
  if countOf sourceId >= limit || countOf targetId >= limit
    then pure False
    else do
      let bump = Map.insertWith (+) sourceId 1 . Map.insertWith (+) targetId 1
      writeTVar (rsCircuitCounts state) (bump counts)
      pure True

-- | Release the circuit slots acquired by 'acquireCircuitSlots'.
releaseCircuitSlots :: RelayState -> PeerId -> PeerId -> STM ()
releaseCircuitSlots state sourceId targetId =
  modifyTVar' (rsCircuitCounts state) (dropOne sourceId . dropOne targetId)
  where
    dropOne = Map.update (\n -> if n <= 1 then Nothing else Just (n - 1))

-- | Open a stop stream to the target, exchange CONNECT/STATUS, and bridge the
-- two streams until EOF or a limit is exceeded.
establishCircuit :: RelayState -> StreamIO -> PeerId -> PeerId -> (PeerId -> IO (Maybe StreamIO)) -> IO ()
establishCircuit state stream sourcePeerId targetId openStopStream = do
          -- Try to open stop stream to target
          mStopStream <- openStopStream targetId
          case mStopStream of
            Nothing -> sendHopStatus stream ConnectionFailed
            Just stopStream -> do
              -- Send CONNECT to target via stop protocol
              let stopMsg = StopMessage
                    { stopType = Just StopConnect
                    , stopPeer = Just RelayPeer
                        { rpId = let PeerId bs = sourcePeerId in bs
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
-- Terminates when either direction closes or limits are exceeded, then closes
-- both streams so neither end is left with a half-open circuit.
bridgeStreams :: Maybe RelayLimit -> StreamIO -> StreamIO -> IO ()
bridgeStreams mLimit streamA streamB = do
  let dataLimit = case mLimit >>= rlData of
        Just n  -> fromIntegral n :: Int
        Nothing -> maxBound
      -- Duration limit in microseconds for threadDelay
      mDurationMicros = case mLimit >>= rlDuration of
        Just secs | secs > 0 -> Just (fromIntegral secs * 1000000 :: Int)
        _                    -> Nothing
  -- Track bytes transferred in each direction
  countAtoB <- newIORef (0 :: Int)
  countBtoA <- newIORef (0 :: Int)
  -- Forward A→B and B→A concurrently; terminate when either finishes
  let forwarding = race_
        (forwardWithLimit streamA streamB countAtoB dataLimit)
        (forwardWithLimit streamB streamA countBtoA dataLimit)
  -- Enforce the advertised duration limit: close the circuit when it elapses
  (case mDurationMicros of
     Nothing     -> forwarding
     Just micros -> race_ (threadDelay micros) forwarding)
    `finally` do
      streamClose streamA
      streamClose streamB

-- | Forward bytes from source to destination with a byte limit.
-- The limit is checked before each read, so the circuit terminates as soon
-- as exactly @limit@ bytes have been forwarded — no byte beyond the limit
-- is consumed from the source.
forwardWithLimit :: StreamIO -> StreamIO -> IORef Int -> Int -> IO ()
forwardWithLimit src dst countRef limit = go
  where
    go = do
      count <- readIORef countRef
      if count >= limit
        then pure ()  -- limit reached, stop forwarding
        else do
          b <- streamReadByte src
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

-- | Check whether a multiaddr contains a p2p-circuit component.
isRelayedAddr :: Multiaddr -> Bool
isRelayedAddr (Multiaddr ps) = P2PCircuit `elem` ps

-- | Check whether raw multiaddr bytes describe a relayed connection.
-- Decodes the bytes structurally: the p2p-circuit byte pattern occurring
-- inside another component (e.g. a peer ID) does not count, unlike the
-- previous substring heuristic. Undecodable bytes are not relayed.
isRelayedConnection :: ByteString -> Bool
isRelayedConnection = either (const False) isRelayedAddr . fromBytes
