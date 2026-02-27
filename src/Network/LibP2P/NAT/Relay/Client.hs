-- | Circuit Relay v2 client: make reservations and connect through relays.
--
-- Client-side operations:
--   - makeReservation: send RESERVE to relay, receive reservation info
--   - connectViaRelay: send CONNECT to relay, receive relayed stream
--   - handleStop: target receives relay'd connection notification
module Network.LibP2P.NAT.Relay.Client
  ( -- * Client operations
    makeReservation
  , connectViaRelay
    -- * Target handler
  , handleStop
  ) where

import Network.LibP2P.NAT.Relay.Message
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import Network.LibP2P.Crypto.PeerId (PeerId (..))

-- | Send a RESERVE request to a relay and receive the reservation response.
makeReservation :: StreamIO -> IO (Either String HopMessage)
makeReservation stream = do
  let reserveMsg = HopMessage
        { hopType = Just HopReserve
        , hopPeer = Nothing
        , hopReservation = Nothing
        , hopLimit = Nothing
        , hopStatus = Nothing
        }
  writeHopMessage stream reserveMsg
  readHopMessage stream maxRelayMessageSize

-- | Send a CONNECT request to a relay to reach a target peer.
connectViaRelay :: StreamIO -> PeerId -> IO (Either String HopMessage)
connectViaRelay stream (PeerId targetIdBytes) = do
  let connectReq = HopMessage
        { hopType = Just HopConnect
        , hopPeer = Just RelayPeer
            { rpId = targetIdBytes
            , rpAddrs = []
            }
        , hopReservation = Nothing
        , hopLimit = Nothing
        , hopStatus = Nothing
        }
  writeHopMessage stream connectReq
  readHopMessage stream maxRelayMessageSize

-- | Handle an incoming stop connection from a relay (target side).
-- Reads the CONNECT message, responds with OK, and returns the source peer ID and limit.
handleStop :: StreamIO -> IO (Either String (PeerId, Maybe RelayLimit))
handleStop stream = do
  result <- readStopMessage stream maxRelayMessageSize
  case result of
    Left err -> pure (Left err)
    Right msg -> case stopType msg of
      Just StopConnect -> do
        case stopPeer msg of
          Nothing -> do
            sendStopStatus stream MalformedMessage
            pure (Left "stop CONNECT missing peer info")
          Just peer -> do
            -- Respond with OK
            sendStopStatus stream RelayOK
            let sourcePeerId = PeerId (rpId peer)
            pure (Right (sourcePeerId, stopLimit msg))
      _ -> pure (Left "unexpected stop message type")

-- | Send a StopMessage STATUS response.
sendStopStatus :: StreamIO -> RelayStatus -> IO ()
sendStopStatus stream status = writeStopMessage stream StopMessage
  { stopType = Just StopStatus
  , stopPeer = Nothing
  , stopLimit = Nothing
  , stopStatus = Just status
  }
