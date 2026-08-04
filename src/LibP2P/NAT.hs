-- | NAT traversal handler registration (specs/autonat, specs/relay, specs/relay/DCUtR).
--
-- Wires the AutoNAT, Circuit Relay v2, and DCUtR module implementations
-- into the Switch protocol registry, in the style of
-- 'LibP2P.Protocol.Identify.registerIdentifyHandlers':
--
--   /libp2p/autonat/1.0.0            — AutoNAT dial-back server
--   /libp2p/circuit/relay/0.2.0/hop  — Circuit Relay v2 relay server
--   /libp2p/circuit/relay/0.2.0/stop — Circuit Relay v2 target (inbound relayed streams)
--   /libp2p/dcutr                    — DCUtR hole-punch coordination (handler side)
module LibP2P.NAT
  ( -- * Configuration
    NATConfig (..)
  , defaultNATConfig
    -- * Registration
  , registerNATHandlers
  , registerAutoNATHandler
  , registerRelayHopHandler
  , registerRelayStopHandler
  , registerDCUtRHandler
  ) where

import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, catch, try)
import LibP2P.Crypto.PeerId (PeerId, peerIdBytes)
import LibP2P.Multiaddr (Multiaddr (..), encapsulate)
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.MultistreamSelect.Negotiation
  ( NegotiationResult (..)
  , StreamIO (..)
  , negotiateInitiator
  )
import LibP2P.NAT.AutoNAT (AutoNATConfig (..), handleAutoNAT)
import LibP2P.NAT.AutoNAT.Message (autoNATProtocolId)
import LibP2P.NAT.DCUtR (DCUtRConfig (..), handleDCUtR)
import LibP2P.NAT.DCUtR.Message (dcutrProtocolId)
import LibP2P.NAT.Relay
  ( HopContext (..)
  , RelayConfig
  , RelayState
  , defaultRelayConfig
  , handleConnect
  , handleReserve
  , newRelayState
  )
import LibP2P.NAT.Relay.Client (handleStop)
import LibP2P.NAT.Relay.Message
  ( HopMessage (..)
  , HopMessageType (..)
  , RelayLimit
  , RelayStatus (..)
  , hopProtocolId
  , maxRelayMessageSize
  , readHopMessage
  , stopProtocolId
  , writeHopMessage
  )
import LibP2P.Switch (selectTransport, setStreamHandler)
import LibP2P.Switch.ConnPool (lookupConn)
import LibP2P.Switch.Connection (newStream)
import LibP2P.Switch.Dial (dial)
import LibP2P.Switch.Listen (switchListenAddrs)
import LibP2P.Switch.Types
  ( Connection (..)
  , MuxerSession (..)
  , Switch (..)
  )
import LibP2P.Switch.Upgrade (upgradeOutbound)
import LibP2P.Transport (Transport (..))

-- | Configuration for the NAT traversal handlers.
data NATConfig = NATConfig
  { ncRelayConfig     :: !RelayConfig
    -- ^ Resource limits for the Circuit Relay v2 server side
  , ncOnRelayedStream :: !(PeerId -> Maybe RelayLimit -> StreamIO -> IO ())
    -- ^ Invoked when a relay delivers an inbound relayed stream (stop
    -- protocol) after the CONNECT/OK exchange: source peer, limit
    -- advertised by the relay, and the relayed stream. The application
    -- owns the stream from this point (e.g. to run DCUtR over it).
  }

-- | Default NAT configuration: default relay limits, and inbound relayed
-- streams are left to the remote end (no local consumer).
defaultNATConfig :: NATConfig
defaultNATConfig = NATConfig
  { ncRelayConfig     = defaultRelayConfig
  , ncOnRelayedStream = \_ _ _ -> pure ()
  }

-- | Register all four NAT protocol handlers on the Switch.
--
-- Creates the relay server state from 'ncRelayConfig' and returns it so
-- callers can inspect reservations/circuits.
registerNATHandlers :: Switch -> NATConfig -> IO RelayState
registerNATHandlers sw config = do
  relayState <- newRelayState (ncRelayConfig config)
  registerAutoNATHandler sw
  registerRelayHopHandler sw relayState
  registerRelayStopHandler sw (ncOnRelayedStream config)
  registerDCUtRHandler sw
  pure relayState

-- | Register the AutoNAT server handler (/libp2p/autonat/1.0.0).
--
-- The dial-back deliberately bypasses the connection pool: reusing the
-- requester's existing connection would always report success. Instead a
-- fresh transport dial + upgrade verifies both reachability and identity,
-- and the probe connection is closed immediately (go-libp2p uses a
-- separate dialer host for the same reason).
registerAutoNATHandler :: Switch -> IO ()
registerAutoNATHandler sw =
  setStreamHandler sw autoNATProtocolId $ \conn stream ->
    let config = AutoNATConfig
          { natThreshold = 3
          , natDialBack  = freshDialBack sw
          }
    in handleAutoNAT config stream (connPeerId conn) (connRemoteAddr conn)

-- | Dial back a peer on a fresh connection, verify its identity, and close.
freshDialBack :: Switch -> PeerId -> [Multiaddr] -> IO (Either String ())
freshDialBack _ _ [] = pure (Left "dial-back: no addresses")
freshDialBack sw pid (addr : rest) = do
  result <- try (probeAddr sw pid addr)
  case result of
    Right (Right ()) -> pure (Right ())
    Right (Left err)
      | null rest -> pure (Left err)
      | otherwise -> freshDialBack sw pid rest
    Left (e :: SomeException)
      | null rest -> pure (Left (show e))
      | otherwise -> freshDialBack sw pid rest

-- | Probe a single address: transport dial, upgrade, check peer identity.
probeAddr :: Switch -> PeerId -> Multiaddr -> IO (Either String ())
probeAddr sw pid addr = do
  mTransport <- selectTransport sw addr
  case mTransport of
    Nothing -> pure (Left ("dial-back: no transport for " ++ show addr))
    Just transport -> do
      rawConn <- transportDial transport addr
      conn <- upgradeOutbound (swIdentityKey sw) rawConn
      let matches = connPeerId conn == pid
      muxClose (connSession conn) `catch` \(_ :: SomeException) -> pure ()
      pure $ if matches
        then Right ()
        else Left "dial-back: peer identity mismatch"

-- | Register the Circuit Relay v2 hop handler
-- (/libp2p/circuit/relay/0.2.0/hop): serve RESERVE and CONNECT requests.
registerRelayHopHandler :: Switch -> RelayState -> IO ()
registerRelayHopHandler sw relayState =
  setStreamHandler sw hopProtocolId $ \conn stream -> do
    result <- readHopMessage stream maxRelayMessageSize
    case result of
      Left _ -> pure ()
      Right msg -> case hopType msg of
        Just HopReserve -> do
          ctx <- switchHopContext sw conn
          handleReserve relayState ctx stream (connPeerId conn)
        Just HopConnect -> do
          ctx <- switchHopContext sw conn
          handleConnect relayState ctx stream (connPeerId conn) msg (openStopStream sw)
        _ -> writeHopMessage stream HopMessage
          { hopType = Just HopStatus
          , hopPeer = Nothing
          , hopReservation = Nothing
          , hopLimit = Nothing
          , hopStatus = Just UnexpectedMessage
          }

-- | Build the per-request hop context from the Switch: the relay's own
-- identity (signs reservation vouchers), its listen addresses with the
-- @/p2p/\<relay\>@ suffix the circuit-v2 spec requires for reservation
-- addrs, and the address the requesting connection arrived over.
switchHopContext :: Switch -> Connection -> IO HopContext
switchHopContext sw conn = do
  addrs <- switchListenAddrs sw
  let relayP2P = Multiaddr [P2P (peerIdBytes (swLocalPeerId sw))]
  pure HopContext
    { hcRelayId    = swLocalPeerId sw
    , hcRelayKey   = swIdentityKey sw
    , hcRelayAddrs = map (`encapsulate` relayP2P) addrs
    , hcRemoteAddr = connRemoteAddr conn
    }

-- | Open a stop-protocol stream to the circuit target over an existing
-- connection. Returns Nothing when the target is not connected or the
-- stop protocol cannot be negotiated.
openStopStream :: Switch -> PeerId -> IO (Maybe StreamIO)
openStopStream sw targetId = do
  result <- try $ do
    mConn <- atomically $ lookupConn (swConnPool sw) targetId
    case mConn of
      Nothing -> pure Nothing
      Just conn -> do
        streamOrErr <- newStream sw conn
        case streamOrErr of
          Left _ -> pure Nothing
          Right stream -> do
            negotiated <- negotiateInitiator stream [stopProtocolId]
            case negotiated of
              Accepted _ -> pure (Just stream)
              NoProtocol -> do
                streamClose stream `catch` \(_ :: SomeException) -> pure ()
                pure Nothing
  case result of
    Left (_ :: SomeException) -> pure Nothing
    Right mStream -> pure mStream

-- | Register the Circuit Relay v2 stop handler
-- (/libp2p/circuit/relay/0.2.0/stop): accept inbound relayed streams and
-- hand them to the application callback.
registerRelayStopHandler
  :: Switch
  -> (PeerId -> Maybe RelayLimit -> StreamIO -> IO ())
  -> IO ()
registerRelayStopHandler sw onRelayedStream =
  setStreamHandler sw stopProtocolId $ \_conn stream -> do
    result <- handleStop stream
    case result of
      Left _ -> pure ()
      Right (sourcePeer, mLimit) -> onRelayedStream sourcePeer mLimit stream

-- | Register the DCUtR handler (/libp2p/dcutr).
--
-- Answers the CONNECT/SYNC exchange with our listen addresses and dials
-- the initiator's addresses through the Switch for the hole punch.
registerDCUtRHandler :: Switch -> IO ()
registerDCUtRHandler sw =
  setStreamHandler sw dcutrProtocolId $ \conn stream -> do
    addrs <- switchListenAddrs sw
    let config = DCUtRConfig
          { dcMaxAttempts = 3
          , dcDialer = \addr -> do
              dialed <- dial sw (connPeerId conn) [addr]
              pure $ either (Left . show) (const (Right ())) dialed
          }
    _ <- handleDCUtR config stream addrs
    pure ()
