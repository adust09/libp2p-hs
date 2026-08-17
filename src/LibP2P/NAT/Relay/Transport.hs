-- | Circuit Relay v2 client transport (specs/relay/circuit-v2).
--
-- Turns a @p2p-circuit@ multiaddr into a first-class 'Transport' so that
-- relayed peers become ordinary 'Connection's in the Switch's pool.
--
-- The spec states that once the @hop@ CONNECT (dialer side) or @stop@
-- CONNECT (target side) exchange succeeds, "the original stream becomes
-- the relayed connection", which clients then upgrade "with a security
-- protocol and a multiplexer, just like they would e.g. upgrade a TCP
-- connection". This module produces the 'RawConnection' for that stream;
-- the existing upgrade pipeline in "LibP2P.Switch.Upgrade" does the rest.
--
-- Outbound: dial the relay, negotiate @hop@, send CONNECT, hand the
-- stream to the Switch as a raw connection.
--
-- Inbound: 'transportListen' reserves on a relay and registers a queue
-- keyed by that relay's peer id. The @stop@ protocol handler calls
-- 'acceptStopStream', which enqueues the relayed stream; the Switch's
-- accept loop then drives it through the normal inbound path (gating,
-- upgrade, resource limits, pool, notifiers, teardown).
module LibP2P.NAT.Relay.Transport
  ( -- * Shared state
    CircuitState
  , newCircuitState
    -- * Transport
  , circuitTransport
    -- * Inbound relayed streams
  , acceptStopStream
    -- * Address handling (exported for testing)
  , CircuitAddr (..)
  , parseCircuitAddr
  , circuitAddrOf
  ) where

import Control.Concurrent.STM
  ( TQueue
  , TVar
  , atomically
  , newTQueue
  , newTVar
  , newTVarIO
  , readTQueue
  , readTVar
  , writeTQueue
  , writeTVar
  )
import Control.Exception (SomeException, catch, throwIO)
import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import LibP2P.Crypto.PeerId (PeerId (..), peerIdBytes)
import LibP2P.Multiaddr (Multiaddr (..), fromBytes)
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.MultistreamSelect.Negotiation
  ( NegotiationResult (..)
  , StreamIO (..)
  , negotiateInitiator
  )
import LibP2P.NAT.Relay.Client (connectViaRelay, makeReservation)
import LibP2P.NAT.Relay.Message
  ( HopMessage (..)
  , RelayStatus (..)
  , Reservation (..)
  , hopProtocolId
  )
import LibP2P.Switch.Connection (newStream)
import LibP2P.Switch.Dial (dial)
import LibP2P.Switch.Types (Connection (..), Switch (..))
import LibP2P.Transport (Listener (..), RawConnection (..), Transport (..))

-- | A parsed circuit multiaddr.
--
-- Wire form: @\<relayTransportAddr\>\/p2p\/\<relay\>\/p2p-circuit[\/p2p\/\<target\>]@.
-- The target component is present when dialling and absent when listening.
data CircuitAddr = CircuitAddr
  { caRelayAddr :: !Multiaddr      -- ^ Relay's transport address, without the @\/p2p@ suffix
  , caRelayId   :: !PeerId         -- ^ Relay's peer id
  , caTarget    :: !(Maybe PeerId) -- ^ Destination peer id, when dialling
  } deriving (Show, Eq)

-- | Per-Switch state shared between the circuit transport and the @stop@
-- protocol handler: one inbound queue per relay we hold a reservation on.
newtype CircuitState = CircuitState (TVar (Map.Map PeerId InboundQueue))

-- | An inbound queue for relayed connections arriving via one relay.
-- The closed flag lets 'listenerClose' unblock a waiting 'listenerAccept'
-- so the Switch's accept loop terminates.
data InboundQueue = InboundQueue
  { iqQueue  :: !(TQueue RawConnection)
  , iqClosed :: !(TVar Bool)
  }

-- | Create empty circuit state.
newCircuitState :: IO CircuitState
newCircuitState = CircuitState <$> newTVarIO Map.empty

-- | The Circuit Relay v2 client transport.
--
-- Captures the Switch so it can dial the relay; register it after
-- 'LibP2P.Switch.newSwitch' with 'LibP2P.Switch.addTransport'.
circuitTransport :: Switch -> CircuitState -> Transport
circuitTransport sw st = Transport
  { transportDial    = dialCircuit sw
  , transportListen  = listenCircuit sw st
  , transportCanDial = either (const False) (const True) . parseCircuitAddr
  }

-- Address handling

-- | Parse a circuit multiaddr into its relay and target parts.
parseCircuitAddr :: Multiaddr -> Either String CircuitAddr
parseCircuitAddr (Multiaddr ps) = case break (== P2PCircuit) ps of
  (_, []) -> Left "circuit address: no /p2p-circuit component"
  (before, _ : after) -> do
    (relayAddr, relayId) <- splitRelay before
    target <- parseTarget after
    pure CircuitAddr
      { caRelayAddr = relayAddr
      , caRelayId   = relayId
      , caTarget    = target
      }
  where
    splitRelay comps = case reverse comps of
      (P2P pid : rest)
        | not (null rest) -> Right (Multiaddr (reverse rest), PeerId pid)
        | otherwise -> Left "circuit address: relay has no transport address"
      _ -> Left "circuit address: relay component must end with /p2p/<relay>"
    parseTarget [] = Right Nothing
    parseTarget [P2P pid] = Right (Just (PeerId pid))
    parseTarget _ =
      Left "circuit address: expected at most /p2p/<target> after /p2p-circuit"

-- | Build the circuit multiaddr describing a relayed connection.
circuitAddrOf :: Multiaddr -> PeerId -> Maybe PeerId -> Multiaddr
circuitAddrOf relayAddr relayId mTarget =
  Multiaddr (stripP2P relayAddr ++ [P2P (peerIdBytes relayId), P2PCircuit] ++ targetPart)
  where
    targetPart = maybe [] (\t -> [P2P (peerIdBytes t)]) mTarget
    stripP2P (Multiaddr comps) = case reverse comps of
      (P2P _ : rest) -> reverse rest
      _              -> comps

-- Outbound

-- | Dial a peer through a relay.
--
-- Connects to the relay, negotiates @hop@, sends CONNECT for the target,
-- and on @STATUS OK@ returns the hop stream as the raw relayed connection.
dialCircuit :: Switch -> Multiaddr -> IO RawConnection
dialCircuit sw addr = do
  circuit <- either fail pure (parseCircuitAddr addr)
  target <- maybe (fail "circuit dial: address has no /p2p/<target>") pure
              (caTarget circuit)
  relayConn <- dialRelay sw circuit
  stream <- openHopStream sw relayConn
  resp <- connectViaRelay stream target >>= either (failClosing stream) pure
  unless (hopStatus resp == Just RelayOK) $
    failClosing stream ("relay refused CONNECT: " ++ show (hopStatus resp))
  pure RawConnection
    { rcStreamIO   = stream
    , rcLocalAddr  = connLocalAddr relayConn
    , rcRemoteAddr = circuitAddrOf (caRelayAddr circuit) (caRelayId circuit) (Just target)
    , rcClose      = closeQuietly stream
    }

-- Inbound

-- | Reserve a slot on a relay and listen for relayed connections through it.
--
-- The @hop@ stream is closed once the reservation is granted: per the
-- spec the reservation lives as long as the connection to the relay, and
-- inbound circuits arrive as fresh @stop@ streams on that connection.
listenCircuit :: Switch -> CircuitState -> Multiaddr -> IO Listener
listenCircuit sw st addr = do
  circuit <- either fail pure (parseCircuitAddr addr)
  relayConn <- dialRelay sw circuit
  stream <- openHopStream sw relayConn
  resp <- makeReservation stream >>= either (failClosing stream) pure
  unless (hopStatus resp == Just RelayOK) $
    failClosing stream ("relay refused RESERVE: " ++ show (hopStatus resp))
  closeQuietly stream
  queue <- registerQueue st (caRelayId circuit)
  pure Listener
    { listenerAccept = acceptFrom queue
    , listenerClose  = unregisterQueue st (caRelayId circuit)
    , listenerAddr   = reservationAddr circuit resp
    }

-- | Hand a relayed stream that arrived via the @stop@ protocol to the
-- listener for the relay it came over.
--
-- The stream is closed when no listener is registered for that relay:
-- without a reservation we have nothing to accept the circuit into.
acceptStopStream :: CircuitState -> Connection -> PeerId -> StreamIO -> IO ()
acceptStopStream (CircuitState var) relayConn source stream = do
  enqueued <- atomically $ do
    queues <- readTVar var
    case Map.lookup (connPeerId relayConn) queues of
      Nothing -> pure False
      Just q -> do
        closed <- readTVar (iqClosed q)
        if closed
          then pure False
          else do
            writeTQueue (iqQueue q) rawConn
            pure True
  unless enqueued (closeQuietly stream)
  where
    rawConn = RawConnection
      { rcStreamIO   = stream
      , rcLocalAddr  = connLocalAddr relayConn
      , rcRemoteAddr =
          circuitAddrOf (connRemoteAddr relayConn) (connPeerId relayConn) (Just source)
      , rcClose      = closeQuietly stream
      }

-- Helpers

-- | Dial the relay named by a circuit address, reusing a pooled
-- connection to it when one exists.
dialRelay :: Switch -> CircuitAddr -> IO Connection
dialRelay sw circuit =
  dial sw (caRelayId circuit) [caRelayAddr circuit]
    >>= either (\err -> fail ("circuit: cannot reach relay: " ++ show err)) pure

-- | Open a stream to the relay and negotiate the @hop@ protocol.
openHopStream :: Switch -> Connection -> IO StreamIO
openHopStream sw relayConn = do
  stream <- newStream sw relayConn
    >>= either (\err -> fail ("circuit: cannot open hop stream: " ++ show err)) pure
  negotiated <- negotiateInitiator stream [hopProtocolId]
  case negotiated of
    Accepted _ -> pure stream
    NoProtocol -> failClosing stream "relay does not support /libp2p/circuit/relay/0.2.0/hop"

-- | The address this listener is reachable on: the relay's advertised
-- reservation address with @\/p2p-circuit@ appended. Falls back to the
-- dialled relay address when the relay advertises none.
reservationAddr :: CircuitAddr -> HopMessage -> Multiaddr
reservationAddr circuit resp =
  case hopReservation resp >>= firstDecodable . rsvAddrs of
    Just relayAddr -> circuitAddrOf relayAddr (caRelayId circuit) Nothing
    Nothing -> circuitAddrOf (caRelayAddr circuit) (caRelayId circuit) Nothing
  where
    firstDecodable [] = Nothing
    firstDecodable (bs : rest) = either (const (firstDecodable rest)) Just (fromBytes bs)

-- | Register an inbound queue for a relay, replacing any previous one.
registerQueue :: CircuitState -> PeerId -> IO InboundQueue
registerQueue (CircuitState var) relayId = atomically $ do
  queue <- InboundQueue <$> newTQueue <*> newTVar False
  queues <- readTVar var
  writeTVar var (Map.insert relayId queue queues)
  pure queue

-- | Mark a relay's inbound queue closed and drop it, releasing any
-- 'listenerAccept' blocked on it.
unregisterQueue :: CircuitState -> PeerId -> IO ()
unregisterQueue (CircuitState var) relayId = atomically $ do
  queues <- readTVar var
  case Map.lookup relayId queues of
    Nothing -> pure ()
    Just q -> do
      writeTVar (iqClosed q) True
      writeTVar var (Map.delete relayId queues)

-- | Block for the next relayed connection, or throw once the listener is
-- closed so the Switch's accept loop stops.
acceptFrom :: InboundQueue -> IO RawConnection
acceptFrom q = do
  result <- atomically $ do
    closed <- readTVar (iqClosed q)
    if closed
      then pure Nothing
      else Just <$> readTQueue (iqQueue q)
  maybe (fail "circuit listener closed") pure result

-- | Close a stream, ignoring failures from an already-dead session.
closeQuietly :: StreamIO -> IO ()
closeQuietly stream = streamClose stream `catch` \(_ :: SomeException) -> pure ()

-- | Abort with an error, closing the stream we were using first.
failClosing :: StreamIO -> String -> IO a
failClosing stream msg = do
  closeQuietly stream
  throwIO (userError msg)
