-- | Tests for NAT protocol handler registration on the Switch (issue #152).
--
-- Verifies that registerNATHandlers registers the AutoNAT, Circuit Relay v2
-- hop/stop, and DCUtR protocol handlers, and that an inbound stream for each
-- protocol is dispatched to the right handler over a real TCP connection.
module LibP2P.NAT.RegistrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (KeyPair, publicKey)
import LibP2P.Crypto.PeerId (PeerId (..), fromPublicKey, peerIdBytes)
import LibP2P.Multiaddr (Multiaddr (..), fromBytes, toBytes)
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.MultistreamSelect.Negotiation
  ( NegotiationResult (..)
  , ProtocolId
  , StreamIO (..)
  , negotiateInitiator
  )
import LibP2P.NAT
  ( NATConfig (..)
  , defaultNATConfig
  , registerNATHandlers
  )
import LibP2P.NAT.AutoNAT (requestAutoNAT)
import LibP2P.NAT.AutoNAT.Message
  ( ResponseStatus (..)
  , anRespStatus
  , autoNATProtocolId
  )
import LibP2P.NAT.DCUtR.Message
  ( HolePunchMessage (..)
  , HolePunchType (..)
  , dcutrProtocolId
  , maxDCUtRMessageSize
  , readHolePunchMessage
  , writeHolePunchMessage
  )
import LibP2P.NAT.Relay.Client (connectViaRelay, makeReservation)
import LibP2P.NAT.Relay.Message
  ( HopMessage (..)
  , RelayPeer (..)
  , RelayStatus (..)
  , Reservation (..)
  , StopMessage (..)
  , StopMessageType (..)
  , hopProtocolId
  , maxRelayMessageSize
  , readStopMessage
  , stopProtocolId
  , writeStopMessage
  )
import LibP2P.Protocol.Identify (buildLocalIdentify)
import LibP2P.Protocol.Identify.Message (IdentifyInfo (..))
import LibP2P.Switch (addTransport, lookupStreamHandler, newSwitch, switchClose)
import LibP2P.Switch.Connection (newStream)
import LibP2P.Switch.Dial (dial)
import LibP2P.Switch.Listen (defaultConnectionGater, switchListen)
import LibP2P.Switch.Types (Connection (..), Switch)
import LibP2P.Transport.TCP (newTCPTransport)
import System.Timeout (timeout)
import Test.Hspec

-- | Generate a test identity (PeerId, KeyPair).
mkTestIdentity :: IO (PeerId, KeyPair)
mkTestIdentity = do
  Right kp <- generateKeyPair
  let pid = fromPublicKey (publicKey kp)
  pure (pid, kp)

-- | Loopback address with port 0 (OS assigns ephemeral port).
loopbackAddr :: Multiaddr
loopbackAddr = Multiaddr [IP4 0x7f000001, TCP 0]

-- | Server B registers NAT handlers and listens; client A (also listening,
-- so AutoNAT dial-back can succeed) dials B. The action receives A's switch,
-- peer id and listen addrs, B's switch and peer id, and A's connection to B.
withNATPair
  :: NATConfig
  -> ((Switch, PeerId, [Multiaddr]) -> (Switch, PeerId) -> Connection -> IO a)
  -> IO a
withNATPair config action = do
  -- Node B: NAT server
  (pidB, kpB) <- mkTestIdentity
  swB <- newSwitch pidB kpB
  addTransport swB =<< newTCPTransport
  _relayState <- registerNATHandlers swB config
  addrsB <- switchListen swB defaultConnectionGater [loopbackAddr]
  -- Node A: client, listening so B can dial back
  (pidA, kpA) <- mkTestIdentity
  swA <- newSwitch pidA kpA
  addTransport swA =<< newTCPTransport
  addrsA <- switchListen swA defaultConnectionGater [loopbackAddr]
  dialResult <- dial swA pidB [head addrsB]
  case dialResult of
    Left err -> do
      switchClose swA
      switchClose swB
      fail $ "withNATPair: dial failed: " ++ show err
    Right conn -> do
      threadDelay 300000
      result <- action (swA, pidA, addrsA) (swB, pidB) conn
      switchClose swA
      switchClose swB
      pure result

-- | Open a stream on the connection and negotiate the given protocol.
openProtoStream :: Switch -> Connection -> ProtocolId -> IO StreamIO
openProtoStream sw conn proto = do
  streamOrErr <- newStream sw conn
  case streamOrErr of
    Left err -> fail $ "newStream failed: " ++ show err
    Right stream -> do
      result <- negotiateInitiator stream [proto]
      case result of
        Accepted _ -> pure stream
        NoProtocol -> fail $ "protocol not supported by remote: " ++ show proto

spec :: Spec
spec = do
  describe "registerNATHandlers" $ do
    it "registers AutoNAT, relay hop/stop and DCUtR protocol handlers" $ do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      _state <- registerNATHandlers sw defaultNATConfig
      mapM_
        (\proto -> do
          mHandler <- lookupStreamHandler sw proto
          (proto, isJust mHandler) `shouldBe` (proto, True))
        [autoNATProtocolId, hopProtocolId, stopProtocolId, dcutrProtocolId]

    it "registered NAT protocols appear in the local identify protocol list" $ do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      _state <- registerNATHandlers sw defaultNATConfig
      info <- buildLocalIdentify sw Nothing
      mapM_ (\proto -> idProtocols info `shouldContain` [proto])
        [autoNATProtocolId, hopProtocolId, stopProtocolId, dcutrProtocolId]

  describe "inbound stream dispatch" $ do
    it "dispatches /libp2p/autonat/1.0.0 to the AutoNAT handler (dial-back OK)" $ do
      withNATPair defaultNATConfig $ \(swA, pidA, addrsA) _nodeB conn -> do
        result <- timeout 10000000 $ do
          stream <- openProtoStream swA conn autoNATProtocolId
          requestAutoNAT stream pidA addrsA
        case result of
          Nothing -> expectationFailure "autonat request timed out"
          Just (Left err) -> expectationFailure $ "autonat request failed: " ++ err
          Just (Right resp) -> anRespStatus resp `shouldBe` Just StatusOK

    it "dispatches /libp2p/circuit/relay/0.2.0/hop to the relay hop handler" $ do
      withNATPair defaultNATConfig $ \(swA, _pidA, _addrsA) _nodeB conn -> do
        result <- timeout 10000000 $ do
          stream <- openProtoStream swA conn hopProtocolId
          makeReservation stream
        case result of
          Nothing -> expectationFailure "hop reserve timed out"
          Just (Left err) -> expectationFailure $ "hop reserve failed: " ++ err
          Just (Right resp) -> hopStatus resp `shouldBe` Just RelayOK

    it "dispatches /libp2p/circuit/relay/0.2.0/stop to the relay stop handler" $ do
      relayedMVar <- newEmptyMVar
      let config = defaultNATConfig
            { ncOnRelayedStream = \src mLimit _stream -> putMVar relayedMVar (src, mLimit) }
      withNATPair config $ \(swA, pidA, _addrsA) _nodeB conn -> do
        result <- timeout 10000000 $ do
          stream <- openProtoStream swA conn stopProtocolId
          writeStopMessage stream StopMessage
            { stopType = Just StopConnect
            , stopPeer = Just RelayPeer { rpId = peerIdBytes pidA, rpAddrs = [] }
            , stopLimit = Nothing
            , stopStatus = Nothing
            }
          readStopMessage stream maxRelayMessageSize
        case result of
          Nothing -> expectationFailure "stop connect timed out"
          Just (Left err) -> expectationFailure $ "stop connect failed: " ++ err
          Just (Right resp) -> do
            stopStatus resp `shouldBe` Just RelayOK
            callback <- timeout 5000000 $ takeMVar relayedMVar
            case callback of
              Nothing -> expectationFailure "relayed-stream callback not invoked"
              Just (src, _mLimit) -> src `shouldBe` pidA

    it "dispatches /libp2p/dcutr to the DCUtR handler" $ do
      withNATPair defaultNATConfig $ \(swA, _pidA, addrsA) _nodeB conn -> do
        result <- timeout 10000000 $ do
          stream <- openProtoStream swA conn dcutrProtocolId
          writeHolePunchMessage stream HolePunchMessage
            { hpType = HPConnect
            , hpObsAddrs = map toBytes addrsA
            }
          readHolePunchMessage stream maxDCUtRMessageSize
        case result of
          Nothing -> expectationFailure "dcutr exchange timed out"
          Just (Left err) -> expectationFailure $ "dcutr exchange failed: " ++ err
          Just (Right resp) -> do
            hpType resp `shouldBe` HPConnect
            -- The handler advertises B's listen addresses
            hpObsAddrs resp `shouldSatisfy` (not . null)

  describe "multi-host relay circuit" $ do
    it "bridges reserve → connect → application data across three in-process hosts" $ do
      -- Three real switches over TCP: relay R serves hop/stop, target A
      -- reserves on R, source B connects to A through R, and application
      -- data crosses the bridged circuit in both directions. Hole punching
      -- against real NATs is out of reach in-process and is covered by the
      -- interop work (issue #131).
      (pidR, kpR) <- mkTestIdentity
      swR <- newSwitch pidR kpR
      addTransport swR =<< newTCPTransport
      _ <- registerNATHandlers swR defaultNATConfig
      addrsR <- switchListen swR defaultConnectionGater [loopbackAddr]
      -- Target A: consumes the relayed stream (3 bytes in, 2 bytes reply)
      relayedMVar <- newEmptyMVar
      (pidA, kpA) <- mkTestIdentity
      swA <- newSwitch pidA kpA
      addTransport swA =<< newTCPTransport
      let configA = defaultNATConfig
            { ncOnRelayedStream = \src _mLimit stream -> do
                payload <- mapM (\_ -> streamReadByte stream) [1..3 :: Int]
                streamWrite stream (BS.pack [9, 8])
                putMVar relayedMVar (src, payload)
            }
      _ <- registerNATHandlers swA configA
      -- Source B
      (pidB, kpB) <- mkTestIdentity
      swB <- newSwitch pidB kpB
      addTransport swB =<< newTCPTransport
      result <- timeout 20000000 $ do
        -- A dials R and reserves
        connAR <- dial swA pidR [head addrsR] >>= either (fail . show) pure
        hopA <- openProtoStream swA connAR hopProtocolId
        rsv <- makeReservation hopA >>= either fail pure
        hopStatus rsv `shouldBe` Just RelayOK
        -- The reservation advertises R's addresses, each ending in /p2p/<R>
        case hopReservation rsv of
          Nothing -> expectationFailure "expected reservation in RESERVE response"
          Just r -> do
            rsvAddrs r `shouldSatisfy` (not . null)
            mapM_
              (\addrBytes -> case fromBytes addrBytes of
                Right (Multiaddr ps) -> last ps `shouldBe` P2P (peerIdBytes pidR)
                Left err -> expectationFailure $ "undecodable reservation addr: " ++ err)
              (rsvAddrs r)
        -- B dials R and connects to A through the circuit
        connBR <- dial swB pidR [head addrsR] >>= either (fail . show) pure
        hopB <- openProtoStream swB connBR hopProtocolId
        connResp <- connectViaRelay hopB pidA >>= either fail pure
        hopStatus connResp `shouldBe` Just RelayOK
        -- Application data B → A through the bridged circuit
        streamWrite hopB (BS.pack [1, 2, 3])
        (src, payload) <- takeMVar relayedMVar
        src `shouldBe` pidB
        payload `shouldBe` [1, 2, 3]
        -- and A → B back through the same circuit
        reply <- mapM (\_ -> streamReadByte hopB) [1..2 :: Int]
        reply `shouldBe` [9, 8]
      switchClose swA
      switchClose swB
      switchClose swR
      case result of
        Nothing -> expectationFailure "multi-host relay circuit timed out"
        Just () -> pure ()
