-- | Tests for the Circuit Relay v2 client transport (issue #266).
--
-- Address handling is checked in isolation; the end-to-end behaviour is
-- exercised with three real in-process switches over loopback TCP —
-- relay R, target B holding a reservation on R, and dialer A reaching B
-- through the circuit. Hole punching against real NATs is out of reach
-- in-process and is covered by the interop work (issue #131).
module LibP2P.NAT.Relay.TransportSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (KeyPair, publicKey)
import LibP2P.Crypto.PeerId (PeerId (..), fromPublicKey, peerIdBytes)
import LibP2P.Multiaddr (Multiaddr (..))
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.NAT (defaultNATConfig, registerNATHandlers)
import LibP2P.NAT.Relay (isRelayedAddr)
import LibP2P.NAT.Relay.Transport
  ( CircuitAddr (..)
  , circuitAddrOf
  , circuitTransport
  , newCircuitState
  , parseCircuitAddr
  )
import LibP2P.Protocol.Ping (openPingSession, ping, registerPingHandler)
import LibP2P.Switch (addTransport, newSwitch, switchClose)
import LibP2P.Switch.ConnPool (lookupAllConns)
import LibP2P.Switch.Dial (dial)
import LibP2P.Switch.Listen (defaultConnectionGater, switchListen, switchListenAddrs)
import LibP2P.Switch.Types (Connection (..), Direction (..), Switch (..))
import LibP2P.Transport (Transport (..))
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

-- | A syntactically valid identity-multihash peer id. Only the bytes
-- matter for address parsing, so no key generation is needed.
samplePeerId :: Word8 -> PeerId
samplePeerId n = PeerId (BS.pack ([0x12, 0x20] ++ replicate 32 n))

-- | A plain TCP address.
tcpAddr :: Multiaddr
tcpAddr = Multiaddr [IP4 0x7f000001, TCP 4001]

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

-- | Append @\/p2p\/\<relay\>\/p2p-circuit@ (and optionally the target) to
-- a transport address.
withCircuit :: Multiaddr -> PeerId -> Maybe PeerId -> Multiaddr
withCircuit (Multiaddr comps) relayId mTarget =
  Multiaddr (comps ++ [P2P (peerIdBytes relayId), P2PCircuit] ++ target)
  where
    target = maybe [] (\t -> [P2P (peerIdBytes t)]) mTarget

-- | Everything the relay tests need: the relay's identity and transport
-- address, the target's identity and circuit listen addresses, and a
-- dialer.
data Trio = Trio
  { trRelayId    :: !PeerId
  , trRelayAddr  :: !Multiaddr
  , trTargetSw   :: !Switch
  , trTargetId   :: !PeerId
  , trTargetAddrs :: ![Multiaddr]
  , trDialerSw   :: !Switch
  }

-- | Bring up a relay, a target that reserves on it, and a dialer, run the
-- action, then close all three switches.
withRelayTrio :: (Trio -> IO a) -> IO a
withRelayTrio action = do
  -- Relay R
  (pidR, kpR) <- mkTestIdentity
  swR <- newSwitch pidR kpR
  addTransport swR =<< newTCPTransport
  _ <- registerNATHandlers swR defaultNATConfig
  addrsR <- switchListen swR defaultConnectionGater [loopbackAddr]
  relayAddr <- case addrsR of
    (a : _) -> pure a
    []      -> fail "relay did not bind a listen address"
  -- Target B: reserves on R and answers pings
  (pidB, kpB) <- mkTestIdentity
  swB <- newSwitch pidB kpB
  addTransport swB =<< newTCPTransport
  _ <- registerNATHandlers swB defaultNATConfig
  registerPingHandler swB
  addrsB <- switchListen swB defaultConnectionGater
              [withCircuit relayAddr pidR Nothing]
  -- Dialer A
  (pidA, kpA) <- mkTestIdentity
  swA <- newSwitch pidA kpA
  addTransport swA =<< newTCPTransport
  _ <- registerNATHandlers swA defaultNATConfig
  threadDelay 300000
  result <- action Trio
    { trRelayId     = pidR
    , trRelayAddr   = relayAddr
    , trTargetSw    = swB
    , trTargetId    = pidB
    , trTargetAddrs = addrsB
    , trDialerSw    = swA
    }
  switchClose swA
  switchClose swB
  switchClose swR
  pure result

-- | The circuit address a dialer uses to reach the target through R.
dialAddrFor :: Trio -> PeerId -> Multiaddr
dialAddrFor trio target = withCircuit (trRelayAddr trio) (trRelayId trio) (Just target)

spec :: Spec
spec = do
  describe "parseCircuitAddr" $ do
    it "splits a dial address into relay address, relay id and target" $ do
      let relayId = samplePeerId 1
          targetId = samplePeerId 2
      case parseCircuitAddr (withCircuit tcpAddr relayId (Just targetId)) of
        Left err -> expectationFailure err
        Right ca -> do
          caRelayAddr ca `shouldBe` tcpAddr
          caRelayId ca `shouldBe` relayId
          caTarget ca `shouldBe` Just targetId

    it "reports no target for a listen address" $ do
      let relayId = samplePeerId 1
      case parseCircuitAddr (withCircuit tcpAddr relayId Nothing) of
        Left err -> expectationFailure err
        Right ca -> do
          caRelayId ca `shouldBe` relayId
          caTarget ca `shouldBe` Nothing

    it "rejects an address without a /p2p-circuit component" $
      parseCircuitAddr tcpAddr `shouldSatisfy` isLeft

    it "rejects a circuit address whose relay carries no peer id" $
      parseCircuitAddr (Multiaddr [IP4 0x7f000001, TCP 4001, P2PCircuit])
        `shouldSatisfy` isLeft

    it "rejects a relay component with no transport address" $
      parseCircuitAddr (Multiaddr [P2P (peerIdBytes (samplePeerId 1)), P2PCircuit])
        `shouldSatisfy` isLeft

    it "round-trips an address built by circuitAddrOf" $ do
      let relayId = samplePeerId 3
          targetId = samplePeerId 4
      parseCircuitAddr (circuitAddrOf tcpAddr relayId (Just targetId)) `shouldBe`
        Right CircuitAddr
          { caRelayAddr = tcpAddr
          , caRelayId = relayId
          , caTarget = Just targetId
          }

    it "drops an existing /p2p suffix when building a circuit address" $ do
      let relayId = samplePeerId 5
          suffixed = Multiaddr [IP4 0x7f000001, TCP 4001, P2P (peerIdBytes relayId)]
      circuitAddrOf suffixed relayId Nothing
        `shouldBe` withCircuit tcpAddr relayId Nothing

  describe "circuitTransport" $
    it "claims circuit addresses and declines plain TCP addresses" $ do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      st <- newCircuitState
      let transport = circuitTransport sw st
          relayId = samplePeerId 6
      transportCanDial transport (withCircuit tcpAddr relayId (Just (samplePeerId 7)))
        `shouldBe` True
      transportCanDial transport (withCircuit tcpAddr relayId Nothing) `shouldBe` True
      transportCanDial transport tcpAddr `shouldBe` False
      switchClose sw

  describe "relayed connections through a real relay" $ do
    it "advertises the reservation address as a circuit listen address" $
      withRelayTrio $ \trio -> do
        let addrsB = trTargetAddrs trio
        addrsB `shouldSatisfy` (not . null)
        all isRelayedAddr addrsB `shouldBe` True
        mapM_
          (\addr -> case parseCircuitAddr addr of
            Left err -> expectationFailure err
            Right ca -> do
              caRelayId ca `shouldBe` trRelayId trio
              caTarget ca `shouldBe` Nothing)
          addrsB
        listenAddrs <- switchListenAddrs (trTargetSw trio)
        listenAddrs `shouldBe` addrsB

    it "yields an upgraded connection to the target on both sides" $
      withRelayTrio $ \trio -> do
        let swA = trDialerSw trio
            pidB = trTargetId trio
        result <- timeout 20000000 $ dial swA pidB [dialAddrFor trio pidB]
        case result of
          Nothing -> expectationFailure "circuit dial timed out"
          Just (Left err) -> expectationFailure $ "circuit dial failed: " ++ show err
          Just (Right conn) -> do
            -- A's side: outbound, relayed, authenticated as B
            connPeerId conn `shouldBe` pidB
            connDirection conn `shouldBe` Outbound
            isRelayedAddr (connRemoteAddr conn) `shouldBe` True
            -- B's side: the same peer arrives as an inbound relayed connection
            threadDelay 500000
            conns <- atomically $
              lookupAllConns (swConnPool (trTargetSw trio)) (swLocalPeerId swA)
            let inbound = filter ((== Inbound) . connDirection) conns
            -- Connection has no Show instance, so assert on its shape
            null inbound `shouldBe` False
            all (isRelayedAddr . connRemoteAddr) inbound `shouldBe` True

    it "round-trips a protocol stream over the relayed connection" $
      withRelayTrio $ \trio -> do
        let swA = trDialerSw trio
            pidB = trTargetId trio
        result <- timeout 20000000 $ do
          conn <- dial swA pidB [dialAddrFor trio pidB] >>= either (fail . show) pure
          session <- openPingSession swA conn >>= either (fail . show) pure
          ping session
        case result of
          Nothing -> expectationFailure "ping over circuit timed out"
          Just (Left err) -> expectationFailure $ "ping over circuit failed: " ++ show err
          Just (Right _) -> pure ()

    it "fails the dial when the target holds no reservation" $
      withRelayTrio $ \trio -> do
        let unknown = samplePeerId 9
        result <- timeout 20000000 $
          try (dial (trDialerSw trio) unknown [dialAddrFor trio unknown])
        case result of
          Nothing -> expectationFailure "dial to unreserved target timed out"
          Just (Left (_ :: SomeException)) -> pure ()
          Just (Right (Left _err)) -> pure ()
          Just (Right (Right _conn)) ->
            expectationFailure "dial to unreserved target unexpectedly succeeded"
