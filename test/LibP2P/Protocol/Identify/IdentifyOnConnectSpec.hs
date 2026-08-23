-- | Tests for running Identify on every new connection (issue #267).
--
-- specs/relay/DCUtR relies on this: "the inbound peer (here `B`) checks
-- the addresses advertised by `A` via identify". Before this, swPeerStore
-- only ever filled from an inbound push, so nothing was known about a
-- peer we dialled or accepted.
--
-- Two real switches over loopback TCP; assertions poll the peer store
-- because the notifier is dispatched asynchronously by the Switch, but
-- the direct 'identifyPeer' path is exercised synchronously.
module LibP2P.Protocol.Identify.IdentifyOnConnectSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (KeyPair, publicKey)
import LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import LibP2P.Multiaddr (Multiaddr (..))
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.Protocol.Identify
  ( identifyPeer
  , registerIdentifyHandlers
  )
import LibP2P.Protocol.Identify.Message (IdentifyInfo (..))
import LibP2P.Switch (addTransport, newSwitch, switchClose)
import LibP2P.Switch.Dial (dial)
import LibP2P.Switch.Listen (defaultConnectionGater, switchListen)
import LibP2P.Switch.Types (Connection (..), Switch (..))
import LibP2P.Transport.TCP (newTCPTransport)
import Test.Hspec

mkTestIdentity :: IO (PeerId, KeyPair)
mkTestIdentity = do
  Right kp <- generateKeyPair
  let pid = fromPublicKey (publicKey kp)
  pure (pid, kp)

loopbackAddr :: Multiaddr
loopbackAddr = Multiaddr [IP4 0x7f000001, TCP 0]

-- | A switch that listens and answers Identify.
newIdentifyingSwitch :: IO (Switch, PeerId, [Multiaddr])
newIdentifyingSwitch = do
  (pid, kp) <- mkTestIdentity
  sw <- newSwitch pid kp
  addTransport sw =<< newTCPTransport
  registerIdentifyHandlers sw
  addrs <- switchListen sw defaultConnectionGater [loopbackAddr]
  pure (sw, pid, addrs)

-- | A switch that listens but never registers the Identify handlers.
newSilentSwitch :: IO (Switch, PeerId, [Multiaddr])
newSilentSwitch = do
  (pid, kp) <- mkTestIdentity
  sw <- newSwitch pid kp
  addTransport sw =<< newTCPTransport
  addrs <- switchListen sw defaultConnectionGater [loopbackAddr]
  pure (sw, pid, addrs)

firstAddr :: [Multiaddr] -> IO Multiaddr
firstAddr (a : _) = pure a
firstAddr []      = fail "switch did not bind a listen address"

-- | Wait for the peer store to hold an entry, since the on-connect
-- notifier is dispatched asynchronously.
waitForPeerEntry :: Switch -> PeerId -> IO (Maybe IdentifyInfo)
waitForPeerEntry sw pid = go (300 :: Int)
  where
    go 0 = pure Nothing
    go k = do
      store <- readTVarIO (swPeerStore sw)
      case Map.lookup pid store of
        Just info -> pure (Just info)
        Nothing   -> threadDelay 10000 >> go (k - 1)

spec :: Spec
spec = describe "identify on connect" $ do
  it "populates the dialer's peer store with the remote's info" $ do
    (swA, _pidA, _) <- newIdentifyingSwitch
    (swB, pidB, addrsB) <- newIdentifyingSwitch
    addrB <- firstAddr addrsB
    _ <- dial swA pidB [addrB] >>= either (fail . show) pure
    entry <- waitForPeerEntry swA pidB
    case entry of
      Nothing -> expectationFailure "dialer never learned the remote's identify info"
      Just info -> do
        idListenAddrs info `shouldSatisfy` (not . null)
        idProtocolVersion info `shouldSatisfy` isJust
    switchClose swA
    switchClose swB

  it "populates the listener's peer store for an accepted connection" $ do
    (swA, pidA, _) <- newIdentifyingSwitch
    (swB, pidB, addrsB) <- newIdentifyingSwitch
    addrB <- firstAddr addrsB
    _ <- dial swA pidB [addrB] >>= either (fail . show) pure
    entry <- waitForPeerEntry swB pidA
    case entry of
      Nothing -> expectationFailure "listener never learned the remote's identify info"
      Just info -> idProtocolVersion info `shouldSatisfy` isJust
    switchClose swA
    switchClose swB

  it "reports failure and leaves the connection usable when the remote has no identify handler" $ do
    (swA, _pidA, _) <- newIdentifyingSwitch
    (swB, pidB, addrsB) <- newSilentSwitch
    addrB <- firstAddr addrsB
    conn <- dial swA pidB [addrB] >>= either (fail . show) pure
    result <- identifyPeer swA conn
    result `shouldSatisfy` either (const True) (const False)
    -- Nothing is recorded for a peer that did not answer
    store <- readTVarIO (swPeerStore swA)
    Map.member pidB store `shouldBe` False
    -- and the connection still works for another exchange
    second <- identifyPeer swA conn
    second `shouldSatisfy` either (const True) (const False)
    switchClose swA
    switchClose swB

  it "merges into an existing peer store entry rather than replacing it" $ do
    (swA, _pidA, _) <- newIdentifyingSwitch
    (swB, pidB, addrsB) <- newIdentifyingSwitch
    addrB <- firstAddr addrsB
    -- Seed an entry carrying a field the live exchange will not supply
    let seeded = IdentifyInfo
          { idProtocolVersion  = Nothing
          , idAgentVersion     = Just "seeded-agent"
          , idPublicKey        = Nothing
          , idListenAddrs      = []
          , idObservedAddr     = Nothing
          , idProtocols        = []
          , idSignedPeerRecord = Nothing
          }
    atomically $ modifyTVar' (swPeerStore swA) (Map.insert pidB seeded)
    conn <- dial swA pidB [addrB] >>= either (fail . show) pure
    identifyPeer swA conn >>= either (expectationFailure . show) pure
    store <- readTVarIO (swPeerStore swA)
    case Map.lookup pidB store of
      Nothing -> expectationFailure "peer store entry disappeared"
      Just info -> do
        -- The live exchange filled in what it knows ...
        idProtocolVersion info `shouldSatisfy` isJust
        idListenAddrs info `shouldSatisfy` (not . null)
        -- ... without erasing what was already there
        idAgentVersion info `shouldSatisfy` isJust
    switchClose swA
    switchClose swB

  it "records the peer id under which the connection was authenticated" $ do
    (swA, _pidA, _) <- newIdentifyingSwitch
    (swB, pidB, addrsB) <- newIdentifyingSwitch
    addrB <- firstAddr addrsB
    conn <- dial swA pidB [addrB] >>= either (fail . show) pure
    connPeerId conn `shouldBe` pidB
    identifyPeer swA conn >>= either (expectationFailure . show) pure
    store <- readTVarIO (swPeerStore swA)
    Map.keys store `shouldBe` [pidB]
    switchClose swA
    switchClose swB
