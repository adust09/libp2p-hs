module Test.Network.LibP2P.Switch.SwitchSpec (spec) where

import Control.Concurrent.STM (atomically, readTVar)
import qualified Data.Map.Strict as Map
import Network.LibP2P.Crypto.Ed25519 (generateKeyPair)
import Network.LibP2P.Crypto.Key (KeyPair, publicKey)
import Network.LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr (..), fromText)
import Network.LibP2P.Multiaddr.Protocol (Protocol (..))
import Network.LibP2P.Switch.Switch
import Network.LibP2P.Switch.Types
import Network.LibP2P.Transport.TCP (newTCPTransport)
import Network.LibP2P.Transport.Transport (Transport (..))
import Test.Hspec

-- | Generate a PeerId and KeyPair for testing.
mkTestIdentity :: IO (PeerId, KeyPair)
mkTestIdentity = do
  Right kp <- generateKeyPair
  let pid = fromPublicKey (publicKey kp)
  pure (pid, kp)

spec :: Spec
spec = do
  describe "Switch construction" $ do
    it "newSwitch creates switch with empty state" $ do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      transports <- atomically $ readTVar (swTransports sw)
      pool <- atomically $ readTVar (swConnPool sw)
      protos <- atomically $ readTVar (swProtocols sw)
      closed <- atomically $ readTVar (swClosed sw)
      length transports `shouldBe` 0
      Map.null pool `shouldBe` True
      Map.null protos `shouldBe` True
      closed `shouldBe` False

  describe "Transport management" $ do
    it "addTransport registers a transport" $ do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      tcp <- newTCPTransport
      addTransport sw tcp
      transports <- atomically $ readTVar (swTransports sw)
      length transports `shouldBe` 1

    it "selectTransport finds matching transport by canDial" $ do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      tcp <- newTCPTransport
      addTransport sw tcp
      let Right addr = fromText "/ip4/127.0.0.1/tcp/4001"
      result <- selectTransport sw addr
      case result of
        Nothing -> expectationFailure "Expected to find TCP transport"
        Just t  -> transportCanDial t addr `shouldBe` True

    it "selectTransport returns Nothing for unsupported addr" $ do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      tcp <- newTCPTransport
      addTransport sw tcp
      -- /ip4/127.0.0.1/udp/4001 is not a TCP address
      let addr = Multiaddr [IP4 0x7f000001, UDP 4001]
      result <- selectTransport sw addr
      case result of
        Nothing -> pure ()
        Just _  -> expectationFailure "Expected Nothing for unsupported addr"

  describe "Protocol registry" $ do
    it "setStreamHandler registers a handler" $ do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      let handler _ _ = pure ()
      setStreamHandler sw "/test/1.0.0" handler
      protos <- atomically $ readTVar (swProtocols sw)
      Map.member "/test/1.0.0" protos `shouldBe` True

    it "lookupStreamHandler finds registered handler" $ do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      let handler _ _ = pure ()
      setStreamHandler sw "/test/1.0.0" handler
      result <- lookupStreamHandler sw "/test/1.0.0"
      case result of
        Nothing -> expectationFailure "Expected to find handler"
        Just _  -> pure ()  -- handler found (can't compare functions)

    it "lookupStreamHandler returns Nothing for unknown protocol" $ do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      result <- lookupStreamHandler sw "/unknown/1.0.0"
      case result of
        Nothing -> pure ()
        Just _  -> expectationFailure "Expected Nothing for unknown protocol"

    it "removeStreamHandler removes a handler" $ do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      let handler _ _ = pure ()
      setStreamHandler sw "/test/1.0.0" handler
      removeStreamHandler sw "/test/1.0.0"
      result <- lookupStreamHandler sw "/test/1.0.0"
      case result of
        Nothing -> pure ()
        Just _  -> expectationFailure "Expected handler to be removed"

    it "setStreamHandler overwrites existing handler" $ do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      let handler1 _ _ = pure ()
          handler2 _ _ = pure ()
      setStreamHandler sw "/test/1.0.0" handler1
      setStreamHandler sw "/test/1.0.0" handler2
      protos <- atomically $ readTVar (swProtocols sw)
      -- Still exactly one entry for this protocol
      Map.size protos `shouldBe` 1

  describe "Switch lifecycle" $ do
    it "switchClose sets closed state" $ do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      switchClose sw
      closed <- atomically $ readTVar (swClosed sw)
      closed `shouldBe` True
