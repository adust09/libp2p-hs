-- | Fault-injection tests (T5 tier, #178).
--
-- These tests drive the stack through failure paths that no happy-path
-- tier can reach: peers that send garbage instead of a handshake, peers
-- that disconnect mid-negotiation, and servers that accept TCP but never
-- speak libp2p. The assertions are always twofold: the failing exchange
-- terminates (no hang), and the node stays healthy afterwards.
--
-- The hostile peer is played by the raw 'Transport' interface, below the
-- upgrade pipeline, so arbitrary bytes can be written to the socket.
module LibP2P.FaultInjectionSpec (spec) where

import Control.Concurrent.Async (withAsync)
import Control.Exception (SomeException, bracket, try)
import qualified Data.ByteString as BS
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (KeyPair, publicKey)
import LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import LibP2P.Multiaddr (Multiaddr (..))
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import LibP2P.Protocol.Ping (PingResult (..), registerPingHandler, sendPing)
import LibP2P.Switch (addTransport, newSwitch, switchClose)
import LibP2P.Switch.Dial (dial)
import LibP2P.Switch.Listen (defaultConnectionGater, switchListen)
import LibP2P.Switch.Types (Switch)
import LibP2P.Transport
  ( Listener (..)
  , RawConnection (..)
  , Transport (..)
  )
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

-- | A full node (Switch + TCP + Ping) listening on loopback.
withListeningNode :: ((Switch, PeerId, Multiaddr) -> IO a) -> IO a
withListeningNode action = bracket setup teardown action
  where
    setup = do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      tcp <- newTCPTransport
      addTransport sw tcp
      registerPingHandler sw
      addrs <- switchListen sw defaultConnectionGater [loopbackAddr]
      pure (sw, pid, head addrs)
    teardown (sw, _pid, _addr) = switchClose sw

-- | A dial-only node (Switch + TCP + Ping), no listener.
withDialerNode :: (Switch -> IO a) -> IO a
withDialerNode action = bracket setup switchClose action
  where
    setup = do
      (pid, kp) <- mkTestIdentity
      sw <- newSwitch pid kp
      tcp <- newTCPTransport
      addTransport sw tcp
      registerPingHandler sw
      pure sw

-- | Prove the listener is still alive: a well-formed dial + ping succeeds.
expectHealthyListener :: PeerId -> Multiaddr -> IO ()
expectHealthyListener pid listenAddr =
  withDialerNode $ \sw -> do
    dialResult <- timeout 10000000 $ dial sw pid [listenAddr]
    case dialResult of
      Just (Right conn) -> do
        pingResult <- timeout 5000000 $ sendPing conn
        case pingResult of
          Just (Right (PingResult rtt)) -> rtt `shouldSatisfy` (> 0)
          other ->
            expectationFailure $ "ping after fault failed: " ++ show other
      Just (Left err) ->
        expectationFailure $ "dial after fault failed: " ++ show err
      Nothing ->
        expectationFailure "dial after fault timed out (listener wedged?)"

-- | Raw-connect to an address and run an action on the socket.
withRawDial :: Multiaddr -> (RawConnection -> IO a) -> IO a
withRawDial addr action = do
  tcp <- newTCPTransport
  bracket (transportDial tcp addr) rcClose action

-- | The multistream-select header bytes:
-- varint(19) "/multistream/1.0.0\n" (hand-written, not via our encoder).
mssHeaderBytes :: BS.ByteString
mssHeaderBytes = BS.singleton 0x13 <> "/multistream/1.0.0\n"

spec :: Spec
spec = do
  describe "listener under hostile input" $ do
    it "survives a peer that sends garbage instead of a handshake" $ do
      withListeningNode $ \(_sw, pid, listenAddr) -> do
        withRawDial listenAddr $ \raw -> do
          -- 0xff bytes are a hostile varint: continuation bit forever.
          streamWrite (rcStreamIO raw) (BS.replicate 1024 0xff)
        expectHealthyListener pid listenAddr

    it "survives a peer that disconnects mid multistream-select" $ do
      withListeningNode $ \(_sw, pid, listenAddr) -> do
        withRawDial listenAddr $ \raw ->
          -- Valid mss header, then the peer vanishes before proposing
          -- a security protocol.
          streamWrite (rcStreamIO raw) mssHeaderBytes
        expectHealthyListener pid listenAddr

    it "survives a peer that connects and immediately disconnects" $ do
      withListeningNode $ \(_sw, pid, listenAddr) -> do
        withRawDial listenAddr $ \_raw -> pure ()
        expectHealthyListener pid listenAddr

  describe "dialer against a hostile server" $ do
    it "dial fails cleanly (no hang) when the server sends garbage" $ do
      tcp <- newTCPTransport
      bracket (transportListen tcp loopbackAddr) listenerClose $ \listener -> do
        let serveGarbage = do
              result <- try $ bracket (listenerAccept listener) rcClose $
                \raw -> streamWrite (rcStreamIO raw) (BS.replicate 4096 0xff)
              case result of
                Left (_ :: SomeException) -> pure ()
                Right () -> pure ()
        withAsync serveGarbage $ \_server -> do
          (fakePid, _) <- mkTestIdentity
          withDialerNode $ \sw -> do
            result <- timeout 15000000 $ dial sw fakePid [listenerAddr listener]
            case result of
              Just (Left _err) -> pure ()  -- failed, and failed promptly
              Just (Right _) ->
                expectationFailure "dial must not succeed against garbage"
              Nothing ->
                expectationFailure "dial hung against a garbage-speaking server"

    it "dial fails cleanly when the server closes right after accepting" $ do
      tcp <- newTCPTransport
      bracket (transportListen tcp loopbackAddr) listenerClose $ \listener -> do
        let acceptAndSlam = do
              result <- try $ listenerAccept listener >>= rcClose
              case result of
                Left (_ :: SomeException) -> pure ()
                Right () -> pure ()
        withAsync acceptAndSlam $ \_server -> do
          (fakePid, _) <- mkTestIdentity
          withDialerNode $ \sw -> do
            result <- timeout 15000000 $ dial sw fakePid [listenerAddr listener]
            case result of
              Just (Left _err) -> pure ()
              Just (Right _) ->
                expectationFailure "dial must not succeed against a slammed socket"
              Nothing ->
                expectationFailure "dial hung against a slamming server"
