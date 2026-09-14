module LibP2P.Transport.QUICSpec (spec) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (bracket)
import Control.Monad (replicateM)
import qualified Data.ByteString as BS
import Data.Text (Text)
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (KeyPair, publicKey)
import LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import LibP2P.Multiaddr (Multiaddr (..), fromText)
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import LibP2P.Protocol.Ping (PingResult (..), registerPingHandler, sendPing)
import LibP2P.Switch (addTransport, newSwitch, switchClose)
import LibP2P.Switch.Dial (dial)
import LibP2P.Switch.Listen (defaultConnectionGater, switchListen)
import LibP2P.Switch.Types (Connection (..))
import LibP2P.Transport
  ( ConnectionEndpoint (..)
  , Listener (..)
  , NativeMuxer (..)
  , RawConnection (..)
  , Transport (..)
  )
import LibP2P.Transport.QUIC (canDialQUIC, newQUICTransport)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
  describe "canDialQUIC" $ do
    it "accepts IPv4 and IPv6 QUIC v1 multiaddrs" $ do
      canDialQUIC (parseAddr "/ip4/127.0.0.1/udp/443/quic-v1") `shouldBe` True
      canDialQUIC (parseAddr "/ip6/::1/udp/443/quic-v1") `shouldBe` True

    it "accepts a trailing peer ID and rejects non-QUIC addresses" $ do
      let peerSuffix = "/p2p/12D3KooWEtEnzvAaVkULx9kqyfRsU7Dw5Z7fuCBNhdEPCAuF1uAC"
      canDialQUIC (parseAddr ("/ip4/127.0.0.1/udp/443/quic-v1" <> peerSuffix))
        `shouldBe` True
      canDialQUIC (parseAddr "/ip4/127.0.0.1/tcp/443") `shouldBe` False
      canDialQUIC (Multiaddr [IP6 "short", UDP 443, QuicV1]) `shouldBe` False

  describe "QUIC loopback" $ do
    it "authenticates both peers and exchanges data on a native IPv4 stream" $
      withQUICPair (parseAddr "/ip4/127.0.0.1/udp/0/quic-v1") $
        \(clientPeer, serverPeer, clientRaw, serverRaw) -> do
          clientMuxer <- expectNative clientRaw
          serverMuxer <- expectNative serverRaw
          nativePeerId clientMuxer `shouldBe` serverPeer
          nativePeerId serverMuxer `shouldBe` clientPeer
          nativeSecurity clientMuxer `shouldBe` "/tls/1.0.0"
          nativeMuxerProtocol clientMuxer `shouldBe` "/quic-v1"
          exchangePayload clientMuxer serverMuxer

    it "dials and exchanges data over IPv6" $
      withQUICPair (parseAddr "/ip6/::1/udp/0/quic-v1") $
        \(_clientPeer, _serverPeer, clientRaw, serverRaw) -> do
          clientMuxer <- expectNative clientRaw
          serverMuxer <- expectNative serverRaw
          exchangePayload clientMuxer serverMuxer

  describe "Switch integration" $ do
    it "registers through addTransport and runs Ping without Noise or Yamux" $ do
      (clientPeer, clientKey) <- generateIdentity
      (serverPeer, serverKey) <- generateIdentity
      bracket (newSwitch clientPeer clientKey) switchClose $ \clientSwitch ->
        bracket (newSwitch serverPeer serverKey) switchClose $ \serverSwitch -> do
          newQUICTransport clientKey >>= addTransport clientSwitch
          newQUICTransport serverKey >>= addTransport serverSwitch
          registerPingHandler serverSwitch
          addresses <- switchListen serverSwitch defaultConnectionGater
            [parseAddr "/ip4/127.0.0.1/udp/0/quic-v1"]
          connectionResult <- timeout 10000000 $
            dial clientSwitch serverPeer addresses
          connection <- case connectionResult of
            Nothing -> fail "QUIC Switch dial timed out"
            Just (Left err) -> fail $ "QUIC Switch dial failed: " <> show err
            Just (Right established) -> pure established
          connSecurity connection `shouldBe` "/tls/1.0.0"
          connMuxer connection `shouldBe` "/quic-v1"
          pingResult <- timeout 5000000 $ sendPing clientSwitch connection
          case pingResult of
            Just (Right (PingResult rtt)) -> rtt `shouldSatisfy` (> 0)
            other -> expectationFailure $ "QUIC Ping failed: " <> show other

withQUICPair
  :: Multiaddr
  -> ((PeerId, PeerId, RawConnection, RawConnection) -> IO a)
  -> IO a
withQUICPair listenAddress action = do
  (clientPeer, clientKey) <- generateIdentity
  (serverPeer, serverKey) <- generateIdentity
  clientTransport <- newQUICTransport clientKey
  serverTransport <- newQUICTransport serverKey
  bracket
    (transportListen serverTransport listenAddress)
    listenerClose
    (\listener -> do
      connected <- timeout 10000000 $ concurrently
        (transportDial clientTransport (listenerAddr listener))
        (listenerAccept listener)
      pair <- maybe (fail "QUIC loopback connection timed out") pure connected
      bracket
        (pure pair)
        (\(clientRaw, serverRaw) -> rcClose clientRaw >> rcClose serverRaw)
        (\(clientRaw, serverRaw) ->
          action (clientPeer, serverPeer, clientRaw, serverRaw)))

exchangePayload :: NativeMuxer -> NativeMuxer -> IO ()
exchangePayload clientMuxer serverMuxer = do
  clientStream <- nativeOpenStream clientMuxer
  streamWrite clientStream "hello over QUIC"
  serverStream <- nativeAcceptStream serverMuxer
  request <- readExactly serverStream 15
  request `shouldBe` "hello over QUIC"
  streamWrite serverStream "native streams"
  response <- readExactly clientStream 14
  response `shouldBe` "native streams"
  streamClose clientStream
  streamClose serverStream

expectNative :: RawConnection -> IO NativeMuxer
expectNative raw = case rcEndpoint raw of
  NativeMuxerEndpoint native -> pure native
  ByteStreamEndpoint _ -> fail "expected a native QUIC endpoint"

readExactly :: StreamIO -> Int -> IO BS.ByteString
readExactly stream size = BS.pack <$> replicateM size (streamReadByte stream)

generateIdentity :: IO (PeerId, KeyPair)
generateIdentity = do
  result <- generateKeyPair
  keyPair <- either fail pure result
  pure (fromPublicKey (publicKey keyPair), keyPair)

parseAddr :: Text -> Multiaddr
parseAddr text = case fromText text of
  Left err -> error $ "invalid test multiaddr: " <> show err
  Right address -> address
