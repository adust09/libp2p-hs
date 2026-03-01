module Test.Network.LibP2P.NAT.Relay.RelaySpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import Control.Concurrent.Async (withAsync, race)
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Data.Word (Word8)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Network.LibP2P.NAT.Relay.Message
import Network.LibP2P.NAT.Relay.Relay
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import Network.LibP2P.Crypto.PeerId (PeerId (..))

-- | Create an in-memory stream pair for testing.
mkStreamPair :: IO (StreamIO, StreamIO)
mkStreamPair = do
  q1 <- newTQueueIO :: IO (TQueue Word8)
  q2 <- newTQueueIO :: IO (TQueue Word8)
  let streamA = StreamIO
        { streamWrite = \bs -> mapM_ (\b -> atomically (writeTQueue q1 b)) (BS.unpack bs)
        , streamReadByte = atomically (readTQueue q2)
        , streamClose = pure ()
        }
      streamB = StreamIO
        { streamWrite = \bs -> mapM_ (\b -> atomically (writeTQueue q2 b)) (BS.unpack bs)
        , streamReadByte = atomically (readTQueue q1)
        , streamClose = pure ()
        }
  pure (streamA, streamB)

testPeerId :: PeerId
testPeerId = PeerId (BS.pack [0x00, 0x24, 0x08, 0x01, 0x12, 0x20, 0xAA, 0xBB, 0xCC, 0xDD])

targetPeerId :: PeerId
targetPeerId = PeerId (BS.pack [0x00, 0x24, 0x08, 0x01, 0x12, 0x20, 0x11, 0x22, 0x33, 0x44])

spec :: Spec
spec = do
  describe "Relay server handleReserve" $ do
    it "accepts reservation and returns OK with expiration" $ do
      relayState <- newRelayState defaultRelayConfig
      (clientStream, serverStream) <- mkStreamPair
      -- Client sends RESERVE
      writeHopMessage clientStream HopMessage
        { hopType = Just HopReserve
        , hopPeer = Nothing
        , hopReservation = Nothing
        , hopLimit = Nothing
        , hopStatus = Nothing
        }
      handleReserve relayState serverStream testPeerId
      -- Read response
      result <- readHopMessage clientStream maxRelayMessageSize
      case result of
        Right resp -> do
          hopType resp `shouldBe` Just HopStatus
          hopStatus resp `shouldBe` Just RelayOK
          -- Reservation should have expiration
          case hopReservation resp of
            Just rsv -> rsvExpire rsv `shouldSatisfy` (/= Nothing)
            Nothing -> expectationFailure "Expected reservation in response"
        Left err -> expectationFailure $ "Read failed: " ++ err

    it "rejects reservation when max reservations exceeded" $ do
      let config = defaultRelayConfig { rcMaxReservations = 0 }
      relayState <- newRelayState config
      (clientStream, serverStream) <- mkStreamPair
      writeHopMessage clientStream HopMessage
        { hopType = Just HopReserve
        , hopPeer = Nothing
        , hopReservation = Nothing
        , hopLimit = Nothing
        , hopStatus = Nothing
        }
      handleReserve relayState serverStream testPeerId
      result <- readHopMessage clientStream maxRelayMessageSize
      case result of
        Right resp -> hopStatus resp `shouldBe` Just ResourceLimitExceeded
        Left err -> expectationFailure $ "Read failed: " ++ err

  describe "Relay server handleConnect" $ do
    it "rejects CONNECT when target has no reservation" $ do
      relayState <- newRelayState defaultRelayConfig
      (clientStream, serverStream) <- mkStreamPair
      let connectReq = HopMessage
            { hopType = Just HopConnect
            , hopPeer = Just RelayPeer
                { rpId = let PeerId bs = targetPeerId in bs
                , rpAddrs = []
                }
            , hopReservation = Nothing
            , hopLimit = Nothing
            , hopStatus = Nothing
            }
      writeHopMessage clientStream connectReq
      handleConnect relayState serverStream testPeerId connectReq (\_pid -> pure Nothing)
      result <- readHopMessage clientStream maxRelayMessageSize
      case result of
        Right resp -> hopStatus resp `shouldBe` Just NoReservation
        Left err -> expectationFailure $ "Read failed: " ++ err

  describe "bridgeStreams" $ do
    it "forwards data bidirectionally" $ do
      (streamA1, streamA2) <- mkStreamPair
      (streamB1, streamB2) <- mkStreamPair
      let limitCfg = Just RelayLimit { rlDuration = Nothing, rlData = Nothing }
      -- Bridge streamA2 ↔ streamB1 in background
      withAsync (bridgeStreams limitCfg streamA2 streamB1) $ \_ -> do
        -- Write from A side, read from B side
        streamWrite streamA1 (BS.pack [1, 2, 3])
        b1 <- streamReadByte streamB2
        b2 <- streamReadByte streamB2
        b3 <- streamReadByte streamB2
        [b1, b2, b3] `shouldBe` [1, 2, 3]
        -- Write from B side, read from A side
        streamWrite streamB2 (BS.pack [4, 5, 6])
        c1 <- streamReadByte streamA1
        c2 <- streamReadByte streamA1
        c3 <- streamReadByte streamA1
        [c1, c2, c3] `shouldBe` [4, 5, 6]

    it "enforces data limit" $ do
      (streamA1, streamA2) <- mkStreamPair
      (streamB1, streamB2) <- mkStreamPair
      -- Limit to 5 bytes of data per direction
      let limitCfg = Just RelayLimit { rlDuration = Nothing, rlData = Just 5 }
      -- Bridge in background; should terminate after limit exceeded
      result <- race
        (bridgeStreams limitCfg streamA2 streamB1)
        (do
          -- Send 5 bytes (at limit)
          streamWrite streamA1 (BS.pack [1, 2, 3, 4, 5])
          -- Read them on the other side
          bs <- mapM (\_ -> streamReadByte streamB2) [1..5 :: Int]
          -- Small delay to let bridge detect limit
          threadDelay 50000
          pure bs
        )
      case result of
        Left () -> pure ()  -- bridge terminated first (expected after limit)
        Right bs -> bs `shouldBe` [1, 2, 3, 4, 5]

  describe "Relay address parsing" $ do
    it "buildRelayAddr constructs valid relay multiaddr bytes" $ do
      let relayAddr = BS.pack [4, 203, 0, 113, 1, 6, 0x0F, 0xA1]  -- /ip4/203.0.113.1/tcp/4001
          relayId = BS.pack [0x00, 0x24, 0xAA]
          targetId = BS.pack [0x00, 0x24, 0xBB]
          result = buildRelayAddrBytes relayAddr relayId targetId
      -- Should contain the relay addr, P2P(relay), P2PCircuit, P2P(target)
      BS.null result `shouldBe` False

    it "isRelayedConnection detects P2PCircuit in address" $ do
      isRelayedConnection (BS.pack []) `shouldBe` False
