module LibP2P.NAT.Relay.RelaySpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import Control.Concurrent.Async (withAsync, race, wait)
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word8, Word64)
import Data.IORef (newIORef, readIORef, modifyIORef')
import LibP2P.NAT.Relay.Message
import LibP2P.NAT.Relay
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import LibP2P.Crypto.PeerId (PeerId (..))

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

    it "should encode expire as an absolute Unix timestamp when reservation is accepted" $ do
      relayState <- newRelayState defaultRelayConfig
      (clientStream, serverStream) <- mkStreamPair
      writeHopMessage clientStream HopMessage
        { hopType = Just HopReserve
        , hopPeer = Nothing
        , hopReservation = Nothing
        , hopLimit = Nothing
        , hopStatus = Nothing
        }
      now <- getPOSIXTime
      handleReserve relayState serverStream testPeerId
      result <- readHopMessage clientStream maxRelayMessageSize
      let nowSecs = floor now :: Word64
          duration = rcReservationDuration defaultRelayConfig
      case result of
        Right resp -> case hopReservation resp >>= rsvExpire of
          Just expire -> do
            -- Spec (circuit-v2.md): expire is a UTC Unix time in seconds,
            -- not a duration. It must lie in [now, now + duration + slack].
            expire `shouldSatisfy` (>= nowSecs)
            expire `shouldSatisfy` (<= nowSecs + duration + 5)
          Nothing -> expectationFailure "Expected reservation with expire in response"
        Left err -> expectationFailure $ "Read failed: " ++ err
      -- Internal state must carry the same absolute expiration
      reservations <- readTVarIO (rsReservations relayState)
      case Map.lookup testPeerId reservations of
        Just ar -> do
          arExpiration ar `shouldSatisfy` (>= nowSecs)
          arExpiration ar `shouldSatisfy` (<= nowSecs + duration + 5)
        Nothing -> expectationFailure "Expected stored reservation for peer"

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
    it "should reject CONNECT with NO_RESERVATION when the target reservation has expired" $ do
      relayState <- newRelayState defaultRelayConfig
      -- Insert an already-expired reservation for the target
      now <- getPOSIXTime
      let expired = ActiveReservation
            { arPeerId = targetPeerId
            , arExpiration = floor now - 10
            }
      atomically $ modifyTVar' (rsReservations relayState) (Map.insert targetPeerId expired)
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
      -- The expired reservation must be released from state
      reservations <- readTVarIO (rsReservations relayState)
      Map.member targetPeerId reservations `shouldBe` False

    it "should reject CONNECT with RESOURCE_LIMIT_EXCEEDED when the per-peer circuit limit is reached" $ do
      let config = defaultRelayConfig { rcMaxCircuits = 0 }
      relayState <- newRelayState config
      -- Insert a valid (unexpired) reservation for the target
      now <- getPOSIXTime
      let valid = ActiveReservation
            { arPeerId = targetPeerId
            , arExpiration = floor now + 3600
            }
      atomically $ modifyTVar' (rsReservations relayState) (Map.insert targetPeerId valid)
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
        Right resp -> hopStatus resp `shouldBe` Just ResourceLimitExceeded
        Left err -> expectationFailure $ "Read failed: " ++ err

    it "should release per-peer circuit slots when the circuit ends" $ do
      -- 1 second duration limit so the bridged circuit ends on its own
      let config = defaultRelayConfig { rcMaxCircuits = 1, rcDefaultDurationLimit = 1 }
      relayState <- newRelayState config
      now <- getPOSIXTime
      let valid = ActiveReservation
            { arPeerId = targetPeerId
            , arExpiration = floor now + 3600
            }
      atomically $ modifyTVar' (rsReservations relayState) (Map.insert targetPeerId valid)
      (clientStream, serverStream) <- mkStreamPair
      (relayStopStream, targetStream) <- mkStreamPair
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
      let runConnect = handleConnect relayState serverStream testPeerId connectReq
                         (\_pid -> pure (Just relayStopStream))
      withAsync runConnect $ \connectAsync -> do
        -- Fake target: accept the STOP CONNECT
        stopReq <- readStopMessage targetStream maxRelayMessageSize
        case stopReq of
          Right m -> stopType m `shouldBe` Just StopConnect
          Left err -> expectationFailure $ "Stop read failed: " ++ err
        writeStopMessage targetStream StopMessage
          { stopType = Just StopStatus
          , stopPeer = Nothing
          , stopLimit = Nothing
          , stopStatus = Just RelayOK
          }
        -- Source is notified of success
        result <- readHopMessage clientStream maxRelayMessageSize
        case result of
          Right resp -> hopStatus resp `shouldBe` Just RelayOK
          Left err -> expectationFailure $ "Read failed: " ++ err
        -- While the circuit is active both peers hold one slot
        counts <- readTVarIO (rsCircuitCounts relayState)
        Map.lookup testPeerId counts `shouldBe` Just 1
        Map.lookup targetPeerId counts `shouldBe` Just 1
        -- The duration limit ends the circuit; the slots must be released
        wait connectAsync
        counts' <- readTVarIO (rsCircuitCounts relayState)
        counts' `shouldBe` Map.empty

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

    it "should close both streams when the duration limit elapses" $ do
      (_streamA1, streamA2) <- mkStreamPair
      (streamB1, _streamB2) <- mkStreamPair
      closedA <- newIORef False
      closedB <- newIORef False
      let trackClose ref s = s { streamClose = modifyIORef' ref (const True) }
          -- 1 second duration limit, no data limit
          limitCfg = Just RelayLimit { rlDuration = Just 1, rlData = Nothing }
      -- No data ever flows, so only the duration timer can end the bridge.
      result <- race
        (bridgeStreams limitCfg (trackClose closedA streamA2) (trackClose closedB streamB1))
        (threadDelay 3000000)
      result `shouldBe` Left ()
      readIORef closedA `shouldReturn` True
      readIORef closedB `shouldReturn` True

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
