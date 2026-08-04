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
import LibP2P.Multiaddr (Multiaddr (..), toBytes, fromBytes)
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (publicKey)
import LibP2P.Crypto.PeerId (PeerId (..), fromPublicKey, peerIdBytes)
import LibP2P.Crypto.SignedEnvelope (decodeSignedEnvelope, sePayload, sePayloadType, verifyEnvelope)

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

-- | Hop context for a request arriving over a direct (non-relayed)
-- connection, with a fresh relay identity and one public relay address.
mkHopContext :: IO HopContext
mkHopContext = do
  Right kp <- generateKeyPair
  let relayId = fromPublicKey (publicKey kp)
  pure HopContext
    { hcRelayId    = relayId
    , hcRelayKey   = kp
    , hcRelayAddrs = [Multiaddr [IP4 0xCB007105, TCP 4001, P2P (peerIdBytes relayId)]]
    , hcRemoteAddr = Multiaddr [IP4 0xC0A80102, TCP 51234]
    }

-- | The same context but with the request arriving over a relayed connection.
overRelayed :: HopContext -> HopContext
overRelayed ctx = ctx
  { hcRemoteAddr = Multiaddr
      [IP4 0xCB007105, TCP 4001, P2P (BS.pack [0x00, 0x02, 0xAA, 0xBB]), P2PCircuit]
  }

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
      ctx <- mkHopContext
      handleReserve relayState ctx serverStream testPeerId
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
      ctx <- mkHopContext
      handleReserve relayState ctx serverStream testPeerId
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

    it "should refuse reservation with RESERVATION_REFUSED when max reservations exceeded" $ do
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
      ctx <- mkHopContext
      handleReserve relayState ctx serverStream testPeerId
      result <- readHopMessage clientStream maxRelayMessageSize
      case result of
        Right resp -> do
          -- circuit-v2.md assigns RESERVATION_REFUSED (200) to a reservation
          -- rejected for capacity ("e.g. because there are too many
          -- reservations"); RESOURCE_LIMIT_EXCEEDED (201) is reserved for
          -- relayed-connection limits.
          hopStatus resp `shouldBe` Just ReservationRefused
          hopReservation resp `shouldBe` Nothing
        Left err -> expectationFailure $ "Read failed: " ++ err
      -- No reservation may be stored for the refused peer
      reservations <- readTVarIO (rsReservations relayState)
      Map.member testPeerId reservations `shouldBe` False

    it "should allow a peer to refresh its own reservation when the relay is at capacity" $ do
      let config = defaultRelayConfig { rcMaxReservations = 1 }
      relayState <- newRelayState config
      ctx <- mkHopContext
      -- First reservation fills the relay to capacity
      (clientStream1, serverStream1) <- mkStreamPair
      handleReserve relayState ctx serverStream1 testPeerId
      first <- readHopMessage clientStream1 maxRelayMessageSize
      case first of
        Right resp -> hopStatus resp `shouldBe` Just RelayOK
        Left err -> expectationFailure $ "Read failed: " ++ err
      firstExpire <- do
        reservations <- readTVarIO (rsReservations relayState)
        case Map.lookup testPeerId reservations of
          Just ar -> pure (arExpiration ar)
          Nothing -> fail "expected stored reservation"
      -- A refresh from the same peer is not a new reservation: it must
      -- succeed even though Map.size == rcMaxReservations.
      (clientStream2, serverStream2) <- mkStreamPair
      handleReserve relayState ctx serverStream2 testPeerId
      refreshed <- readHopMessage clientStream2 maxRelayMessageSize
      case refreshed of
        Right resp -> do
          hopStatus resp `shouldBe` Just RelayOK
          (hopReservation resp >>= rsvExpire) `shouldSatisfy` (/= Nothing)
        Left err -> expectationFailure $ "Read failed: " ++ err
      -- Still exactly one reservation, with a refreshed (not earlier) expiry
      reservations <- readTVarIO (rsReservations relayState)
      Map.size reservations `shouldBe` 1
      case Map.lookup testPeerId reservations of
        Just ar -> arExpiration ar `shouldSatisfy` (>= firstExpire)
        Nothing -> expectationFailure "expected stored reservation after refresh"
      -- A different peer must still be refused at capacity
      (clientStream3, serverStream3) <- mkStreamPair
      handleReserve relayState ctx serverStream3 targetPeerId
      other <- readHopMessage clientStream3 maxRelayMessageSize
      case other of
        Right resp -> hopStatus resp `shouldBe` Just ReservationRefused
        Left err -> expectationFailure $ "Read failed: " ++ err

    it "should populate the reservation with the relay's public addresses" $ do
      relayState <- newRelayState defaultRelayConfig
      ctx <- mkHopContext
      (clientStream, serverStream) <- mkStreamPair
      handleReserve relayState ctx serverStream testPeerId
      result <- readHopMessage clientStream maxRelayMessageSize
      case result of
        Right resp -> case hopReservation resp of
          Just rsv -> do
            -- circuit-v2: addrs contains the relay's public addresses
            -- (including /p2p/<relay>, without a trailing /p2p-circuit)
            rsvAddrs rsv `shouldBe` map toBytes (hcRelayAddrs ctx)
            rsvAddrs rsv `shouldSatisfy` (not . null)
          Nothing -> expectationFailure "Expected reservation in response"
        Left err -> expectationFailure $ "Read failed: " ++ err

    it "should attach a voucher that verifies as a relay-signed envelope over the reservation" $ do
      relayState <- newRelayState defaultRelayConfig
      ctx <- mkHopContext
      (clientStream, serverStream) <- mkStreamPair
      handleReserve relayState ctx serverStream testPeerId
      result <- readHopMessage clientStream maxRelayMessageSize
      voucherBytes <- case result of
        Right resp -> case hopReservation resp >>= rsvVoucher of
          Just vb -> pure vb
          Nothing -> fail "Expected voucher in reservation"
        Left err -> fail $ "Read failed: " ++ err
      -- The voucher is an RFC 0002 signed envelope under the relay-rsvp domain
      case decodeSignedEnvelope voucherBytes of
        Left err -> expectationFailure $ "Voucher envelope decode failed: " ++ err
        Right env -> do
          sePayloadType env `shouldBe` relayRsvpPayloadType
          verifyEnvelope env relayRsvpDomain `shouldBe` True
          -- The signed payload identifies relay, reserving peer and expiry
          case decodeVoucher (sePayload env) of
            Left err -> expectationFailure $ "Voucher payload decode failed: " ++ show err
            Right voucher -> do
              vRelay voucher `shouldBe` peerIdBytes (hcRelayId ctx)
              vPeer voucher `shouldBe` peerIdBytes testPeerId
              vExpiration voucher `shouldSatisfy` (> 0)

    it "should refuse a RESERVE arriving over an already-relayed connection with PERMISSION_DENIED" $ do
      relayState <- newRelayState defaultRelayConfig
      ctx <- overRelayed <$> mkHopContext
      (clientStream, serverStream) <- mkStreamPair
      handleReserve relayState ctx serverStream testPeerId
      result <- readHopMessage clientStream maxRelayMessageSize
      case result of
        Right resp -> do
          hopStatus resp `shouldBe` Just PermissionDenied
          hopReservation resp `shouldBe` Nothing
        Left err -> expectationFailure $ "Read failed: " ++ err
      -- No reservation may be stored for the refused peer
      reservations <- readTVarIO (rsReservations relayState)
      Map.member testPeerId reservations `shouldBe` False

  describe "Relay server handleConnect" $ do
    it "should refuse a CONNECT arriving over an already-relayed connection with PERMISSION_DENIED" $ do
      relayState <- newRelayState defaultRelayConfig
      -- Even with a valid target reservation, relay chains are refused
      now <- getPOSIXTime
      let valid = ActiveReservation
            { arPeerId = targetPeerId
            , arExpiration = floor now + 3600
            }
      atomically $ modifyTVar' (rsReservations relayState) (Map.insert targetPeerId valid)
      ctx <- overRelayed <$> mkHopContext
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
      handleConnect relayState ctx serverStream testPeerId connectReq (\_pid -> pure Nothing)
      result <- readHopMessage clientStream maxRelayMessageSize
      case result of
        Right resp -> hopStatus resp `shouldBe` Just PermissionDenied
        Left err -> expectationFailure $ "Read failed: " ++ err
      -- The refused CONNECT must not consume circuit slots
      counts <- readTVarIO (rsCircuitCounts relayState)
      counts `shouldBe` Map.empty

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
      ctx <- mkHopContext
      handleConnect relayState ctx serverStream testPeerId connectReq (\_pid -> pure Nothing)
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
      ctx <- mkHopContext
      handleConnect relayState ctx serverStream testPeerId connectReq (\_pid -> pure Nothing)
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
      ctx <- mkHopContext
      let runConnect = handleConnect relayState ctx serverStream testPeerId connectReq
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
      ctx <- mkHopContext
      handleConnect relayState ctx serverStream testPeerId connectReq (\_pid -> pure Nothing)
      result <- readHopMessage clientStream maxRelayMessageSize
      case result of
        Right resp -> hopStatus resp `shouldBe` Just NoReservation
        Left err -> expectationFailure $ "Read failed: " ++ err

    it "should reject a CONNECT without a peer field with MALFORMED_MESSAGE" $ do
      relayState <- newRelayState defaultRelayConfig
      (clientStream, serverStream) <- mkStreamPair
      let connectReq = HopMessage
            { hopType = Just HopConnect
            , hopPeer = Nothing
            , hopReservation = Nothing
            , hopLimit = Nothing
            , hopStatus = Nothing
            }
      writeHopMessage clientStream connectReq
      ctx <- mkHopContext
      handleConnect relayState ctx serverStream testPeerId connectReq (\_pid -> pure Nothing)
      result <- readHopMessage clientStream maxRelayMessageSize
      case result of
        Right resp -> hopStatus resp `shouldBe` Just MalformedMessage
        Left err -> expectationFailure $ "Read failed: " ++ err

    it "should respond CONNECTION_FAILED when the stop stream to the target cannot be opened" $ do
      relayState <- newRelayState defaultRelayConfig
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
      -- Target has a valid reservation, but the relay cannot reach it
      ctx <- mkHopContext
      handleConnect relayState ctx serverStream testPeerId connectReq (\_pid -> pure Nothing)
      result <- readHopMessage clientStream maxRelayMessageSize
      case result of
        Right resp -> hopStatus resp `shouldBe` Just ConnectionFailed
        Left err -> expectationFailure $ "Read failed: " ++ err
      -- The failed circuit must not leak circuit slots
      counts <- readTVarIO (rsCircuitCounts relayState)
      counts `shouldBe` Map.empty

    it "should respond CONNECTION_FAILED when the target answers the stop CONNECT with a non-OK status" $ do
      relayState <- newRelayState defaultRelayConfig
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
      ctx <- mkHopContext
      let runConnect = handleConnect relayState ctx serverStream testPeerId connectReq
                         (\_pid -> pure (Just relayStopStream))
      withAsync runConnect $ \connectAsync -> do
        -- Fake target refuses the incoming circuit
        stopReq <- readStopMessage targetStream maxRelayMessageSize
        case stopReq of
          Right m -> stopType m `shouldBe` Just StopConnect
          Left err -> expectationFailure $ "Stop read failed: " ++ err
        writeStopMessage targetStream StopMessage
          { stopType = Just StopStatus
          , stopPeer = Nothing
          , stopLimit = Nothing
          , stopStatus = Just ConnectionFailed
          }
        result <- readHopMessage clientStream maxRelayMessageSize
        case result of
          Right resp -> hopStatus resp `shouldBe` Just ConnectionFailed
          Left err -> expectationFailure $ "Read failed: " ++ err
        wait connectAsync
        counts <- readTVarIO (rsCircuitCounts relayState)
        counts `shouldBe` Map.empty

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

    it "should stop forwarding and close both streams when the data limit is exceeded" $ do
      (streamA1, streamA2) <- mkStreamPair
      (streamB1, streamB2) <- mkStreamPair
      closedA <- newIORef False
      closedB <- newIORef False
      forwardedToB <- newIORef (0 :: Int)
      let trackClose ref s = s { streamClose = modifyIORef' ref (const True) }
          countWrites s = s
            { streamWrite = \bs -> do
                modifyIORef' forwardedToB (+ BS.length bs)
                streamWrite s bs
            }
          -- Limit of 5 bytes per direction
          limitCfg = Just RelayLimit { rlDuration = Nothing, rlData = Just 5 }
      -- Queue 6 bytes (one past the limit) before starting the bridge. The
      -- bridge must cut the circuit at exactly `limit` forwarded bytes and
      -- leave byte limit+1 unconsumed (see the exact-at-limit test below).
      streamWrite streamA1 (BS.pack [1, 2, 3, 4, 5, 6])
      result <- race
        (bridgeStreams limitCfg
           (trackClose closedA streamA2)
           (countWrites (trackClose closedB streamB1)))
        (threadDelay 2000000)
      -- The bridge must terminate on its own, not via the timeout
      result `shouldBe` Left ()
      -- Exactly the limit's worth of bytes was forwarded, and no more
      readIORef forwardedToB `shouldReturn` 5
      bs <- mapM (\_ -> streamReadByte streamB2) [1..5 :: Int]
      bs `shouldBe` [1, 2, 3, 4, 5]
      -- Both sides of the circuit must be torn down
      readIORef closedA `shouldReturn` True
      readIORef closedB `shouldReturn` True

    it "should terminate the bridge once exactly the data limit has been forwarded" $ do
      (streamA1, streamA2) <- mkStreamPair
      (streamB1, streamB2) <- mkStreamPair
      closedA <- newIORef False
      closedB <- newIORef False
      let trackClose ref s = s { streamClose = modifyIORef' ref (const True) }
          limitCfg = Just RelayLimit { rlDuration = Nothing, rlData = Just 5 }
      -- Exactly the limit's worth of data: the circuit must terminate on its
      -- own without the bridge having to consume byte limit+1 first.
      streamWrite streamA1 (BS.pack [1, 2, 3, 4, 5])
      result <- race
        (bridgeStreams limitCfg
           (trackClose closedA streamA2)
           (trackClose closedB streamB1))
        (threadDelay 2000000)
      result `shouldBe` Left ()
      bs <- mapM (\_ -> streamReadByte streamB2) [1..5 :: Int]
      bs `shouldBe` [1, 2, 3, 4, 5]
      readIORef closedA `shouldReturn` True
      readIORef closedB `shouldReturn` True

    it "should count the data limit per direction, not shared across both" $ do
      (streamA1, streamA2) <- mkStreamPair
      (streamB1, streamB2) <- mkStreamPair
      let limitCfg = Just RelayLimit { rlDuration = Nothing, rlData = Just 5 }
      withAsync (bridgeStreams limitCfg streamA2 streamB1) $ \_ -> do
        -- 4 bytes in each direction: 8 total exceeds a shared 5-byte budget
        -- but stays under a correct per-direction one, so all must arrive.
        streamWrite streamA1 (BS.pack [1, 2, 3, 4])
        streamWrite streamB2 (BS.pack [5, 6, 7, 8])
        a2b <- mapM (\_ -> streamReadByte streamB2) [1..4 :: Int]
        b2a <- mapM (\_ -> streamReadByte streamA1) [1..4 :: Int]
        a2b `shouldBe` [1, 2, 3, 4]
        b2a `shouldBe` [5, 6, 7, 8]

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
    it "round-trips buildRelayAddrBytes through the multiaddr decoder" $ do
      -- /ip4/203.0.113.1/tcp/4001 in binary form
      let relayAddr = BS.pack [4, 203, 0, 113, 1, 6, 0x0F, 0xA1]
          -- Valid identity multihashes (code 0x00, length, digest)
          relayId = BS.pack [0x00, 0x03, 0xAA, 0xBB, 0xCC]
          targetId = BS.pack [0x00, 0x03, 0x11, 0x22, 0x33]
          result = buildRelayAddrBytes relayAddr relayId targetId
      -- The bytes must decode back to
      -- /ip4/203.0.113.1/tcp/4001/p2p/<relay>/p2p-circuit/p2p/<target>
      fromBytes result `shouldBe`
        Right (Multiaddr [IP4 0xCB007101, TCP 4001, P2P relayId, P2PCircuit, P2P targetId])

    it "isRelayedConnection detects P2PCircuit in address" $ do
      let relayAddr = BS.pack [4, 203, 0, 113, 1, 6, 0x0F, 0xA1]
          relayId = BS.pack [0x00, 0x03, 0xAA, 0xBB, 0xCC]
          targetId = BS.pack [0x00, 0x03, 0x11, 0x22, 0x33]
          circuitAddr = buildRelayAddrBytes relayAddr relayId targetId
          directAddr = toBytes (Multiaddr [IP4 0xCB007101, TCP 4001, P2P relayId])
      isRelayedConnection circuitAddr `shouldBe` True
      isRelayedConnection directAddr `shouldBe` False
      isRelayedConnection BS.empty `shouldBe` False

    it "should not report a relayed connection when a peer id merely contains the p2p-circuit byte pattern" $ do
      -- The varint encoding of the p2p-circuit code (290) is 0xA2 0x02. A
      -- substring scan misfires when those bytes appear inside another
      -- component, e.g. an identity-multihash peer id with digest A2 02.
      let trickyPeerId = BS.pack [0x00, 0x02, 0xA2, 0x02]
          directAddr = toBytes (Multiaddr [IP4 0xCB007101, TCP 4001, P2P trickyPeerId])
      isRelayedConnection directAddr `shouldBe` False
      -- Structural inspection must still detect a real circuit component
      let circuitAddr = toBytes (Multiaddr [IP4 0xCB007101, TCP 4001, P2P trickyPeerId, P2PCircuit])
      isRelayedConnection circuitAddr `shouldBe` True
