module LibP2P.NAT.Relay.ClientSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import Control.Concurrent.Async (withAsync, wait)
import Control.Concurrent.STM (newTQueueIO, atomically, writeTQueue, readTQueue, TQueue)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word8, Word64)
import LibP2P.NAT.Relay.Message
import LibP2P.NAT.Relay
import LibP2P.NAT.Relay.Client
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
  describe "Relay client makeReservation" $ do
    it "sends RESERVE and receives OK with reservation" $ do
      (clientStream, serverStream) <- mkStreamPair
      let serverAction = do
            req <- readHopMessage serverStream maxRelayMessageSize
            case req of
              Right msg | hopType msg == Just HopReserve -> do
                let resp = HopMessage
                      { hopType = Just HopStatus
                      , hopPeer = Nothing
                      , hopReservation = Just Reservation
                          { rsvExpire = Just 1700000000
                          , rsvAddrs = [BS.pack [4, 127, 0, 0, 1, 6, 0x10, 0x01]]
                          , rsvVoucher = Nothing
                          }
                      , hopLimit = Just RelayLimit { rlDuration = Just 120, rlData = Just 131072 }
                      , hopStatus = Just RelayOK
                      }
                writeHopMessage serverStream resp
              _ -> pure ()
      withAsync serverAction $ \_ -> do
        result <- makeReservation clientStream
        case result of
          Right resp -> do
            hopStatus resp `shouldBe` Just RelayOK
            case hopReservation resp of
              Just rsv -> rsvExpire rsv `shouldBe` Just 1700000000
              Nothing -> expectationFailure "Expected reservation"
          Left err -> expectationFailure $ "makeReservation failed: " ++ err

    it "should interpret received expire as an absolute Unix timestamp" $ do
      (clientStream, serverStream) <- mkStreamPair
      now <- getPOSIXTime
      -- Relay sends an absolute expiration one hour from now, per spec.
      let absoluteExpire = floor now + 3600 :: Word64
          serverAction = do
            _ <- readHopMessage serverStream maxRelayMessageSize
            writeHopMessage serverStream HopMessage
              { hopType = Just HopStatus
              , hopPeer = Nothing
              , hopReservation = Just Reservation
                  { rsvExpire = Just absoluteExpire
                  , rsvAddrs = []
                  , rsvVoucher = Nothing
                  }
              , hopLimit = Nothing
              , hopStatus = Just RelayOK
              }
      withAsync serverAction $ \_ -> do
        result <- makeReservation clientStream
        case result of
          Right resp -> case hopReservation resp >>= rsvExpire of
            -- The client must not rebase or offset the value: it is
            -- already a UTC Unix time in seconds.
            Just expire -> expire `shouldBe` absoluteExpire
            Nothing -> expectationFailure "Expected reservation with expire"
          Left err -> expectationFailure $ "makeReservation failed: " ++ err

    it "returns error when relay refuses reservation" $ do
      (clientStream, serverStream) <- mkStreamPair
      let serverAction = do
            _ <- readHopMessage serverStream maxRelayMessageSize
            let resp = HopMessage
                  { hopType = Just HopStatus
                  , hopPeer = Nothing
                  , hopReservation = Nothing
                  , hopLimit = Nothing
                  , hopStatus = Just ReservationRefused
                  }
            writeHopMessage serverStream resp
      withAsync serverAction $ \_ -> do
        result <- makeReservation clientStream
        case result of
          Right resp -> hopStatus resp `shouldBe` Just ReservationRefused
          Left err -> expectationFailure $ "makeReservation failed: " ++ err

  describe "Relay client connectViaRelay" $ do
    it "sends CONNECT and receives OK" $ do
      (clientStream, serverStream) <- mkStreamPair
      let serverAction = do
            req <- readHopMessage serverStream maxRelayMessageSize
            case req of
              Right msg | hopType msg == Just HopConnect -> do
                let resp = HopMessage
                      { hopType = Just HopStatus
                      , hopPeer = Nothing
                      , hopReservation = Nothing
                      , hopLimit = Just RelayLimit { rlDuration = Just 120, rlData = Just 131072 }
                      , hopStatus = Just RelayOK
                      }
                writeHopMessage serverStream resp
              _ -> pure ()
      withAsync serverAction $ \_ -> do
        result <- connectViaRelay clientStream targetPeerId
        case result of
          Right resp -> hopStatus resp `shouldBe` Just RelayOK
          Left err -> expectationFailure $ "connectViaRelay failed: " ++ err

    it "returns error when target not found" $ do
      (clientStream, serverStream) <- mkStreamPair
      let serverAction = do
            _ <- readHopMessage serverStream maxRelayMessageSize
            let resp = HopMessage
                  { hopType = Just HopStatus
                  , hopPeer = Nothing
                  , hopReservation = Nothing
                  , hopLimit = Nothing
                  , hopStatus = Just NoReservation
                  }
            writeHopMessage serverStream resp
      withAsync serverAction $ \_ -> do
        result <- connectViaRelay clientStream targetPeerId
        case result of
          Right resp -> hopStatus resp `shouldBe` Just NoReservation
          Left err -> expectationFailure $ "connectViaRelay failed: " ++ err

  describe "Relay client handleStop" $ do
    it "receives CONNECT and responds OK" $ do
      (relayStream, targetStream) <- mkStreamPair
      -- Relay sends CONNECT to target
      let stopMsg = StopMessage
            { stopType = Just StopConnect
            , stopPeer = Just RelayPeer
                { rpId = let PeerId bs = testPeerId in bs
                , rpAddrs = []
                }
            , stopLimit = Just RelayLimit { rlDuration = Just 120, rlData = Just 131072 }
            , stopStatus = Nothing
            }
      withAsync (writeStopMessage relayStream stopMsg) $ \_ -> do
        result <- handleStop targetStream
        case result of
          Right (pid, mLimit) -> do
            pid `shouldBe` testPeerId
            case mLimit of
              Just lim -> rlDuration lim `shouldBe` Just 120
              Nothing -> expectationFailure "Expected limit"
          Left err -> expectationFailure $ "handleStop failed: " ++ err
      -- Check that target sent OK response
      resp <- readStopMessage relayStream maxRelayMessageSize
      case resp of
        Right msg -> stopStatus msg `shouldBe` Just RelayOK
        Left err -> expectationFailure $ "readStopMessage failed: " ++ err

  describe "Relay client end-to-end flow against the relay server" $ do
    it "reserve → connect → stop → bridge using the real server and client code" $ do
      -- Full single-process circuit:
      --   A (reserver) reserves on the relay via makeReservation/handleReserve
      --   B (source) connects to A via connectViaRelay/handleConnect
      --   A accepts the stop CONNECT via handleStop
      --   Application data flows both ways through the bridged circuit
      relayState <- newRelayState defaultRelayConfig
      let reserver = testPeerId    -- A: the peer holding the reservation
          source = targetPeerId    -- B: the peer connecting through the relay
      -- Step 1: A reserves on the relay
      (aHop, relayAHop) <- mkStreamPair
      let relayReserveSide = do
            req <- readHopMessage relayAHop maxRelayMessageSize
            case req of
              Right msg | hopType msg == Just HopReserve ->
                handleReserve relayState relayAHop reserver
              _ -> expectationFailure "relay expected a RESERVE request"
      withAsync relayReserveSide $ \reserveAsync -> do
        rsvResult <- makeReservation aHop
        wait reserveAsync
        case rsvResult of
          Right resp -> do
            hopStatus resp `shouldBe` Just RelayOK
            (hopReservation resp >>= rsvExpire) `shouldSatisfy` (/= Nothing)
          Left err -> expectationFailure $ "makeReservation failed: " ++ err
      -- Step 2: B connects to A through the relay; A accepts via handleStop
      (bHop, relayBHop) <- mkStreamPair
      (relayStop, targetStop) <- mkStreamPair
      let relayConnectSide = do
            req <- readHopMessage relayBHop maxRelayMessageSize
            case req of
              Right msg | hopType msg == Just HopConnect ->
                handleConnect relayState relayBHop source msg
                  (\pid -> pure (if pid == reserver then Just relayStop else Nothing))
              _ -> expectationFailure "relay expected a CONNECT request"
      withAsync relayConnectSide $ \_connectAsync ->
        withAsync (handleStop targetStop) $ \stopAsync -> do
          connResult <- connectViaRelay bHop reserver
          case connResult of
            Right resp -> hopStatus resp `shouldBe` Just RelayOK
            Left err -> expectationFailure $ "connectViaRelay failed: " ++ err
          stopResult <- wait stopAsync
          case stopResult of
            Right (srcPid, mLimit) -> do
              -- The stop CONNECT must identify the connecting peer
              srcPid `shouldBe` source
              mLimit `shouldSatisfy` (/= Nothing)
            Left err -> expectationFailure $ "handleStop failed: " ++ err
          -- Step 3: application data flows through the bridged circuit
          streamWrite bHop (BS.pack [1, 2, 3])
          fwd <- mapM (\_ -> streamReadByte targetStop) [1..3 :: Int]
          fwd `shouldBe` [1, 2, 3]
          streamWrite targetStop (BS.pack [9, 8])
          back <- mapM (\_ -> streamReadByte bHop) [1..2 :: Int]
          back `shouldBe` [9, 8]
