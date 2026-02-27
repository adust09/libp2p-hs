module Test.Network.LibP2P.NAT.Relay.MessageSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import Control.Concurrent.STM (newTQueueIO, atomically, writeTQueue, readTQueue, TQueue)
import Data.Word (Word8)
import Network.LibP2P.NAT.Relay.Message
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))

-- | Create an in-memory stream pair for testing.
mkStreamPair :: IO (StreamIO, StreamIO)
mkStreamPair = do
  q1 <- newTQueueIO :: IO (TQueue Word8)
  q2 <- newTQueueIO :: IO (TQueue Word8)
  let streamA = StreamIO
        { streamWrite = \bs -> mapM_ (\b -> atomically (writeTQueue q1 b)) (BS.unpack bs)
        , streamReadByte = atomically (readTQueue q2)
        }
      streamB = StreamIO
        { streamWrite = \bs -> mapM_ (\b -> atomically (writeTQueue q2 b)) (BS.unpack bs)
        , streamReadByte = atomically (readTQueue q1)
        }
  pure (streamA, streamB)

-- Test data

reserveMsg :: HopMessage
reserveMsg = HopMessage
  { hopType        = Just HopReserve
  , hopPeer        = Nothing
  , hopReservation = Nothing
  , hopLimit       = Nothing
  , hopStatus      = Nothing
  }

connectMsg :: HopMessage
connectMsg = HopMessage
  { hopType        = Just HopConnect
  , hopPeer        = Just RelayPeer
      { rpId    = BS.pack [0x00, 0x24, 0x08, 0x01, 0x12, 0x20, 0xAA, 0xBB]
      , rpAddrs = [BS.pack [4, 127, 0, 0, 1, 6, 0x10, 0x01]]
      }
  , hopReservation = Nothing
  , hopLimit       = Nothing
  , hopStatus      = Nothing
  }

statusOKMsg :: HopMessage
statusOKMsg = HopMessage
  { hopType        = Just HopStatus
  , hopPeer        = Nothing
  , hopReservation = Just Reservation
      { rsvExpire  = Just 1700000000
      , rsvAddrs   = [ BS.pack [4, 203, 0, 113, 1, 6, 0x0F, 0xA1] ]
      , rsvVoucher = Just (BS.pack [0xDE, 0xAD, 0xBE, 0xEF])
      }
  , hopLimit       = Just RelayLimit
      { rlDuration = Just 120
      , rlData     = Just 131072
      }
  , hopStatus      = Just RelayOK
  }

statusErrorMsg :: HopMessage
statusErrorMsg = HopMessage
  { hopType        = Just HopStatus
  , hopPeer        = Nothing
  , hopReservation = Nothing
  , hopLimit       = Nothing
  , hopStatus      = Just ReservationRefused
  }

stopConnectMsg :: StopMessage
stopConnectMsg = StopMessage
  { stopType   = Just StopConnect
  , stopPeer   = Just RelayPeer
      { rpId    = BS.pack [0x00, 0x24, 0x08, 0x01, 0x12, 0x20, 0x11, 0x22]
      , rpAddrs = []
      }
  , stopLimit  = Just RelayLimit
      { rlDuration = Just 120
      , rlData     = Just 131072
      }
  , stopStatus = Nothing
  }

stopStatusOKMsg :: StopMessage
stopStatusOKMsg = StopMessage
  { stopType   = Just StopStatus
  , stopPeer   = Nothing
  , stopLimit  = Nothing
  , stopStatus = Just RelayOK
  }

stopStatusErrorMsg :: StopMessage
stopStatusErrorMsg = StopMessage
  { stopType   = Just StopStatus
  , stopPeer   = Nothing
  , stopLimit  = Nothing
  , stopStatus = Just ConnectionFailed
  }

emptyHopMsg :: HopMessage
emptyHopMsg = HopMessage
  { hopType = Nothing, hopPeer = Nothing, hopReservation = Nothing
  , hopLimit = Nothing, hopStatus = Nothing }

emptyStopMsg :: StopMessage
emptyStopMsg = StopMessage
  { stopType = Nothing, stopPeer = Nothing, stopLimit = Nothing, stopStatus = Nothing }

spec :: Spec
spec = do
  describe "Relay HopMessage encoding/decoding" $ do
    it "encode → decode round-trip: RESERVE" $ do
      let encoded = encodeHopMessage reserveMsg
          decoded = decodeHopMessage encoded
      decoded `shouldBe` Right reserveMsg

    it "encode → decode round-trip: CONNECT with peer" $ do
      let encoded = encodeHopMessage connectMsg
          decoded = decodeHopMessage encoded
      decoded `shouldBe` Right connectMsg

    it "encode → decode round-trip: STATUS OK with reservation + limit" $ do
      let encoded = encodeHopMessage statusOKMsg
          decoded = decodeHopMessage encoded
      decoded `shouldBe` Right statusOKMsg

    it "encode → decode round-trip: STATUS error" $ do
      let encoded = encodeHopMessage statusErrorMsg
          decoded = decodeHopMessage encoded
      decoded `shouldBe` Right statusErrorMsg

    it "encode → decode round-trip: empty HopMessage" $ do
      let encoded = encodeHopMessage emptyHopMsg
          decoded = decodeHopMessage encoded
      decoded `shouldBe` Right emptyHopMsg

  describe "Relay StopMessage encoding/decoding" $ do
    it "encode → decode round-trip: CONNECT with peer + limit" $ do
      let encoded = encodeStopMessage stopConnectMsg
          decoded = decodeStopMessage encoded
      decoded `shouldBe` Right stopConnectMsg

    it "encode → decode round-trip: STATUS OK" $ do
      let encoded = encodeStopMessage stopStatusOKMsg
          decoded = decodeStopMessage encoded
      decoded `shouldBe` Right stopStatusOKMsg

    it "encode → decode round-trip: STATUS ConnectionFailed" $ do
      let encoded = encodeStopMessage stopStatusErrorMsg
          decoded = decodeStopMessage encoded
      decoded `shouldBe` Right stopStatusErrorMsg

    it "encode → decode round-trip: empty StopMessage" $ do
      let encoded = encodeStopMessage emptyStopMsg
          decoded = decodeStopMessage encoded
      decoded `shouldBe` Right emptyStopMsg

  describe "Relay RelayStatus mapping" $ do
    it "all status values round-trip through word conversion" $ do
      let statuses = [RelayOK, ReservationRefused, ResourceLimitExceeded,
                       PermissionDenied, ConnectionFailed, NoReservation,
                       MalformedMessage, UnexpectedMessage]
      mapM_ (\s -> wordToRelayStatus (relayStatusToWord s) `shouldBe` Just s) statuses

    it "unknown status returns Nothing" $ do
      wordToRelayStatus 42 `shouldBe` Nothing
      wordToRelayStatus 999 `shouldBe` Nothing

  describe "Relay Message wire framing" $ do
    it "HopMessage framed round-trip" $ do
      let framed = encodeHopFramed statusOKMsg
      case decodeHopFramed maxRelayMessageSize framed of
        Right decoded -> decoded `shouldBe` statusOKMsg
        Left err -> expectationFailure $ "Round-trip failed: " ++ err

    it "StopMessage framed round-trip" $ do
      let framed = encodeStopFramed stopConnectMsg
      case decodeStopFramed maxRelayMessageSize framed of
        Right decoded -> decoded `shouldBe` stopConnectMsg
        Left err -> expectationFailure $ "Round-trip failed: " ++ err

    it "writeHopMessage + readHopMessage over StreamIO pair" $ do
      (streamA, streamB) <- mkStreamPair
      writeHopMessage streamA statusOKMsg
      result <- readHopMessage streamB maxRelayMessageSize
      result `shouldBe` Right statusOKMsg

    it "writeStopMessage + readStopMessage over StreamIO pair" $ do
      (streamA, streamB) <- mkStreamPair
      writeStopMessage streamA stopConnectMsg
      result <- readStopMessage streamB maxRelayMessageSize
      result `shouldBe` Right stopConnectMsg

  describe "Relay nested message structures" $ do
    it "Reservation with multiple addresses" $ do
      let msg = statusOKMsg
            { hopReservation = Just Reservation
                { rsvExpire = Just 1700000000
                , rsvAddrs = [ BS.pack [4, 127, 0, 0, 1, 6, 0x10, 0x01]
                             , BS.pack [4, 10, 0, 0, 1, 6, 0x0F, 0xA1]
                             ]
                , rsvVoucher = Nothing
                }
            }
          encoded = encodeHopMessage msg
          decoded = decodeHopMessage encoded
      decoded `shouldBe` Right msg

    it "RelayPeer with multiple addresses" $ do
      let msg = connectMsg
            { hopPeer = Just RelayPeer
                { rpId = BS.pack [0xAA, 0xBB]
                , rpAddrs = [ BS.pack [4, 127, 0, 0, 1]
                            , BS.pack [4, 10, 0, 0, 1]
                            , BS.pack [4, 192, 168, 1, 1]
                            ]
                }
            }
          encoded = encodeHopMessage msg
          decoded = decodeHopMessage encoded
      decoded `shouldBe` Right msg

    it "RelayLimit with only duration" $ do
      let msg = stopConnectMsg
            { stopLimit = Just RelayLimit { rlDuration = Just 300, rlData = Nothing } }
          encoded = encodeStopMessage msg
          decoded = decodeStopMessage encoded
      decoded `shouldBe` Right msg

    it "RelayLimit with only data" $ do
      let msg = stopConnectMsg
            { stopLimit = Just RelayLimit { rlDuration = Nothing, rlData = Just 1048576 } }
          encoded = encodeStopMessage msg
          decoded = decodeStopMessage encoded
      decoded `shouldBe` Right msg
