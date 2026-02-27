-- | Circuit Relay v2 message encoding/decoding (protobuf).
--
-- Two message types:
--   HopMessage: client ↔ relay (RESERVE, CONNECT, STATUS)
--   StopMessage: relay ↔ target (CONNECT, STATUS)
--
-- Wire format: [uvarint length][protobuf message]
--
-- HopMessage fields: type(1), peer(2), reservation(3), limit(4), status(5)
-- StopMessage fields: type(1), peer(2), limit(3), status(4)
-- Peer fields: id(1), addrs(2)
-- Reservation fields: expire(1), addrs(2), voucher(3)
-- Limit fields: duration(1), data(2)
module Network.LibP2P.NAT.Relay.Message
  ( -- * Types
    HopMessageType (..)
  , StopMessageType (..)
  , RelayStatus (..)
  , RelayPeer (..)
  , Reservation (..)
  , RelayLimit (..)
  , HopMessage (..)
  , StopMessage (..)
    -- * Status conversion
  , relayStatusToWord
  , wordToRelayStatus
    -- * HopMessage encode/decode
  , encodeHopMessage
  , decodeHopMessage
  , encodeHopFramed
  , decodeHopFramed
  , writeHopMessage
  , readHopMessage
    -- * StopMessage encode/decode
  , encodeStopMessage
  , decodeStopMessage
  , encodeStopFramed
  , decodeStopFramed
  , writeStopMessage
  , readStopMessage
    -- * Constants
  , maxRelayMessageSize
  , hopProtocolId
  , stopProtocolId
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Word (Word32, Word64)
import Proto3.Wire.Decode (Parser, RawMessage, ParseError, at, one, optional, repeated, embedded, parse)
import qualified Proto3.Wire.Decode as Decode
import qualified Proto3.Wire.Encode as Encode
import Proto3.Wire.Encode (MessageBuilder)
import Proto3.Wire.Types (FieldNumber (..))
import Network.LibP2P.Core.Varint (encodeUvarint, decodeUvarint)
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))

-- | Hop protocol identifier.
hopProtocolId :: Text
hopProtocolId = "/libp2p/circuit/relay/0.2.0/hop"

-- | Stop protocol identifier.
stopProtocolId :: Text
stopProtocolId = "/libp2p/circuit/relay/0.2.0/stop"

-- | Maximum relay message size: 64 KiB.
maxRelayMessageSize :: Int
maxRelayMessageSize = 64 * 1024

-- Types

-- | HopMessage type enum.
data HopMessageType = HopReserve | HopConnect | HopStatus
  deriving (Show, Eq)

-- | StopMessage type enum.
data StopMessageType = StopConnect | StopStatus
  deriving (Show, Eq)

-- | Relay status codes (shared between Hop and Stop).
data RelayStatus
  = RelayOK                   -- ^ 100
  | ReservationRefused        -- ^ 200
  | ResourceLimitExceeded     -- ^ 201
  | PermissionDenied          -- ^ 202
  | ConnectionFailed          -- ^ 203
  | NoReservation             -- ^ 204
  | MalformedMessage          -- ^ 400
  | UnexpectedMessage         -- ^ 401
  deriving (Show, Eq)

-- | Convert RelayStatus to wire value.
relayStatusToWord :: RelayStatus -> Word32
relayStatusToWord RelayOK               = 100
relayStatusToWord ReservationRefused     = 200
relayStatusToWord ResourceLimitExceeded  = 201
relayStatusToWord PermissionDenied       = 202
relayStatusToWord ConnectionFailed       = 203
relayStatusToWord NoReservation          = 204
relayStatusToWord MalformedMessage       = 400
relayStatusToWord UnexpectedMessage      = 401

-- | Convert wire value to RelayStatus.
wordToRelayStatus :: Word32 -> Maybe RelayStatus
wordToRelayStatus 100 = Just RelayOK
wordToRelayStatus 200 = Just ReservationRefused
wordToRelayStatus 201 = Just ResourceLimitExceeded
wordToRelayStatus 202 = Just PermissionDenied
wordToRelayStatus 203 = Just ConnectionFailed
wordToRelayStatus 204 = Just NoReservation
wordToRelayStatus 400 = Just MalformedMessage
wordToRelayStatus 401 = Just UnexpectedMessage
wordToRelayStatus _   = Nothing

-- | Relay peer info (nested message).
data RelayPeer = RelayPeer
  { rpId    :: !ByteString     -- ^ field 1: peer ID bytes
  , rpAddrs :: ![ByteString]   -- ^ field 2: multiaddr bytes (repeated)
  } deriving (Show, Eq)

-- | Reservation info (nested in HopMessage).
data Reservation = Reservation
  { rsvExpire  :: !(Maybe Word64)      -- ^ field 1: expiration (Unix UTC)
  , rsvAddrs   :: ![ByteString]        -- ^ field 2: relay addresses
  , rsvVoucher :: !(Maybe ByteString)  -- ^ field 3: signed envelope bytes
  } deriving (Show, Eq)

-- | Relay limit (nested in both Hop and Stop).
data RelayLimit = RelayLimit
  { rlDuration :: !(Maybe Word32)  -- ^ field 1: max seconds (0=unlimited)
  , rlData     :: !(Maybe Word64)  -- ^ field 2: max bytes per direction
  } deriving (Show, Eq)

-- | HopMessage: client ↔ relay.
data HopMessage = HopMessage
  { hopType        :: !(Maybe HopMessageType)   -- ^ field 1
  , hopPeer        :: !(Maybe RelayPeer)         -- ^ field 2
  , hopReservation :: !(Maybe Reservation)       -- ^ field 3
  , hopLimit       :: !(Maybe RelayLimit)        -- ^ field 4
  , hopStatus      :: !(Maybe RelayStatus)       -- ^ field 5
  } deriving (Show, Eq)

-- | StopMessage: relay ↔ target.
data StopMessage = StopMessage
  { stopType   :: !(Maybe StopMessageType)  -- ^ field 1
  , stopPeer   :: !(Maybe RelayPeer)        -- ^ field 2
  , stopLimit  :: !(Maybe RelayLimit)       -- ^ field 3
  , stopStatus :: !(Maybe RelayStatus)      -- ^ field 4
  } deriving (Show, Eq)

-- Encoding helpers

optEnum :: Word -> Maybe Word32 -> MessageBuilder
optEnum _ Nothing  = mempty
optEnum n (Just v) = Encode.uint32 (FieldNumber (fromIntegral n)) v

optEmbedded :: Word -> (a -> MessageBuilder) -> Maybe a -> MessageBuilder
optEmbedded _ _ Nothing  = mempty
optEmbedded n f (Just v) = Encode.embedded (FieldNumber (fromIntegral n)) (f v)

optBytes :: Word -> Maybe ByteString -> MessageBuilder
optBytes _ Nothing  = mempty
optBytes n (Just v) = Encode.byteString (FieldNumber (fromIntegral n)) v

optUint32 :: Word -> Maybe Word32 -> MessageBuilder
optUint32 _ Nothing  = mempty
optUint32 n (Just v) = Encode.uint32 (FieldNumber (fromIntegral n)) v

optUint64 :: Word -> Maybe Word64 -> MessageBuilder
optUint64 _ Nothing  = mempty
optUint64 n (Just v) = Encode.uint64 (FieldNumber (fromIntegral n)) v

nonEmpty :: ByteString -> Maybe ByteString
nonEmpty bs
  | BS.null bs = Nothing
  | otherwise  = Just bs

-- Encoding

hopTypeToWord :: HopMessageType -> Word32
hopTypeToWord HopReserve = 0
hopTypeToWord HopConnect = 1
hopTypeToWord HopStatus  = 2

stopTypeToWord :: StopMessageType -> Word32
stopTypeToWord StopConnect = 0
stopTypeToWord StopStatus  = 1

encodeRelayPeer :: RelayPeer -> MessageBuilder
encodeRelayPeer peer =
     optBytes 1 (nonEmpty (rpId peer))
  <> foldMap (\a -> Encode.byteString (FieldNumber 2) a) (rpAddrs peer)

encodeReservation :: Reservation -> MessageBuilder
encodeReservation rsv =
     optUint64 1 (rsvExpire rsv)
  <> foldMap (\a -> Encode.byteString (FieldNumber 2) a) (rsvAddrs rsv)
  <> optBytes 3 (rsvVoucher rsv)

encodeRelayLimit :: RelayLimit -> MessageBuilder
encodeRelayLimit lim =
     optUint32 1 (rlDuration lim)
  <> optUint64 2 (rlData lim)

-- | Encode HopMessage to protobuf (no framing).
encodeHopMessage :: HopMessage -> ByteString
encodeHopMessage msg = BL.toStrict $ Encode.toLazyByteString $
     optEnum 1 (fmap hopTypeToWord (hopType msg))
  <> optEmbedded 2 encodeRelayPeer (hopPeer msg)
  <> optEmbedded 3 encodeReservation (hopReservation msg)
  <> optEmbedded 4 encodeRelayLimit (hopLimit msg)
  <> optEnum 5 (fmap relayStatusToWord (hopStatus msg))

-- | Encode StopMessage to protobuf (no framing).
encodeStopMessage :: StopMessage -> ByteString
encodeStopMessage msg = BL.toStrict $ Encode.toLazyByteString $
     optEnum 1 (fmap stopTypeToWord (stopType msg))
  <> optEmbedded 2 encodeRelayPeer (stopPeer msg)
  <> optEmbedded 3 encodeRelayLimit (stopLimit msg)
  <> optEnum 4 (fmap relayStatusToWord (stopStatus msg))

-- Decoding

-- | Decode HopMessage from protobuf.
decodeHopMessage :: ByteString -> Either ParseError HopMessage
decodeHopMessage = parse hopMessageParser

hopMessageParser :: Parser RawMessage HopMessage
hopMessageParser = HopMessage
  <$> at (fmap wordToHopType (optional Decode.uint32)) (FieldNumber 1)
  <*> at (embedded relayPeerParser) (FieldNumber 2)
  <*> at (embedded reservationParser) (FieldNumber 3)
  <*> at (embedded relayLimitParser) (FieldNumber 4)
  <*> at (fmap (>>= wordToRelayStatus) (optional Decode.uint32)) (FieldNumber 5)

wordToHopType :: Maybe Word32 -> Maybe HopMessageType
wordToHopType (Just 0) = Just HopReserve
wordToHopType (Just 1) = Just HopConnect
wordToHopType (Just 2) = Just HopStatus
wordToHopType _        = Nothing

-- | Decode StopMessage from protobuf.
decodeStopMessage :: ByteString -> Either ParseError StopMessage
decodeStopMessage = parse stopMessageParser

stopMessageParser :: Parser RawMessage StopMessage
stopMessageParser = StopMessage
  <$> at (fmap wordToStopType (optional Decode.uint32)) (FieldNumber 1)
  <*> at (embedded relayPeerParser) (FieldNumber 2)
  <*> at (embedded relayLimitParser) (FieldNumber 3)
  <*> at (fmap (>>= wordToRelayStatus) (optional Decode.uint32)) (FieldNumber 4)

wordToStopType :: Maybe Word32 -> Maybe StopMessageType
wordToStopType (Just 0) = Just StopConnect
wordToStopType (Just 1) = Just StopStatus
wordToStopType _        = Nothing

relayPeerParser :: Parser RawMessage RelayPeer
relayPeerParser = RelayPeer
  <$> at (one Decode.byteString BS.empty) (FieldNumber 1)
  <*> at (repeated Decode.byteString) (FieldNumber 2)

reservationParser :: Parser RawMessage Reservation
reservationParser = Reservation
  <$> at (optional Decode.uint64) (FieldNumber 1)
  <*> at (repeated Decode.byteString) (FieldNumber 2)
  <*> at (optional Decode.byteString) (FieldNumber 3)

relayLimitParser :: Parser RawMessage RelayLimit
relayLimitParser = RelayLimit
  <$> at (optional Decode.uint32) (FieldNumber 1)
  <*> at (optional Decode.uint64) (FieldNumber 2)

-- Wire framing (same pattern as DHT/AutoNAT)

-- | Encode HopMessage with uvarint length prefix.
encodeHopFramed :: HopMessage -> ByteString
encodeHopFramed msg = frameMessage (encodeHopMessage msg)

-- | Decode HopMessage from framed bytes.
decodeHopFramed :: Int -> ByteString -> Either String HopMessage
decodeHopFramed = decodeFramedWith decodeHopMessage "Hop"

-- | Encode StopMessage with uvarint length prefix.
encodeStopFramed :: StopMessage -> ByteString
encodeStopFramed msg = frameMessage (encodeStopMessage msg)

-- | Decode StopMessage from framed bytes.
decodeStopFramed :: Int -> ByteString -> Either String StopMessage
decodeStopFramed = decodeFramedWith decodeStopMessage "Stop"

-- | Write a framed HopMessage to a stream.
writeHopMessage :: StreamIO -> HopMessage -> IO ()
writeHopMessage stream msg = streamWrite stream (encodeHopFramed msg)

-- | Read a framed HopMessage from a stream.
readHopMessage :: StreamIO -> Int -> IO (Either String HopMessage)
readHopMessage = readFramedWith decodeHopMessage "Hop"

-- | Write a framed StopMessage to a stream.
writeStopMessage :: StreamIO -> StopMessage -> IO ()
writeStopMessage stream msg = streamWrite stream (encodeStopFramed msg)

-- | Read a framed StopMessage from a stream.
readStopMessage :: StreamIO -> Int -> IO (Either String StopMessage)
readStopMessage = readFramedWith decodeStopMessage "Stop"

-- Shared framing helpers

frameMessage :: ByteString -> ByteString
frameMessage payload =
  let lenPrefix = encodeUvarint (fromIntegral (BS.length payload))
  in lenPrefix <> payload

decodeFramedWith :: (ByteString -> Either ParseError a) -> String -> Int -> ByteString -> Either String a
decodeFramedWith decoder label maxSize bs = do
  (len, rest) <- decodeUvarint bs
  let msgLen = fromIntegral len :: Int
  if msgLen > maxSize
    then Left $ label ++ " message too large: " ++ show msgLen ++ " > " ++ show maxSize
    else if BS.length rest < msgLen
      then Left $ label ++ " message truncated"
      else case decoder (BS.take msgLen rest) of
        Left err -> Left $ label ++ " protobuf decode error: " ++ show err
        Right msg -> Right msg

readFramedWith :: (ByteString -> Either ParseError a) -> String -> StreamIO -> Int -> IO (Either String a)
readFramedWith decoder label stream maxSize = do
  varintBytes <- readVarintBytes stream
  case decodeUvarint varintBytes of
    Left err -> pure (Left $ label ++ " varint decode error: " ++ err)
    Right (len, _) -> do
      let msgLen = fromIntegral len :: Int
      if msgLen > maxSize
        then pure (Left $ label ++ " message too large: " ++ show msgLen)
        else do
          payload <- readExact stream msgLen
          case decoder payload of
            Left err -> pure (Left $ label ++ " protobuf decode error: " ++ show err)
            Right msg -> pure (Right msg)

readExact :: StreamIO -> Int -> IO ByteString
readExact stream n = BS.pack <$> mapM (const (streamReadByte stream)) [1 .. n]

readVarintBytes :: StreamIO -> IO ByteString
readVarintBytes stream = go [] (0 :: Int)
  where
    go acc n
      | n >= 10 = pure (BS.pack (reverse acc))
      | otherwise = do
          b <- streamReadByte stream
          if b < 0x80
            then pure (BS.pack (reverse (b : acc)))
            else go (b : acc) (n + 1)
