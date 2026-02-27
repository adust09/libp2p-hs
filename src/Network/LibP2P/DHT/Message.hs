-- | DHT RPC message encoding/decoding (protobuf).
--
-- Wire format from docs/09-dht.md:
--   Message framing: [uvarint length][protobuf message]
--   Message fields: type(1), key(2), record(3), closerPeers(8), providerPeers(9)
--   Record fields: key(1), value(2), timeReceived(5)
--   Peer fields: id(1), addrs(2), connection(3)
--
-- Uses proto3-wire for protobuf encoding/decoding, same pattern as Identify.Message.
module Network.LibP2P.DHT.Message
  ( -- * Types
    MessageType (..)
  , DHTRecord (..)
  , DHTPeer (..)
  , DHTMessage (..)
    -- * Protobuf encode/decode (no framing)
  , encodeDHTMessage
  , decodeDHTMessage
    -- * Wire framing (uvarint length prefix)
  , encodeFramed
  , decodeFramed
    -- * Stream I/O helpers
  , writeFramedMessage
  , readFramedMessage
    -- * Constants
  , maxDHTMessageSize
    -- * Defaults
  , emptyDHTMessage
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Word (Word32)
import Proto3.Wire.Decode (Parser, RawMessage, ParseError, at, one, repeated, embedded, embedded', parse)
import qualified Proto3.Wire.Decode as Decode
import Proto3.Wire.Encode (MessageBuilder)
import qualified Proto3.Wire.Encode as Encode
import Proto3.Wire.Types (FieldNumber (..))
import Network.LibP2P.Core.Varint (encodeUvarint, decodeUvarint)
import Network.LibP2P.DHT.Types (ConnectionType (..))
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))

-- | Maximum DHT message size: 64 KiB.
maxDHTMessageSize :: Int
maxDHTMessageSize = 64 * 1024

-- | DHT RPC message type (protobuf enum).
data MessageType
  = PutValue      -- ^ 0
  | GetValue      -- ^ 1
  | AddProvider   -- ^ 2
  | GetProviders  -- ^ 3
  | FindNode      -- ^ 4
  deriving (Show, Eq, Enum, Bounded)

-- | DHT record (protobuf Message.Record).
data DHTRecord = DHTRecord
  { recKey          :: !ByteString   -- ^ field 1
  , recValue        :: !ByteString   -- ^ field 2
  , recTimeReceived :: !Text         -- ^ field 5, RFC 3339
  } deriving (Show, Eq)

-- | DHT peer info (protobuf Message.Peer).
data DHTPeer = DHTPeer
  { dhtPeerId       :: !ByteString       -- ^ field 1: raw Peer ID bytes
  , dhtPeerAddrs    :: ![ByteString]     -- ^ field 2: raw multiaddr bytes
  , dhtPeerConnType :: !ConnectionType   -- ^ field 3: connection capability
  } deriving (Show, Eq)

-- | DHT RPC message (protobuf Message).
data DHTMessage = DHTMessage
  { msgType          :: !MessageType       -- ^ field 1
  , msgKey           :: !ByteString        -- ^ field 2
  , msgRecord        :: !(Maybe DHTRecord) -- ^ field 3
  , msgCloserPeers   :: ![DHTPeer]         -- ^ field 8
  , msgProviderPeers :: ![DHTPeer]         -- ^ field 9
  } deriving (Show, Eq)

-- | Default empty message (FIND_NODE with empty key, no record, no peers).
emptyDHTMessage :: DHTMessage
emptyDHTMessage = DHTMessage
  { msgType          = PutValue
  , msgKey           = BS.empty
  , msgRecord        = Nothing
  , msgCloserPeers   = []
  , msgProviderPeers = []
  }

-- Encoding

-- | Encode a DHTMessage to protobuf wire format (no length prefix).
encodeDHTMessage :: DHTMessage -> ByteString
encodeDHTMessage msg = BL.toStrict $ Encode.toLazyByteString $
     Encode.uint32 (FieldNumber 1) (fromIntegral (fromEnum (msgType msg)))
  <> optBytes 2 (nonEmpty (msgKey msg))
  <> optRecord (msgRecord msg)
  <> repEmbedded 8 encodeDHTPeer (msgCloserPeers msg)
  <> repEmbedded 9 encodeDHTPeer (msgProviderPeers msg)
  where
    optBytes :: Word -> Maybe ByteString -> MessageBuilder
    optBytes _ Nothing  = mempty
    optBytes n (Just v) = Encode.byteString (FieldNumber (fromIntegral n)) v

    nonEmpty :: ByteString -> Maybe ByteString
    nonEmpty bs
      | BS.null bs = Nothing
      | otherwise  = Just bs

    optRecord :: Maybe DHTRecord -> MessageBuilder
    optRecord Nothing  = mempty
    optRecord (Just r) = Encode.embedded (FieldNumber 3) (encodeDHTRecord r)

    repEmbedded :: Word -> (a -> MessageBuilder) -> [a] -> MessageBuilder
    repEmbedded n enc = foldMap (\x -> Encode.embedded (FieldNumber (fromIntegral n)) (enc x))

-- | Encode a DHTRecord sub-message.
encodeDHTRecord :: DHTRecord -> MessageBuilder
encodeDHTRecord rec =
     optBytes 1 (nonEmpty (recKey rec))
  <> optBytes 2 (nonEmpty (recValue rec))
  <> optText 5 (nonEmpty' (recTimeReceived rec))
  where
    optBytes :: Word -> Maybe ByteString -> MessageBuilder
    optBytes _ Nothing  = mempty
    optBytes n (Just v) = Encode.byteString (FieldNumber (fromIntegral n)) v

    optText :: Word -> Maybe Text -> MessageBuilder
    optText _ Nothing  = mempty
    optText n (Just v) = Encode.text (FieldNumber (fromIntegral n)) (TL.fromStrict v)

    nonEmpty :: ByteString -> Maybe ByteString
    nonEmpty bs
      | BS.null bs = Nothing
      | otherwise  = Just bs

    nonEmpty' :: Text -> Maybe Text
    nonEmpty' t
      | t == ""   = Nothing
      | otherwise = Just t

-- | Encode a DHTPeer sub-message.
encodeDHTPeer :: DHTPeer -> MessageBuilder
encodeDHTPeer peer =
     optBytes 1 (nonEmpty (dhtPeerId peer))
  <> foldMap (\a -> Encode.byteString (FieldNumber 2) a) (dhtPeerAddrs peer)
  <> Encode.uint32 (FieldNumber 3) (fromIntegral (fromEnum (dhtPeerConnType peer)))
  where
    optBytes :: Word -> Maybe ByteString -> MessageBuilder
    optBytes _ Nothing  = mempty
    optBytes n (Just v) = Encode.byteString (FieldNumber (fromIntegral n)) v

    nonEmpty :: ByteString -> Maybe ByteString
    nonEmpty bs
      | BS.null bs = Nothing
      | otherwise  = Just bs

-- Decoding

-- | Decode a DHTMessage from protobuf wire format.
decodeDHTMessage :: ByteString -> Either ParseError DHTMessage
decodeDHTMessage = parse dhtMessageParser

dhtMessageParser :: Parser RawMessage DHTMessage
dhtMessageParser = DHTMessage
  <$> (toMessageType <$> at (one Decode.uint32 0) (FieldNumber 1))
  <*> at (one Decode.byteString BS.empty) (FieldNumber 2)
  <*> at (embedded dhtRecordParser) (FieldNumber 3)
  <*> at (repeated (embedded' dhtRecordedPeerParser)) (FieldNumber 8)
  <*> at (repeated (embedded' dhtRecordedPeerParser)) (FieldNumber 9)

-- | Parse a DHTRecord sub-message.
dhtRecordParser :: Parser RawMessage DHTRecord
dhtRecordParser = DHTRecord
  <$> at (one Decode.byteString BS.empty) (FieldNumber 1)
  <*> at (one Decode.byteString BS.empty) (FieldNumber 2)
  <*> at (one (TL.toStrict <$> Decode.text) "") (FieldNumber 5)

-- | Parse a DHTPeer sub-message.
dhtRecordedPeerParser :: Parser RawMessage DHTPeer
dhtRecordedPeerParser = DHTPeer
  <$> at (one Decode.byteString BS.empty) (FieldNumber 1)
  <*> at (repeated Decode.byteString) (FieldNumber 2)
  <*> (toConnectionType <$> at (one Decode.uint32 0) (FieldNumber 3))

-- | Convert Word32 to MessageType (clamped to valid range).
toMessageType :: Word32 -> MessageType
toMessageType n
  | n <= fromIntegral (fromEnum (maxBound :: MessageType)) = toEnum (fromIntegral n)
  | otherwise = PutValue  -- default for unknown

-- | Convert Word32 to ConnectionType (clamped to valid range).
toConnectionType :: Word32 -> ConnectionType
toConnectionType n
  | n <= fromIntegral (fromEnum (maxBound :: ConnectionType)) = toEnum (fromIntegral n)
  | otherwise = NotConnected  -- default for unknown

-- Wire framing

-- | Encode a DHTMessage with uvarint length prefix.
encodeFramed :: DHTMessage -> ByteString
encodeFramed msg =
  let payload = encodeDHTMessage msg
      lenPrefix = encodeUvarint (fromIntegral (BS.length payload))
  in lenPrefix <> payload

-- | Decode a DHTMessage from uvarint-length-prefixed bytes.
decodeFramed :: Int -> ByteString -> Either String DHTMessage
decodeFramed maxSize bs = do
  (len, rest) <- decodeUvarint bs
  let msgLen = fromIntegral len :: Int
  if msgLen > maxSize
    then Left $ "DHT message too large: " ++ show msgLen ++ " > " ++ show maxSize
    else if BS.length rest < msgLen
      then Left $ "DHT message truncated: expected " ++ show msgLen ++ " bytes, got " ++ show (BS.length rest)
      else case decodeDHTMessage (BS.take msgLen rest) of
        Left err -> Left $ "DHT protobuf decode error: " ++ show err
        Right msg -> Right msg

-- Stream I/O helpers

-- | Write a framed DHT message to a stream.
writeFramedMessage :: StreamIO -> DHTMessage -> IO ()
writeFramedMessage stream msg = streamWrite stream (encodeFramed msg)

-- | Read a framed DHT message from a stream.
-- Reads the uvarint length prefix, then the protobuf payload.
readFramedMessage :: StreamIO -> Int -> IO (Either String DHTMessage)
readFramedMessage stream maxSize = do
  -- Read varint bytes one at a time (up to 10 bytes)
  varintBytes <- readVarintBytes stream
  case decodeUvarint varintBytes of
    Left err -> pure (Left $ "DHT varint decode error: " ++ err)
    Right (len, _) -> do
      let msgLen = fromIntegral len :: Int
      if msgLen > maxSize
        then pure (Left $ "DHT message too large: " ++ show msgLen ++ " > " ++ show maxSize)
        else do
          payload <- readExact stream msgLen
          case decodeDHTMessage payload of
            Left err -> pure (Left $ "DHT protobuf decode error: " ++ show err)
            Right msg -> pure (Right msg)

-- | Read exactly n bytes from a stream.
readExact :: StreamIO -> Int -> IO ByteString
readExact stream n = BS.pack <$> mapM (const (streamReadByte stream)) [1 .. n]

-- | Read unsigned varint bytes from a stream (up to 10 bytes).
readVarintBytes :: StreamIO -> IO ByteString
readVarintBytes stream = go [] (0 :: Int)
  where
    go acc n
      | n >= 10 = pure (BS.pack (reverse acc))  -- max varint length
      | otherwise = do
          b <- streamReadByte stream
          if b < 0x80
            then pure (BS.pack (reverse (b : acc)))
            else go (b : acc) (n + 1)
