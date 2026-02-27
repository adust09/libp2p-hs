-- | AutoNAT v1 message encoding/decoding (protobuf).
--
-- Wire format from docs/10-nat-traversal.md:
--   Message framing: [uvarint length][protobuf message]
--   AutoNAT Message fields: type(1), dial(2), dialResponse(3)
--   PeerInfo fields: id(1), addrs(2)
--   Dial fields: peer(1)
--   DialResponse fields: status(1), statusText(2), addr(3)
--
-- Uses proto3-wire for protobuf encoding/decoding, same pattern as DHT.Message.
module Network.LibP2P.NAT.AutoNAT.Message
  ( -- * Types
    AutoNATMessageType (..)
  , ResponseStatus (..)
  , AutoNATPeerInfo (..)
  , AutoNATDial (..)
  , AutoNATDialResponse (..)
  , AutoNATMessage (..)
    -- * Protobuf encode/decode (no framing)
  , encodeAutoNATMessage
  , decodeAutoNATMessage
    -- * Wire framing (uvarint length prefix)
  , encodeAutoNATFramed
  , decodeAutoNATFramed
    -- * Stream I/O helpers
  , writeAutoNATMessage
  , readAutoNATMessage
    -- * Status conversion helpers
  , responseStatusToWord
  , wordToResponseStatus
    -- * Constants
  , maxAutoNATMessageSize
  , autoNATProtocolId
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Word (Word32)
import Proto3.Wire.Decode (Parser, RawMessage, ParseError, at, one, optional, repeated, embedded, parse)
import qualified Proto3.Wire.Decode as Decode
import Proto3.Wire.Encode (MessageBuilder)
import qualified Proto3.Wire.Encode as Encode
import Proto3.Wire.Types (FieldNumber (..))
import Network.LibP2P.Core.Varint (encodeUvarint, decodeUvarint)
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))

-- | AutoNAT protocol identifier.
autoNATProtocolId :: Text
autoNATProtocolId = "/libp2p/autonat/1.0.0"

-- | Maximum AutoNAT message size: 64 KiB.
maxAutoNATMessageSize :: Int
maxAutoNATMessageSize = 64 * 1024

-- | AutoNAT message type.
data AutoNATMessageType = DIAL | DIAL_RESPONSE
  deriving (Show, Eq)

-- | AutoNAT response status.
-- Wire values are non-contiguous: OK=0, E_DIAL_ERROR=100, E_DIAL_REFUSED=101,
-- E_BAD_REQUEST=200, E_INTERNAL_ERROR=300.
data ResponseStatus
  = StatusOK         -- ^ 0
  | EDialError       -- ^ 100
  | EDialRefused     -- ^ 101
  | EBadRequest      -- ^ 200
  | EInternalError   -- ^ 300
  deriving (Show, Eq)

-- | Convert ResponseStatus to wire value.
responseStatusToWord :: ResponseStatus -> Word32
responseStatusToWord StatusOK       = 0
responseStatusToWord EDialError     = 100
responseStatusToWord EDialRefused   = 101
responseStatusToWord EBadRequest    = 200
responseStatusToWord EInternalError = 300

-- | Convert wire value to ResponseStatus.
wordToResponseStatus :: Word32 -> Maybe ResponseStatus
wordToResponseStatus 0   = Just StatusOK
wordToResponseStatus 100 = Just EDialError
wordToResponseStatus 101 = Just EDialRefused
wordToResponseStatus 200 = Just EBadRequest
wordToResponseStatus 300 = Just EInternalError
wordToResponseStatus _   = Nothing

-- | AutoNAT peer info (nested message).
data AutoNATPeerInfo = AutoNATPeerInfo
  { anPeerId :: !ByteString     -- ^ field 1: peer ID bytes
  , anAddrs  :: ![ByteString]   -- ^ field 2: multiaddr bytes (repeated)
  } deriving (Show, Eq)

-- | AutoNAT Dial sub-message.
data AutoNATDial = AutoNATDial
  { anDialPeer :: !(Maybe AutoNATPeerInfo)  -- ^ field 1: peer info
  } deriving (Show, Eq)

-- | AutoNAT DialResponse sub-message.
data AutoNATDialResponse = AutoNATDialResponse
  { anRespStatus     :: !(Maybe ResponseStatus)  -- ^ field 1: status enum
  , anRespStatusText :: !(Maybe Text)             -- ^ field 2: human-readable text
  , anRespAddr       :: !(Maybe ByteString)       -- ^ field 3: successful dial-back addr
  } deriving (Show, Eq)

-- | AutoNAT top-level message.
data AutoNATMessage = AutoNATMessage
  { anMsgType         :: !(Maybe AutoNATMessageType)      -- ^ field 1
  , anMsgDial         :: !(Maybe AutoNATDial)             -- ^ field 2
  , anMsgDialResponse :: !(Maybe AutoNATDialResponse)     -- ^ field 3
  } deriving (Show, Eq)

-- Encoding

-- | Encode message type to wire value.
msgTypeToWord :: AutoNATMessageType -> Word32
msgTypeToWord DIAL          = 0
msgTypeToWord DIAL_RESPONSE = 1

-- | Encode an AutoNATMessage to protobuf wire format (no length prefix).
encodeAutoNATMessage :: AutoNATMessage -> ByteString
encodeAutoNATMessage msg = BL.toStrict $ Encode.toLazyByteString $
     optEnum 1 (anMsgType msg)
  <> optEmbedded 2 encodeAutoNATDial (anMsgDial msg)
  <> optEmbedded 3 encodeAutoNATDialResponse (anMsgDialResponse msg)
  where
    optEnum :: Word -> Maybe AutoNATMessageType -> MessageBuilder
    optEnum _ Nothing  = mempty
    optEnum n (Just t) = Encode.uint32 (FieldNumber (fromIntegral n)) (msgTypeToWord t)

    optEmbedded :: Word -> (a -> MessageBuilder) -> Maybe a -> MessageBuilder
    optEmbedded _ _ Nothing  = mempty
    optEmbedded n f (Just v) = Encode.embedded (FieldNumber (fromIntegral n)) (f v)

-- | Encode AutoNATDial sub-message.
encodeAutoNATDial :: AutoNATDial -> MessageBuilder
encodeAutoNATDial dial =
  case anDialPeer dial of
    Nothing   -> mempty
    Just peer -> Encode.embedded (FieldNumber 1) (encodeAutoNATPeerInfo peer)

-- | Encode AutoNATDialResponse sub-message.
encodeAutoNATDialResponse :: AutoNATDialResponse -> MessageBuilder
encodeAutoNATDialResponse resp =
     optStatus 1 (anRespStatus resp)
  <> optText 2 (anRespStatusText resp)
  <> optBytes 3 (anRespAddr resp)
  where
    optStatus :: Word -> Maybe ResponseStatus -> MessageBuilder
    optStatus _ Nothing  = mempty
    optStatus n (Just s) = Encode.uint32 (FieldNumber (fromIntegral n)) (responseStatusToWord s)

    optText :: Word -> Maybe Text -> MessageBuilder
    optText _ Nothing  = mempty
    optText n (Just t) = Encode.text (FieldNumber (fromIntegral n)) (TL.fromStrict t)

    optBytes :: Word -> Maybe ByteString -> MessageBuilder
    optBytes _ Nothing  = mempty
    optBytes n (Just v) = Encode.byteString (FieldNumber (fromIntegral n)) v

-- | Encode AutoNATPeerInfo sub-message.
encodeAutoNATPeerInfo :: AutoNATPeerInfo -> MessageBuilder
encodeAutoNATPeerInfo peer =
     optBytes 1 (nonEmpty (anPeerId peer))
  <> foldMap (\a -> Encode.byteString (FieldNumber 2) a) (anAddrs peer)
  where
    optBytes :: Word -> Maybe ByteString -> MessageBuilder
    optBytes _ Nothing  = mempty
    optBytes n (Just v) = Encode.byteString (FieldNumber (fromIntegral n)) v

    nonEmpty :: ByteString -> Maybe ByteString
    nonEmpty bs
      | BS.null bs = Nothing
      | otherwise  = Just bs

-- Decoding

-- | Decode an AutoNATMessage from protobuf wire format.
decodeAutoNATMessage :: ByteString -> Either ParseError AutoNATMessage
decodeAutoNATMessage = parse autoNATMessageParser

autoNATMessageParser :: Parser RawMessage AutoNATMessage
autoNATMessageParser = AutoNATMessage
  <$> at (fmap wordToMsgType (optional Decode.uint32)) (FieldNumber 1)
  <*> at (embedded autoNATDialParser) (FieldNumber 2)
  <*> at (embedded autoNATDialResponseParser) (FieldNumber 3)
  where
    wordToMsgType :: Maybe Word32 -> Maybe AutoNATMessageType
    wordToMsgType (Just 0) = Just DIAL
    wordToMsgType (Just 1) = Just DIAL_RESPONSE
    wordToMsgType _        = Nothing

-- | Parse AutoNATDial sub-message.
autoNATDialParser :: Parser RawMessage AutoNATDial
autoNATDialParser = AutoNATDial
  <$> at (embedded autoNATPeerInfoParser) (FieldNumber 1)

-- | Parse AutoNATDialResponse sub-message.
autoNATDialResponseParser :: Parser RawMessage AutoNATDialResponse
autoNATDialResponseParser = AutoNATDialResponse
  <$> at (fmap wordToStatus (optional Decode.uint32)) (FieldNumber 1)
  <*> at (fmap (fmap TL.toStrict) (optional Decode.text)) (FieldNumber 2)
  <*> at (optional Decode.byteString) (FieldNumber 3)
  where
    wordToStatus :: Maybe Word32 -> Maybe ResponseStatus
    wordToStatus (Just w) = wordToResponseStatus w
    wordToStatus Nothing  = Nothing

-- | Parse AutoNATPeerInfo sub-message.
autoNATPeerInfoParser :: Parser RawMessage AutoNATPeerInfo
autoNATPeerInfoParser = AutoNATPeerInfo
  <$> at (one Decode.byteString BS.empty) (FieldNumber 1)
  <*> at (repeated Decode.byteString) (FieldNumber 2)

-- Wire framing

-- | Encode an AutoNATMessage with uvarint length prefix.
encodeAutoNATFramed :: AutoNATMessage -> ByteString
encodeAutoNATFramed msg =
  let payload = encodeAutoNATMessage msg
      lenPrefix = encodeUvarint (fromIntegral (BS.length payload))
  in lenPrefix <> payload

-- | Decode an AutoNATMessage from uvarint-length-prefixed bytes.
decodeAutoNATFramed :: Int -> ByteString -> Either String AutoNATMessage
decodeAutoNATFramed maxSize bs = do
  (len, rest) <- decodeUvarint bs
  let msgLen = fromIntegral len :: Int
  if msgLen > maxSize
    then Left $ "AutoNAT message too large: " ++ show msgLen ++ " > " ++ show maxSize
    else if BS.length rest < msgLen
      then Left $ "AutoNAT message truncated: expected " ++ show msgLen ++ " bytes, got " ++ show (BS.length rest)
      else case decodeAutoNATMessage (BS.take msgLen rest) of
        Left err -> Left $ "AutoNAT protobuf decode error: " ++ show err
        Right msg -> Right msg

-- Stream I/O helpers

-- | Write a framed AutoNAT message to a stream.
writeAutoNATMessage :: StreamIO -> AutoNATMessage -> IO ()
writeAutoNATMessage stream msg = streamWrite stream (encodeAutoNATFramed msg)

-- | Read a framed AutoNAT message from a stream.
readAutoNATMessage :: StreamIO -> Int -> IO (Either String AutoNATMessage)
readAutoNATMessage stream maxSize = do
  varintBytes <- readVarintBytes stream
  case decodeUvarint varintBytes of
    Left err -> pure (Left $ "AutoNAT varint decode error: " ++ err)
    Right (len, _) -> do
      let msgLen = fromIntegral len :: Int
      if msgLen > maxSize
        then pure (Left $ "AutoNAT message too large: " ++ show msgLen ++ " > " ++ show maxSize)
        else do
          payload <- readExact stream msgLen
          case decodeAutoNATMessage payload of
            Left err -> pure (Left $ "AutoNAT protobuf decode error: " ++ show err)
            Right msg -> pure (Right msg)

-- | Read exactly n bytes from a stream.
readExact :: StreamIO -> Int -> IO ByteString
readExact stream n = BS.pack <$> mapM (const (streamReadByte stream)) [1 .. n]

-- | Read unsigned varint bytes from a stream (up to 10 bytes).
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
