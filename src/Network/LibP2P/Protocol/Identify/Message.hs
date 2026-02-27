-- | Identify protocol message encoding/decoding (protobuf).
--
-- Wire format from docs/07-protocols.md and specs/identify/README.md:
--   Field 1: publicKey       (bytes, optional)
--   Field 2: listenAddrs     (repeated bytes)
--   Field 3: protocols       (repeated string)
--   Field 4: observedAddr    (bytes, optional)
--   Field 5: protocolVersion (string, optional)
--   Field 6: agentVersion    (string, optional)
--
-- Uses proto3-wire for protobuf encoding/decoding. No length prefix;
-- the message boundary is determined by stream closure.
module Network.LibP2P.Protocol.Identify.Message
  ( IdentifyInfo (..)
  , encodeIdentify
  , decodeIdentify
  , maxIdentifySize
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Proto3.Wire.Decode (Parser, RawMessage, ParseError, at, optional, repeated, parse)
import qualified Proto3.Wire.Decode as Decode
import Proto3.Wire.Encode (MessageBuilder)
import qualified Proto3.Wire.Encode as Encode
import Proto3.Wire.Types (FieldNumber (..))

-- | Identify message payload.
data IdentifyInfo = IdentifyInfo
  { idProtocolVersion :: !(Maybe Text)       -- ^ e.g. "ipfs/0.1.0"
  , idAgentVersion    :: !(Maybe Text)       -- ^ e.g. "libp2p-hs/0.1.0"
  , idPublicKey       :: !(Maybe ByteString) -- ^ Serialized PublicKey protobuf
  , idListenAddrs     :: ![ByteString]       -- ^ Binary-encoded multiaddrs
  , idObservedAddr    :: !(Maybe ByteString) -- ^ Binary-encoded observed multiaddr
  , idProtocols       :: ![Text]             -- ^ Supported protocol IDs
  } deriving (Show, Eq)

-- | Maximum Identify message size: 64 KiB.
maxIdentifySize :: Int
maxIdentifySize = 64 * 1024

-- | Encode an Identify message to protobuf wire format.
encodeIdentify :: IdentifyInfo -> ByteString
encodeIdentify info = BL.toStrict $ Encode.toLazyByteString $
     optBytes 1 (idPublicKey info)
  <> repBytes 2 (idListenAddrs info)
  <> repText 3 (idProtocols info)
  <> optBytes 4 (idObservedAddr info)
  <> optText 5 (idProtocolVersion info)
  <> optText 6 (idAgentVersion info)
  where
    optBytes :: Word -> Maybe ByteString -> MessageBuilder
    optBytes _ Nothing  = mempty
    optBytes n (Just v) = Encode.byteString (FieldNumber (fromIntegral n)) v

    optText :: Word -> Maybe Text -> MessageBuilder
    optText _ Nothing  = mempty
    optText n (Just v) = Encode.text (FieldNumber (fromIntegral n)) (TL.fromStrict v)

    repBytes :: Word -> [ByteString] -> MessageBuilder
    repBytes n = foldMap (Encode.byteString (FieldNumber (fromIntegral n)))

    repText :: Word -> [Text] -> MessageBuilder
    repText n = foldMap (Encode.text (FieldNumber (fromIntegral n)) . TL.fromStrict)

-- | Decode an Identify message from protobuf wire format.
decodeIdentify :: ByteString -> Either ParseError IdentifyInfo
decodeIdentify = parse identifyParser

identifyParser :: Parser RawMessage IdentifyInfo
identifyParser = IdentifyInfo
  <$> at (optional (TL.toStrict <$> Decode.text)) (FieldNumber 5)  -- protocolVersion
  <*> at (optional (TL.toStrict <$> Decode.text)) (FieldNumber 6)  -- agentVersion
  <*> at (optional Decode.byteString)              (FieldNumber 1)  -- publicKey
  <*> at (repeated Decode.byteString)              (FieldNumber 2)  -- listenAddrs
  <*> at (optional Decode.byteString)              (FieldNumber 4)  -- observedAddr
  <*> at (repeated (TL.toStrict <$> Decode.text))  (FieldNumber 3)  -- protocols
