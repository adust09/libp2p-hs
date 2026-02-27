-- | DCUtR (Direct Connection Upgrade through Relay) message encoding/decoding.
--
-- Protocol: /libp2p/dcutr
-- Wire format: varint-length-prefixed protobuf, max 4 KiB
--
-- HolePunch message:
--   field 1: type (required) - CONNECT(100) or SYNC(300)
--   field 2: ObsAddrs (repeated bytes) - observed multiaddr binary
module Network.LibP2P.NAT.DCUtR.Message
  ( -- * Types
    HolePunchType (..)
  , HolePunchMessage (..)
    -- * Type conversion
  , holePunchTypeToWord
  , wordToHolePunchType
    -- * Protobuf encode/decode (no framing)
  , encodeHolePunchMessage
  , decodeHolePunchMessage
    -- * Wire framing (uvarint length prefix)
  , encodeHolePunchFramed
  , decodeHolePunchFramed
    -- * Stream I/O helpers
  , writeHolePunchMessage
  , readHolePunchMessage
    -- * Constants
  , maxDCUtRMessageSize
  , dcutrProtocolId
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Word (Word32)
import Proto3.Wire.Decode (Parser, RawMessage, ParseError, at, one, repeated, parse)
import qualified Proto3.Wire.Decode as Decode
import qualified Proto3.Wire.Encode as Encode
import Proto3.Wire.Types (FieldNumber (..))
import Network.LibP2P.Core.Varint (encodeUvarint, decodeUvarint)
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))

-- | DCUtR protocol identifier.
dcutrProtocolId :: Text
dcutrProtocolId = "/libp2p/dcutr"

-- | Maximum DCUtR message size: 4 KiB (per spec).
maxDCUtRMessageSize :: Int
maxDCUtRMessageSize = 4096

-- | HolePunch message type.
data HolePunchType = HPConnect | HPSync
  deriving (Show, Eq)

-- | HolePunch message.
data HolePunchMessage = HolePunchMessage
  { hpType     :: !HolePunchType    -- ^ field 1 (required)
  , hpObsAddrs :: ![ByteString]     -- ^ field 2 (repeated, binary multiaddrs)
  } deriving (Show, Eq)

-- | Convert HolePunchType to wire value.
holePunchTypeToWord :: HolePunchType -> Word32
holePunchTypeToWord HPConnect = 100
holePunchTypeToWord HPSync    = 300

-- | Convert wire value to HolePunchType.
wordToHolePunchType :: Word32 -> Maybe HolePunchType
wordToHolePunchType 100 = Just HPConnect
wordToHolePunchType 300 = Just HPSync
wordToHolePunchType _   = Nothing

-- Encoding

-- | Encode HolePunchMessage to protobuf (no framing).
encodeHolePunchMessage :: HolePunchMessage -> ByteString
encodeHolePunchMessage msg = BL.toStrict $ Encode.toLazyByteString $
     Encode.uint32 (FieldNumber 1) (holePunchTypeToWord (hpType msg))
  <> foldMap (\a -> Encode.byteString (FieldNumber 2) a) (hpObsAddrs msg)

-- Decoding

-- | Decode HolePunchMessage from protobuf.
decodeHolePunchMessage :: ByteString -> Either ParseError HolePunchMessage
decodeHolePunchMessage = parse holePunchParser

holePunchParser :: Parser RawMessage HolePunchMessage
holePunchParser = HolePunchMessage
  <$> (toHPType <$> at (one Decode.uint32 0) (FieldNumber 1))
  <*> at (repeated Decode.byteString) (FieldNumber 2)
  where
    toHPType :: Word32 -> HolePunchType
    toHPType w = case wordToHolePunchType w of
      Just t  -> t
      Nothing -> HPConnect  -- default for unknown (should not happen per spec)

-- Wire framing

-- | Encode with uvarint length prefix.
encodeHolePunchFramed :: HolePunchMessage -> ByteString
encodeHolePunchFramed msg =
  let payload = encodeHolePunchMessage msg
      lenPrefix = encodeUvarint (fromIntegral (BS.length payload))
  in lenPrefix <> payload

-- | Decode from uvarint-length-prefixed bytes.
decodeHolePunchFramed :: Int -> ByteString -> Either String HolePunchMessage
decodeHolePunchFramed maxSize bs = do
  (len, rest) <- decodeUvarint bs
  let msgLen = fromIntegral len :: Int
  if msgLen > maxSize
    then Left $ "DCUtR message too large: " ++ show msgLen ++ " > " ++ show maxSize
    else if BS.length rest < msgLen
      then Left "DCUtR message truncated"
      else case decodeHolePunchMessage (BS.take msgLen rest) of
        Left err -> Left $ "DCUtR protobuf decode error: " ++ show err
        Right msg -> Right msg

-- Stream I/O

-- | Write a framed HolePunch message to a stream.
writeHolePunchMessage :: StreamIO -> HolePunchMessage -> IO ()
writeHolePunchMessage stream msg = streamWrite stream (encodeHolePunchFramed msg)

-- | Read a framed HolePunch message from a stream.
readHolePunchMessage :: StreamIO -> Int -> IO (Either String HolePunchMessage)
readHolePunchMessage stream maxSize = do
  varintBytes <- readVarintBytes stream
  case decodeUvarint varintBytes of
    Left err -> pure (Left $ "DCUtR varint decode error: " ++ err)
    Right (len, _) -> do
      let msgLen = fromIntegral len :: Int
      if msgLen > maxSize
        then pure (Left $ "DCUtR message too large: " ++ show msgLen)
        else do
          payload <- readExact stream msgLen
          case decodeHolePunchMessage payload of
            Left err -> pure (Left $ "DCUtR protobuf decode error: " ++ show err)
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
