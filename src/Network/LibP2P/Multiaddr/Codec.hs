-- | Binary and text encoding/decoding for multiaddr.
module Network.LibP2P.Multiaddr.Codec
  ( encodeProtocols
  , decodeProtocols
  , protocolsToText
  , textToProtocols
  ) where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import Network.LibP2P.Core.Varint (decodeUvarint, encodeUvarint)
import Network.LibP2P.Multiaddr.Protocol

-- | Encode a list of protocols to binary multiaddr format.
encodeProtocols :: [Protocol] -> ByteString
encodeProtocols = BS.concat . map encodeOne
  where
    encodeOne :: Protocol -> ByteString
    encodeOne p =
      let code = encodeUvarint (protocolCode p)
          addr = encodeAddress p
       in code <> addr

    encodeAddress :: Protocol -> ByteString
    encodeAddress (IP4 w) = word32BE w
    encodeAddress (IP6 bs) = bs
    encodeAddress (TCP port) = word16BE port
    encodeAddress (UDP port) = word16BE port
    encodeAddress (P2P mh) = encodeUvarint (fromIntegral (BS.length mh)) <> mh
    encodeAddress (DNS t) = encodeVarText t
    encodeAddress (DNS4 t) = encodeVarText t
    encodeAddress (DNS6 t) = encodeVarText t
    encodeAddress (DNSAddr t) = encodeVarText t
    encodeAddress QuicV1 = BS.empty
    encodeAddress WS = BS.empty
    encodeAddress WSS = BS.empty
    encodeAddress P2PCircuit = BS.empty
    encodeAddress WebTransport = BS.empty
    encodeAddress NoiseProto = BS.empty
    encodeAddress YamuxProto = BS.empty

    encodeVarText :: Text -> ByteString
    encodeVarText t =
      let bs = encodeUtf8 t
       in encodeUvarint (fromIntegral (BS.length bs)) <> bs

-- | Decode binary multiaddr format to a list of protocols.
decodeProtocols :: ByteString -> Either String [Protocol]
decodeProtocols bs
  | BS.null bs = Right []
  | otherwise = do
      (code, rest1) <- decodeUvarint bs
      case protocolAddressSize code of
        Nothing -> Left $ "decodeProtocols: unknown protocol code " <> show code
        Just addrSize -> do
          (proto, rest2) <- decodeAddress code addrSize rest1
          rest <- decodeProtocols rest2
          Right (proto : rest)
  where
    decodeAddress :: Word64 -> AddressSize -> ByteString -> Either String (Protocol, ByteString)
    decodeAddress code (Fixed n) input
      | BS.length input < n =
          Left $ "decodeProtocols: not enough bytes for protocol " <> show code
      | otherwise =
          let (addr, rest) = BS.splitAt n input
           in case buildProtocol code addr of
                Just p -> Right (p, rest)
                Nothing -> Left $ "decodeProtocols: failed to parse address for code " <> show code
    decodeAddress code VarIntPrefixed input = do
      (len, rest1) <- decodeUvarint input
      let n = fromIntegral len :: Int
      if BS.length rest1 < n
        then Left $ "decodeProtocols: not enough bytes for varint-prefixed protocol " <> show code
        else
          let (addr, rest2) = BS.splitAt n rest1
           in case buildVarProtocol code addr of
                Just p -> Right (p, rest2)
                Nothing -> Left $ "decodeProtocols: failed to parse varint-prefixed address for code " <> show code
    decodeAddress code NoAddress input =
      case buildNoAddrProtocol code of
        Just p -> Right (p, input)
        Nothing -> Left $ "decodeProtocols: unknown no-address protocol " <> show code

    buildProtocol :: Word64 -> ByteString -> Maybe Protocol
    buildProtocol 4 bs'
      | BS.length bs' == 4 = Just $ IP4 (readWord32BE bs')
    buildProtocol 41 bs'
      | BS.length bs' == 16 = Just $ IP6 bs'
    buildProtocol 6 bs'
      | BS.length bs' == 2 = Just $ TCP (readWord16BE bs')
    buildProtocol 273 bs'
      | BS.length bs' == 2 = Just $ UDP (readWord16BE bs')
    buildProtocol _ _ = Nothing

    buildVarProtocol :: Word64 -> ByteString -> Maybe Protocol
    buildVarProtocol 421 mh = Just $ P2P mh
    buildVarProtocol 53 bs' = Just $ DNS (decodeUtf8 bs')
    buildVarProtocol 54 bs' = Just $ DNS4 (decodeUtf8 bs')
    buildVarProtocol 55 bs' = Just $ DNS6 (decodeUtf8 bs')
    buildVarProtocol 56 bs' = Just $ DNSAddr (decodeUtf8 bs')
    buildVarProtocol _ _ = Nothing

    buildNoAddrProtocol :: Word64 -> Maybe Protocol
    buildNoAddrProtocol 460 = Just QuicV1
    buildNoAddrProtocol 477 = Just WS
    buildNoAddrProtocol 478 = Just WSS
    buildNoAddrProtocol 290 = Just P2PCircuit
    buildNoAddrProtocol 465 = Just WebTransport
    buildNoAddrProtocol 454 = Just NoiseProto
    buildNoAddrProtocol 467 = Just YamuxProto
    buildNoAddrProtocol _ = Nothing

-- | Convert a list of protocols to human-readable text form.
protocolsToText :: [Protocol] -> Text
protocolsToText = T.concat . map renderOne
  where
    renderOne :: Protocol -> Text
    renderOne p@(IP4 w) = "/" <> protocolName p <> "/" <> renderIPv4 w
    renderOne p@(IP6 _bs) = "/" <> protocolName p <> "/" <> "::1" -- TODO: proper IPv6 rendering
    renderOne p@(TCP port) = "/" <> protocolName p <> "/" <> T.pack (show port)
    renderOne p@(UDP port) = "/" <> protocolName p <> "/" <> T.pack (show port)
    renderOne p@(P2P mh) = "/" <> protocolName p <> "/" <> renderBase58 mh
    renderOne p@(DNS t) = "/" <> protocolName p <> "/" <> t
    renderOne p@(DNS4 t) = "/" <> protocolName p <> "/" <> t
    renderOne p@(DNS6 t) = "/" <> protocolName p <> "/" <> t
    renderOne p@(DNSAddr t) = "/" <> protocolName p <> "/" <> t
    renderOne p = "/" <> protocolName p

    renderIPv4 :: Word32 -> Text
    renderIPv4 w =
      let a = (w `shiftR` 24) .&. 0xff
          b = (w `shiftR` 16) .&. 0xff
          c = (w `shiftR` 8) .&. 0xff
          d = w .&. 0xff
       in T.pack $ show a <> "." <> show b <> "." <> show c <> "." <> show d

    -- Minimal base58btc encoding for PeerId display
    renderBase58 :: ByteString -> Text
    renderBase58 = T.pack . base58Encode . BS.unpack

-- | Parse human-readable text form to a list of protocols.
textToProtocols :: Text -> Either String [Protocol]
textToProtocols input
  | T.null input = Right []
  | otherwise =
      let parts = filter (not . T.null) $ T.splitOn "/" input
       in parseParts parts
  where
    parseParts :: [Text] -> Either String [Protocol]
    parseParts [] = Right []
    parseParts (name : rest) = case name of
      "ip4" -> withAddr rest $ \addr remaining -> do
        w <- parseIPv4 addr
        Right (IP4 w, remaining)
      "ip6" -> withAddr rest $ \addr remaining -> do
        w <- parseIPv6 addr
        Right (IP6 w, remaining)
      "tcp" -> withAddr rest $ \addr remaining -> do
        port <- parsePort addr
        Right (TCP port, remaining)
      "udp" -> withAddr rest $ \addr remaining -> do
        port <- parsePort addr
        Right (UDP port, remaining)
      "p2p" -> withAddr rest $ \addr remaining -> do
        mh <- parseBase58PeerId addr
        Right (P2P mh, remaining)
      "dns" -> withAddr rest $ \addr remaining ->
        Right (DNS addr, remaining)
      "dns4" -> withAddr rest $ \addr remaining ->
        Right (DNS4 addr, remaining)
      "dns6" -> withAddr rest $ \addr remaining ->
        Right (DNS6 addr, remaining)
      "dnsaddr" -> withAddr rest $ \addr remaining ->
        Right (DNSAddr addr, remaining)
      "quic-v1" -> do
        more <- parseParts rest
        Right (QuicV1 : more)
      "ws" -> do
        more <- parseParts rest
        Right (WS : more)
      "wss" -> do
        more <- parseParts rest
        Right (WSS : more)
      "p2p-circuit" -> do
        more <- parseParts rest
        Right (P2PCircuit : more)
      "webtransport" -> do
        more <- parseParts rest
        Right (WebTransport : more)
      "noise" -> do
        more <- parseParts rest
        Right (NoiseProto : more)
      "yamux" -> do
        more <- parseParts rest
        Right (YamuxProto : more)
      other -> Left $ "textToProtocols: unknown protocol " <> T.unpack other

    withAddr :: [Text] -> (Text -> [Text] -> Either String (Protocol, [Text])) -> Either String [Protocol]
    withAddr [] _ = Left "textToProtocols: expected address but got end of input"
    withAddr (addr : remaining) f = do
      (proto, rest) <- f addr remaining
      more <- parseParts rest
      Right (proto : more)

    parseIPv4 :: Text -> Either String Word32
    parseIPv4 t = case map (readMaybe . T.unpack) (T.splitOn "." t) of
      [Just a, Just b, Just c, Just d]
        | all (\x -> x <= (255 :: Int)) [a, b, c, d] ->
            Right $
              (fromIntegral a `shiftL` 24)
                + (fromIntegral b `shiftL` 16)
                + (fromIntegral c `shiftL` 8)
                + fromIntegral d
      _ -> Left $ "textToProtocols: invalid IPv4 address: " <> T.unpack t

    parseIPv6 :: Text -> Either String ByteString
    parseIPv6 _ = Left "textToProtocols: IPv6 parsing not yet implemented"

    parsePort :: Text -> Either String Word16
    parsePort t = case readMaybe (T.unpack t) of
      Just n
        | n >= (0 :: Int) && n <= 65535 -> Right (fromIntegral n)
      _ -> Left $ "textToProtocols: invalid port: " <> T.unpack t

    parseBase58PeerId :: Text -> Either String ByteString
    parseBase58PeerId t = case base58Decode (T.unpack t) of
      Just bs -> Right bs
      Nothing -> Left $ "textToProtocols: invalid base58 peer ID: " <> T.unpack t

-- Helpers

word16BE :: Word16 -> ByteString
word16BE w = BS.pack [fromIntegral (w `shiftR` 8), fromIntegral w]

word32BE :: Word32 -> ByteString
word32BE w =
  BS.pack
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

readWord16BE :: ByteString -> Word16
readWord16BE bs =
  let [a, b] = BS.unpack (BS.take 2 bs)
   in (fromIntegral a `shiftL` 8) + fromIntegral b

readWord32BE :: ByteString -> Word32
readWord32BE bs =
  let [a, b, c, d] = BS.unpack (BS.take 4 bs)
   in (fromIntegral a `shiftL` 24)
        + (fromIntegral b `shiftL` 16)
        + (fromIntegral c `shiftL` 8)
        + fromIntegral d

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

encodeUtf8 :: Text -> ByteString
encodeUtf8 = BS.pack . map (fromIntegral . fromEnum) . T.unpack

decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.pack . map (toEnum . fromIntegral) . BS.unpack

-- Base58btc (Bitcoin alphabet) encoding/decoding
base58Alphabet :: String
base58Alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

base58Encode :: [Word8] -> String
base58Encode bytes =
  let leadingZeros = length (takeWhile (== 0) bytes)
      n = foldl (\acc b -> acc * 256 + toInteger b) 0 bytes
      encoded = encodeN n
   in replicate leadingZeros '1' <> encoded
  where
    encodeN :: Integer -> String
    encodeN 0 = ""
    encodeN n =
      let (q, r) = n `divMod` 58
       in encodeN q <> [base58Alphabet !! fromIntegral r]

base58Decode :: String -> Maybe ByteString
base58Decode str =
  let leadingOnes = length (takeWhile (== '1') str)
   in do
        n <- decodeChars str
        let bytes = decodeN n
        Just $ BS.pack (replicate leadingOnes 0 <> bytes)
  where
    decodeChars :: String -> Maybe Integer
    decodeChars = foldl step (Just 0)
      where
        step Nothing _ = Nothing
        step (Just acc) c = case charIndex c of
          Nothing -> Nothing
          Just i -> Just (acc * 58 + toInteger i)

    charIndex :: Char -> Maybe Int
    charIndex c = lookup c (zip base58Alphabet [0 ..])

    decodeN :: Integer -> [Word8]
    decodeN 0 = []
    decodeN n =
      let (q, r) = n `divMod` 256
       in decodeN q <> [fromIntegral r]
