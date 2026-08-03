-- | multistream-select protocol negotiation.
--
-- Implements Initiator and Responder roles for negotiating
-- which protocol to use over a connection or stream.
module LibP2P.MultistreamSelect.Negotiation
  ( NegotiationResult (..)
  , ProtocolId
  , StreamIO (..)
  , negotiateInitiator
  , negotiateResponder
  , mkMemoryStreamPair
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Word (Word64, Word8)
import LibP2P.Core.Varint (decodeUvarint, maxVarintBytes)
import LibP2P.MultistreamSelect.Wire

-- | A protocol identifier (e.g. "/noise", "/yamux/1.0.0").
type ProtocolId = Text

-- | Maximum accepted multistream-select message length in bytes.
-- Negotiation runs on raw, unauthenticated connections before any
-- handshake, so the declared length must be capped before allocating
-- or reading the payload. go-multistream rejects messages over 1024
-- bytes ("incoming message was too large"); protocol ids are far shorter.
maxMessageLength :: Word64
maxMessageLength = 1024

-- | Result of a negotiation attempt.
data NegotiationResult
  = Accepted !ProtocolId
  | NoProtocol
  deriving (Show, Eq)

-- | Abstraction for stream I/O to enable testing with in-memory buffers.
data StreamIO = StreamIO
  { streamWrite    :: ByteString -> IO ()
  , streamReadByte :: IO Word8   -- ^ Read exactly one byte (blocks until available)
  , streamClose    :: IO ()      -- ^ Close/half-close the stream (signals EOF to remote)
  }

-- | Create an in-memory stream pair for testing using STM TQueue.
-- Writes to stream A appear as reads on stream B and vice versa.
mkMemoryStreamPair :: IO (StreamIO, StreamIO)
mkMemoryStreamPair = do
  queueAtoB <- newTQueueIO :: IO (TQueue Word8)
  queueBtoA <- newTQueueIO :: IO (TQueue Word8)
  let writeToQueue q bs = mapM_ (atomically . writeTQueue q) (BS.unpack bs)
      readFromQueue q = atomically (readTQueue q)
  pure
    ( StreamIO (writeToQueue queueAtoB) (readFromQueue queueBtoA) (pure ())
    , StreamIO (writeToQueue queueBtoA) (readFromQueue queueAtoB) (pure ())
    )

-- | Read exactly n bytes from a stream.
readExact :: StreamIO -> Int -> IO ByteString
readExact stream n = BS.pack <$> mapM (const (streamReadByte stream)) [1 .. n]

-- | Read a complete multistream-select message from a stream.
-- Reads varint length byte-by-byte, then reads the full payload.
-- The declared length is validated against 'maxMessageLength' before
-- any payload byte is read.
readMessage :: StreamIO -> IO (Either String Text)
readMessage stream = do
  varintResult <- readVarint stream
  case varintResult of
    Left err -> pure (Left err)
    Right varintBytes ->
      case decodeUvarint varintBytes of
        Left err -> pure (Left err)
        Right (len, _)
          | len > maxMessageLength ->
              pure (Left "readMessage: incoming message too large (max 1024 bytes)")
          | otherwise -> do
              payload <- readExact stream (fromIntegral len)
              case decodeMessage (varintBytes <> payload) of
                Left err -> pure (Left err)
                Right (msg, _) -> pure (Right msg)

-- | Read a varint one byte at a time from the stream.
-- The read loop is bounded at 'maxVarintBytes' (9 bytes per the
-- unsigned-varint spec) so a peer streaming continuation bytes (0x80)
-- cannot keep us reading and accumulating forever.
readVarint :: StreamIO -> IO (Either String ByteString)
readVarint stream = go 0 []
  where
    go :: Int -> [Word8] -> IO (Either String ByteString)
    go n acc
      | n >= maxVarintBytes =
          pure (Left "readVarint: varint too long (exceeds 9 bytes)")
      | otherwise = do
          b <- streamReadByte stream
          if b < 0x80
            then pure (Right (BS.pack (reverse (b : acc))))
            else go (n + 1) (b : acc)

-- | Write a multistream-select message to a stream.
writeMessage :: StreamIO -> Text -> IO ()
writeMessage stream msg = streamWrite stream (encodeMessage msg)

-- | Negotiate as the Initiator.
-- Sends header, then tries each protocol in order until one is accepted.
negotiateInitiator :: StreamIO -> [ProtocolId] -> IO NegotiationResult
negotiateInitiator stream protocols = do
  writeMessage stream multistreamHeader
  result <- readMessage stream
  case result of
    Left _ -> pure NoProtocol
    Right header
      | header /= multistreamHeader -> pure NoProtocol
      | otherwise -> tryProtocols protocols
  where
    tryProtocols [] = pure NoProtocol
    tryProtocols (proto : rest) = do
      writeMessage stream proto
      result <- readMessage stream
      case result of
        Left _ -> pure NoProtocol
        Right response
          | response == proto -> pure (Accepted proto)
          | response == naMessage -> tryProtocols rest
          | otherwise -> pure NoProtocol

-- | Negotiate as the Responder.
-- Receives header, then responds to the initiator's proposal.
negotiateResponder :: StreamIO -> [ProtocolId] -> IO NegotiationResult
negotiateResponder stream supported = do
  result <- readMessage stream
  case result of
    Left _ -> pure NoProtocol
    Right header
      | header /= multistreamHeader -> pure NoProtocol
      | otherwise -> do
          writeMessage stream multistreamHeader
          handleProposals
  where
    handleProposals = do
      result <- readMessage stream
      case result of
        Left _ -> pure NoProtocol
        Right proposal
          | proposal `elem` supported -> do
              writeMessage stream proposal
              pure (Accepted proposal)
          | otherwise -> do
              writeMessage stream naMessage
              handleProposals
