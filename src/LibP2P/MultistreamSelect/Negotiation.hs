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
  , readExactBounded
  ) where

import Control.Concurrent.STM
import Control.Exception (IOException, catch)
import Control.Monad (replicateM)
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

-- | Chunk size for 'readExactBounded'. Bounds the transient boxed-list
-- allocation per read step regardless of the requested length.
readChunkSize :: Int
readChunkSize = 32768

-- | Read exactly @n@ bytes from a stream, bounded by @maxLen@.
--
-- Shared by every length-delimited protocol in the stack (see issue
-- #169): the declared length is validated against the caller's
-- protocol-defined cap before a single byte is read or allocated, so a
-- hostile length prefix cannot trigger an unbounded allocation. Bytes
-- are accumulated in chunks of at most 'readChunkSize', keeping
-- transient memory use proportional to the chunk size, not to @n@.
--
-- I/O failures during the read (stream reset, EOF) are returned as
-- 'Left' instead of propagating as 'IOException's.
readExactBounded
  :: StreamIO
  -> Int  -- ^ Maximum acceptable length (protocol-defined cap)
  -> Int  -- ^ Number of bytes to read
  -> IO (Either String ByteString)
readExactBounded stream maxLen n
  | n < 0 =
      pure (Left ("readExactBounded: negative length: " <> show n))
  | n > maxLen =
      pure (Left ("readExactBounded: requested " <> show n
                  <> " bytes exceeds maximum " <> show maxLen))
  | n == 0 = pure (Right BS.empty)
  | otherwise =
      (Right . BS.concat <$> go n) `catch` \(e :: IOException) ->
        pure (Left ("readExactBounded: read failed: " <> show e))
  where
    go :: Int -> IO [ByteString]
    go 0 = pure []
    go remaining = do
      let m = min readChunkSize remaining
      chunk <- BS.pack <$> replicateM m (streamReadByte stream)
      (chunk :) <$> go (remaining - m)

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
              payloadOrErr <-
                readExactBounded stream (fromIntegral maxMessageLength) (fromIntegral len)
              case payloadOrErr of
                Left err -> pure (Left err)
                Right payload ->
                  case decodeMessage (varintBytes <> payload) of
                    Left err -> pure (Left err)
                    Right (msg, _) -> pure (Right msg)

-- | Read a varint one byte at a time from the stream.
-- The read loop is bounded at 'maxVarintBytes' (9 bytes per the
-- unsigned-varint spec) so a peer streaming continuation bytes (0x80)
-- cannot keep us reading and accumulating forever.
--
-- Like 'readExactBounded', I/O failures (stream reset, EOF from a peer
-- that disconnected mid-negotiation) are returned as 'Left', so the
-- negotiation functions report them as 'NoProtocol' instead of leaking
-- an exception.
readVarint :: StreamIO -> IO (Either String ByteString)
readVarint stream =
  go 0 [] `catch` \(e :: IOException) ->
    pure (Left ("readVarint: read failed: " <> show e))
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
--
-- Pipelines the multistream header and the first protocol proposal in a
-- single write before reading anything, as the multistream-select spec
-- recommends ("the initiator SHOULD pipeline the multistream protocol
-- id and the desired protocol id in the same packet"): this saves one
-- round trip per negotiation. It then reads the header echo and the
-- reply to the optimistic proposal; on @na@ it falls back to proposing
-- the remaining protocols sequentially.
negotiateInitiator :: StreamIO -> [ProtocolId] -> IO NegotiationResult
negotiateInitiator stream [] = do
  -- Nothing to propose: announce the header only (as before pipelining)
  -- and fail the negotiation after checking the peer's echo.
  writeMessage stream multistreamHeader
  result <- readMessage stream
  case result of
    Left _ -> pure NoProtocol
    Right _ -> pure NoProtocol
negotiateInitiator stream (firstProto : rest) = do
  streamWrite stream (encodeMessage multistreamHeader <> encodeMessage firstProto)
  headerReply <- readMessage stream
  case headerReply of
    Left _ -> pure NoProtocol
    Right header
      | header /= multistreamHeader -> pure NoProtocol
      | otherwise -> awaitReply firstProto (tryProtocols rest)
  where
    tryProtocols [] = pure NoProtocol
    tryProtocols (proto : remaining) = do
      writeMessage stream proto
      awaitReply proto (tryProtocols remaining)

    -- Read the responder's answer to an already-sent proposal: an echo
    -- accepts it, @na@ runs the fallback, anything else is a protocol
    -- violation.
    awaitReply proto onNa = do
      result <- readMessage stream
      case result of
        Left _ -> pure NoProtocol
        Right response
          | response == proto -> pure (Accepted proto)
          | response == naMessage -> onNa
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
