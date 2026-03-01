-- | multistream-select protocol negotiation.
--
-- Implements Initiator and Responder roles for negotiating
-- which protocol to use over a connection or stream.
module Network.LibP2P.MultistreamSelect.Negotiation
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
import Data.Word (Word8)
import Network.LibP2P.Core.Varint (decodeUvarint)
import Network.LibP2P.MultistreamSelect.Wire

-- | A protocol identifier (e.g. "/noise", "/yamux/1.0.0").
type ProtocolId = Text

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
readMessage :: StreamIO -> IO (Either String Text)
readMessage stream = do
  varintBytes <- readVarint stream
  case decodeUvarint varintBytes of
    Left err -> pure (Left err)
    Right (len, _) -> do
      let payloadLen = fromIntegral len :: Int
      payload <- readExact stream payloadLen
      case decodeMessage (varintBytes <> payload) of
        Left err -> pure (Left err)
        Right (msg, _) -> pure (Right msg)

-- | Read a varint one byte at a time from the stream.
readVarint :: StreamIO -> IO ByteString
readVarint stream = go BS.empty
  where
    go acc = do
      b <- streamReadByte stream
      let acc' = acc <> BS.singleton b
      if b < 0x80
        then pure acc'
        else go acc'

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
