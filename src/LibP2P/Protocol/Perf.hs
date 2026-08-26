-- | Perf protocol implementation (specs/perf).
--
-- Protocol ID: /perf/1.0.0
--
-- Wire format: the client sends a single 8-byte big-endian uint64
-- naming the number of bytes it wants the server to send back, then
-- streams its upload payload, then half-closes its write side. The
-- server reads the 8-byte header, drains the upload until EOF, and
-- only then (perf.md: the response "MUST NOT be run concurrently"
-- with the upload) writes the requested number of bytes back and
-- closes the stream.
--
-- Each measurement runs on its own stream; there is no framing and no
-- protobuf. Payload bytes carry no meaning, so both sides send zeros.
module LibP2P.Protocol.Perf
  ( -- * Protocol ID
    perfProtocolId
    -- * Types
  , PerfError (..)
  , PerfResult (..)
    -- * Responder
  , handlePerf
    -- * Initiator
  , perfOnStream
  , runPerf
    -- * Registration
  , registerPerfHandler
  ) where

import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Exception (SomeException, catch, finally, try)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import LibP2P.Core.Binary (readWord64BE, word64BE)
import LibP2P.Crypto.PeerId (PeerId)
import LibP2P.MultistreamSelect.Negotiation
  ( NegotiationResult (..)
  , StreamIO (..)
  , closeQuietly
  , negotiateInitiator
  , readExactBounded
  )
import LibP2P.Switch.Connection (newStream)
import LibP2P.Switch.Types
  ( Connection (..)
  , Switch (..)
  )

-- | Perf protocol ID.
perfProtocolId :: Text
perfProtocolId = "/perf/1.0.0"

-- | Chunk size for bulk sends, matching the 64 KiB block the reference
-- implementations use.
perfBlockSize :: Int
perfBlockSize = 65536

-- | Perf error types.
data PerfError
  = PerfNegotiationError !String  -- ^ Stream open or protocol negotiation failed
  | PerfStreamError !String       -- ^ I/O error during the exchange
  deriving (Show, Eq)

-- | Successful perf exchange result.
data PerfResult = PerfResult
  { perfElapsed :: !NominalDiffTime  -- ^ Header write to last byte received
  } deriving (Show, Eq)

-- | A shared zero block for bulk sends.
zeroBlock :: BS.ByteString
zeroBlock = BS.replicate perfBlockSize 0

-- | Write @n@ zero bytes in 'perfBlockSize' chunks.
writeZeros :: StreamIO -> Word64 -> IO ()
writeZeros stream = go
  where
    go 0 = pure ()
    go n = do
      let chunk = min n (fromIntegral perfBlockSize)
      streamWrite stream (BS.take (fromIntegral chunk) zeroBlock)
      go (n - chunk)

-- | Read and discard bytes until EOF (the initiator's half-close).
drainUntilEof :: StreamIO -> IO ()
drainUntilEof stream = loop `catch` \(_ :: SomeException) -> pure ()
  where
    loop = streamReadByte stream >> loop

-- | Read and discard exactly @n@ bytes. The payload carries no meaning,
-- so no ByteString is built; premature EOF throws.
discardExactly :: StreamIO -> Word64 -> IO ()
discardExactly stream = go
  where
    go :: Word64 -> IO ()
    go 0 = pure ()
    go !n = streamReadByte stream >> go (n - 1)

-- | Handle an inbound perf request (responder).
--
-- Reads the 8-byte download size, drains the client's upload until it
-- half-closes, then sends the requested bytes back and closes. A client
-- that closes before sending a full header is dropped silently.
handlePerf :: StreamIO -> PeerId -> IO ()
handlePerf stream _remotePeerId = serve `finally` closeQuietly stream
  where
    serve = do
      header <- readExactBounded stream 8 8 `catch`
                (\(_ :: SomeException) -> pure (Left "stream closed"))
      case header of
        Left _ -> pure ()
        Right sizeBytes -> do
          let downloadSize = readWord64BE sizeBytes
          drainUntilEof stream
          writeZeros stream downloadSize
            `catch` (\(_ :: SomeException) -> pure ())

-- | One perf exchange on an already-negotiated stream (initiator).
--
-- Sends the header and @uploadBytes@ zeros, half-closes the write side,
-- then reads exactly @downloadBytes@ back. The elapsed time covers the
-- full exchange, header write to last byte read.
perfOnStream :: StreamIO -> Word64 -> Word64 -> IO (Either PerfError PerfResult)
perfOnStream stream uploadBytes downloadBytes = do
  t0 <- getCurrentTime
  outcome <- try $ do
    streamWrite stream (word64BE downloadBytes)
    writeZeros stream uploadBytes
    streamClose stream
    discardExactly stream downloadBytes
  case outcome of
    Left (e :: SomeException) ->
      pure (Left (PerfStreamError ("perf I/O failed: " ++ show e)))
    Right () -> do
      t1 <- getCurrentTime
      pure (Right (PerfResult (diffUTCTime t1 t0)))

-- | Run one perf measurement against a connected peer: open a stream,
-- negotiate /perf/1.0.0, run the exchange, and release the stream.
runPerf :: Switch -> Connection -> Word64 -> Word64 -> IO (Either PerfError PerfResult)
runPerf sw conn uploadBytes downloadBytes = do
  streamOrErr <- newStream sw conn
  case streamOrErr of
    Left err ->
      pure (Left (PerfNegotiationError ("stream reservation failed: " ++ show err)))
    Right stream -> do
      negotiated <- try (negotiateInitiator stream [perfProtocolId])
      case negotiated of
        Right (Accepted _) ->
          perfOnStream stream uploadBytes downloadBytes
            `finally` closeQuietly stream
        Right NoProtocol -> do
          closeQuietly stream
          pure (Left (PerfNegotiationError "remote does not support perf"))
        Left (e :: SomeException) -> do
          closeQuietly stream
          pure (Left (PerfNegotiationError ("perf negotiation failed: " ++ show e)))

-- | Register the perf handler on the Switch.
registerPerfHandler :: Switch -> IO ()
registerPerfHandler sw = atomically $ do
  protos <- readTVar (swProtocols sw)
  let handler conn stream = handlePerf stream (connPeerId conn)
  writeTVar (swProtocols sw) (Map.insert perfProtocolId handler protos)
