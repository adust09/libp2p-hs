-- | Ping protocol implementation (docs/07-protocols.md).
--
-- Protocol ID: /ipfs/ping/1.0.0
--
-- Wire format: 32 bytes random → 32 bytes echo. No framing, no protobuf.
-- The handler runs an echo loop: reads 32 bytes, writes them back,
-- until the stream closes. The initiator sends 32 random bytes,
-- measures round-trip time, and verifies the echo matches.
module Network.LibP2P.Protocol.Ping.Ping
  ( -- * Protocol ID
    pingProtocolId
    -- * Types
  , PingError (..)
  , PingResult (..)
    -- * Protocol logic
  , handlePing
  , sendPing
    -- * Registration
  , registerPingHandler
    -- * Constants
  , pingSize
  ) where

import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Exception (SomeException, catch)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Crypto.Random (getRandomBytes)
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.MultistreamSelect.Negotiation
  ( StreamIO (..)
  , negotiateInitiator
  , NegotiationResult (..)
  )
import Network.LibP2P.Switch.Types
  ( Connection (..)
  , MuxerSession (..)
  , Switch (..)
  )

-- | Ping protocol ID.
pingProtocolId :: Text
pingProtocolId = "/ipfs/ping/1.0.0"

-- | Ping payload size: 32 bytes.
pingSize :: Int
pingSize = 32

-- | Ping error types.
data PingError
  = PingTimeout          -- ^ No response within timeout
  | PingMismatch         -- ^ Response doesn't match sent bytes
  | PingStreamError !String  -- ^ Stream I/O error
  deriving (Show, Eq)

-- | Successful ping result.
data PingResult = PingResult
  { pingRTT :: !NominalDiffTime  -- ^ Round-trip time
  } deriving (Show, Eq)

-- | Handle an inbound Ping request (responder / echo loop).
--
-- Reads 32 bytes, writes them back. Repeats until stream closes.
handlePing :: StreamIO -> PeerId -> IO ()
handlePing stream _remotePeerId = echoLoop
  where
    echoLoop = do
      result <- (Right <$> readExact stream pingSize) `catch`
                (\(_ :: SomeException) -> pure (Left ()))
      case result of
        Left () -> pure ()  -- Stream closed, exit loop
        Right payload -> do
          streamWrite stream payload
          echoLoop

-- | Send a Ping to a remote peer (initiator side).
--
-- Opens a new stream, negotiates /ipfs/ping/1.0.0, sends 32 random
-- bytes, reads 32 bytes back, verifies match, measures RTT.
sendPing :: Connection -> IO (Either PingError PingResult)
sendPing conn = do
  stream <- muxOpenStream (connSession conn)
  result <- negotiateInitiator stream [pingProtocolId]
  case result of
    NoProtocol -> pure (Left (PingStreamError "remote does not support ping"))
    Accepted _ -> do
      payload <- getRandomBytes pingSize :: IO ByteString
      t0 <- getCurrentTime
      streamWrite stream payload
      response <- (Right <$> readExact stream pingSize) `catch`
                  (\(_ :: SomeException) -> pure (Left (PingStreamError "read failed")))
      case response of
        Left err -> pure (Left err)
        Right echo
          | echo /= payload -> pure (Left PingMismatch)
          | otherwise -> do
              t1 <- getCurrentTime
              pure (Right (PingResult (diffUTCTime t1 t0)))

-- | Register the Ping handler on the Switch.
registerPingHandler :: Switch -> IO ()
registerPingHandler sw = atomically $ do
  protos <- readTVar (swProtocols sw)
  writeTVar (swProtocols sw) (Map.insert pingProtocolId handlePing protos)

-- | Read exactly n bytes from a StreamIO.
readExact :: StreamIO -> Int -> IO ByteString
readExact stream n = BS.pack <$> mapM (const (streamReadByte stream)) [1 .. n]
