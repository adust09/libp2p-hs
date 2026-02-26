-- | Connection upgrade pipeline for the Switch.
--
-- Transforms a raw transport connection into a fully upgraded
-- (secure + multiplexed) Connection by executing a 4-step pipeline:
--   1. multistream-select: negotiate security protocol ("/noise")
--   2. Noise XX handshake: encrypted channel + remote PeerId
--   3. multistream-select: negotiate muxer ("/yamux/1.0.0")
--   4. Yamux session init: multiplexed streams
--
-- See docs/08-switch.md §Connection Upgrading Pipeline.
module Network.LibP2P.Switch.Upgrade
  ( -- * Streaming handshake
    performStreamHandshake
    -- * Encrypted StreamIO
  , noiseSessionToStreamIO
    -- * Yamux → MuxerSession adapter
  , yamuxToMuxerSession
    -- * Full upgrade pipeline
  , upgradeOutbound
  , upgradeInbound
    -- * Helpers (exported for testing)
  , readExact
  , readFramedMessage
  , writeFramedMessage
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef)
import Network.LibP2P.Crypto.Key (KeyPair)
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import Network.LibP2P.Security.Noise.Handshake (HandshakeResult)
import Network.LibP2P.Security.Noise.Session (NoiseSession)
import Network.LibP2P.Switch.Types (Connection, Direction (..), MuxerSession)
import Network.LibP2P.Transport.Transport (RawConnection)

-- | Read exactly n bytes from a StreamIO.
readExact :: StreamIO -> Int -> IO ByteString
readExact stream n = BS.pack <$> mapM (const (streamReadByte stream)) [1 .. n]

-- | Read a 2-byte-length-prefixed Noise frame from a StreamIO.
readFramedMessage :: StreamIO -> IO ByteString
readFramedMessage = error "readFramedMessage: not yet implemented"

-- | Write a 2-byte-length-prefixed Noise frame to a StreamIO.
writeFramedMessage :: StreamIO -> ByteString -> IO ()
writeFramedMessage = error "writeFramedMessage: not yet implemented"

-- | Perform a Noise XX handshake over a StreamIO.
-- Returns the NoiseSession for post-handshake encryption and the HandshakeResult
-- containing the remote PeerId and public key.
performStreamHandshake
  :: KeyPair -> Direction -> StreamIO -> IO (NoiseSession, HandshakeResult)
performStreamHandshake = error "performStreamHandshake: not yet implemented"

-- | Create an encrypted StreamIO from a NoiseSession and raw StreamIO.
--
-- Uses separate IORefs for send/recv session state (each direction's
-- CipherState is independent in Noise). A read buffer (IORef ByteString)
-- bridges Noise's message-boundary decryption with StreamIO's byte-level reads.
noiseSessionToStreamIO
  :: IORef NoiseSession    -- ^ Send session state
  -> IORef NoiseSession    -- ^ Recv session state
  -> IORef ByteString      -- ^ Read buffer (decrypted but unconsumed bytes)
  -> StreamIO              -- ^ Raw (unencrypted) StreamIO
  -> StreamIO
noiseSessionToStreamIO = error "noiseSessionToStreamIO: not yet implemented"

-- | Adapter: wrap a YamuxSession + background loops as a MuxerSession.
yamuxToMuxerSession :: MuxerSession
yamuxToMuxerSession = error "yamuxToMuxerSession: not yet implemented"

-- | Upgrade a raw outbound connection: mss→Noise→mss→Yamux.
-- Dialer role: initiator for multistream-select and Noise, client for Yamux.
upgradeOutbound :: KeyPair -> RawConnection -> IO Connection
upgradeOutbound = error "upgradeOutbound: not yet implemented"

-- | Upgrade a raw inbound connection: mss→Noise→mss→Yamux.
-- Listener role: responder for multistream-select and Noise, server for Yamux.
upgradeInbound :: KeyPair -> RawConnection -> IO Connection
upgradeInbound = error "upgradeInbound: not yet implemented"
