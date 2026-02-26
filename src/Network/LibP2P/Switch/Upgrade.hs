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

import Control.Concurrent.Async (async)
import Control.Concurrent.STM (newTVarIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Network.LibP2P.Core.Binary (readWord16BE)
import Network.LibP2P.Crypto.Key (KeyPair (..))
import Network.LibP2P.Crypto.PeerId (fromPublicKey)
import Network.LibP2P.Mux.Yamux.Session (closeSession, newSession, recvLoop, sendLoop)
import qualified Network.LibP2P.Mux.Yamux.Session as Yamux
import Network.LibP2P.Mux.Yamux.Stream (streamRead)
import qualified Network.LibP2P.Mux.Yamux.Stream as YS
import Network.LibP2P.Mux.Yamux.Types (SessionRole (..), YamuxSession, YamuxStream)
import Network.LibP2P.MultistreamSelect.Negotiation
  ( NegotiationResult (..)
  , StreamIO (..)
  , negotiateInitiator
  , negotiateResponder
  )
import Network.LibP2P.Security.Noise.Framing (encodeFrame)
import Network.LibP2P.Security.Noise.Handshake
  ( HandshakeResult (..)
  , buildHandshakePayload
  , decodeNoisePayload
  , encodeNoisePayload
  , initHandshakeInitiator
  , initHandshakeResponder
  , readHandshakeMsg
  , writeHandshakeMsg
  )
import Network.LibP2P.Security.Noise.Session
  ( NoiseSession
  , decryptMessage
  , encryptMessage
  , mkNoiseSession
  )
import Network.LibP2P.Switch.Types
  ( ConnState (..)
  , Connection (..)
  , Direction (..)
  , MuxerSession (..)
  )
import Network.LibP2P.Transport.Transport (RawConnection (..))
import qualified Network.LibP2P.Crypto.Protobuf as Proto
import qualified Network.LibP2P.Security.Noise.Handshake as HS

-- | Read exactly n bytes from a StreamIO.
readExact :: StreamIO -> Int -> IO ByteString
readExact stream n = BS.pack <$> mapM (const (streamReadByte stream)) [1 .. n]

-- | Read a 2-byte-BE-length-prefixed Noise frame from a StreamIO.
readFramedMessage :: StreamIO -> IO ByteString
readFramedMessage stream = do
  lenBytes <- readExact stream 2
  let len = fromIntegral (readWord16BE lenBytes) :: Int
  if len == 0
    then pure BS.empty
    else readExact stream len

-- | Write a 2-byte-BE-length-prefixed Noise frame to a StreamIO.
writeFramedMessage :: StreamIO -> ByteString -> IO ()
writeFramedMessage stream msg = streamWrite stream (encodeFrame msg)

-- | Perform a Noise XX handshake over a StreamIO using framed messages.
-- Returns (NoiseSession, HandshakeResult) with the remote PeerId.
performStreamHandshake
  :: KeyPair -> Direction -> StreamIO -> IO (NoiseSession, HandshakeResult)
performStreamHandshake identityKP dir stream = case dir of
  Outbound -> performInitiatorHandshake identityKP stream
  Inbound  -> performResponderHandshake identityKP stream

-- | Initiator (dialer) side of the Noise XX handshake.
--
-- Message flow:
--   1. Initiator → Responder: e (empty payload)
--   2. Responder → Initiator: e, ee, s, es (responder identity payload)
--   3. Initiator → Responder: s, se (initiator identity payload)
performInitiatorHandshake :: KeyPair -> StreamIO -> IO (NoiseSession, HandshakeResult)
performInitiatorHandshake identityKP stream = do
  (hsState0, noiseStaticPub) <- initHandshakeInitiator identityKP

  -- Message 1: → (empty payload)
  let Right (msg1, hsState1) = writeHandshakeMsg hsState0 BS.empty
  writeFramedMessage stream msg1

  -- Message 2: ← (responder's identity payload)
  msg2 <- readFramedMessage stream
  let Right (payload2, hsState2) = readHandshakeMsg hsState1 msg2

  -- Decode responder's identity
  let Right remoteNP = decodeNoisePayload payload2
  let Right remotePubKey = Proto.decodePublicKey (HS.npIdentityKey remoteNP)
  let remotePeerId = fromPublicKey remotePubKey

  -- Message 3: → (initiator's identity payload)
  let identPayload = encodeNoisePayload $ buildHandshakePayload identityKP noiseStaticPub
  let Right (msg3, hsStateFinal) = writeHandshakeMsg hsState2 identPayload
  writeFramedMessage stream msg3

  let noiseSession = mkNoiseSession (HS.hsNoiseState hsStateFinal)
  pure (noiseSession, HandshakeResult remotePeerId remotePubKey)

-- | Responder (listener) side of the Noise XX handshake.
performResponderHandshake :: KeyPair -> StreamIO -> IO (NoiseSession, HandshakeResult)
performResponderHandshake identityKP stream = do
  (hsState0, noiseStaticPub) <- initHandshakeResponder identityKP

  -- Message 1: ← (empty payload)
  msg1 <- readFramedMessage stream
  let Right (_payload1, hsState1) = readHandshakeMsg hsState0 msg1

  -- Message 2: → (responder's identity payload)
  let identPayload = encodeNoisePayload $ buildHandshakePayload identityKP noiseStaticPub
  let Right (msg2, hsState2) = writeHandshakeMsg hsState1 identPayload
  writeFramedMessage stream msg2

  -- Message 3: ← (initiator's identity payload)
  msg3 <- readFramedMessage stream
  let Right (payload3, hsStateFinal) = readHandshakeMsg hsState2 msg3

  -- Decode initiator's identity
  let Right remoteNP = decodeNoisePayload payload3
  let Right remotePubKey = Proto.decodePublicKey (HS.npIdentityKey remoteNP)
  let remotePeerId = fromPublicKey remotePubKey

  let noiseSession = mkNoiseSession (HS.hsNoiseState hsStateFinal)
  pure (noiseSession, HandshakeResult remotePeerId remotePubKey)

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
noiseSessionToStreamIO sendRef recvRef bufRef rawIO = StreamIO
  { streamWrite = encryptAndWrite sendRef rawIO
  , streamReadByte = decryptAndReadByte recvRef bufRef rawIO
  }

-- | Encrypt plaintext and write as a framed Noise message.
encryptAndWrite :: IORef NoiseSession -> StreamIO -> ByteString -> IO ()
encryptAndWrite sendRef rawIO plaintext = do
  sess <- readIORef sendRef
  case encryptMessage sess plaintext of
    Left err -> fail $ "encryptAndWrite: " <> err
    Right (ct, sess') -> do
      writeIORef sendRef sess'
      writeFramedMessage rawIO ct

-- | Read and decrypt a byte from the Noise channel.
-- If the buffer has bytes, return the first. Otherwise, read a full Noise
-- frame from the raw stream, decrypt it, and buffer the result.
decryptAndReadByte :: IORef NoiseSession -> IORef ByteString -> StreamIO -> IO Word8
decryptAndReadByte recvRef bufRef rawIO = do
  buf <- readIORef bufRef
  if BS.null buf
    then do
      -- Read a full framed Noise message
      ct <- readFramedMessage rawIO
      sess <- readIORef recvRef
      case decryptMessage sess ct of
        Left err -> fail $ "decryptAndReadByte: " <> err
        Right (pt, sess') -> do
          writeIORef recvRef sess'
          if BS.null pt
            then fail "decryptAndReadByte: empty plaintext"
            else do
              writeIORef bufRef (BS.tail pt)
              pure (BS.head pt)
    else do
      writeIORef bufRef (BS.tail buf)
      pure (BS.head buf)

-- | Wrap a YamuxSession as a MuxerSession.
-- Starts sendLoop and recvLoop as background threads.
-- The MuxerSession provides open/accept stream operations that
-- produce StreamIO-compatible streams.
yamuxToMuxerSession :: YamuxSession -> IO MuxerSession
yamuxToMuxerSession yamuxSess = do
  -- Start background loops
  _ <- async (sendLoop yamuxSess)
  _ <- async (recvLoop yamuxSess)
  pure MuxerSession
    { muxOpenStream = do
        Right stream <- Yamux.openStream yamuxSess
        yamuxStreamToStreamIO stream
    , muxAcceptStream = do
        Right stream <- Yamux.acceptStream yamuxSess
        yamuxStreamToStreamIO stream
    , muxClose = closeSession yamuxSess
    }

-- | Convert a YamuxStream to StreamIO with a read buffer.
-- Yamux delivers data in chunks via streamRead, but StreamIO requires
-- byte-by-byte reads. An IORef buffer bridges this gap.
yamuxStreamToStreamIO :: YamuxStream -> IO StreamIO
yamuxStreamToStreamIO yamuxStream = do
  readBuf <- newIORef BS.empty
  pure StreamIO
    { streamWrite = \bs -> do
        Right () <- YS.streamWrite yamuxStream bs
        pure ()
    , streamReadByte = do
        buf <- readIORef readBuf
        if BS.null buf
          then do
            Right chunk <- streamRead yamuxStream
            if BS.length chunk <= 1
              then pure (BS.head chunk)
              else do
                writeIORef readBuf (BS.tail chunk)
                pure (BS.head chunk)
          else do
            writeIORef readBuf (BS.tail buf)
            pure (BS.head buf)
    }

-- | Upgrade an outbound (dialer) raw connection.
-- Pipeline: mss(/noise) → Noise XX → mss(/yamux/1.0.0) → Yamux client
upgradeOutbound :: KeyPair -> RawConnection -> IO Connection
upgradeOutbound identityKP rawConn = do
  let rawIO = rcStreamIO rawConn

  -- Step 1: multistream-select → "/noise"
  Accepted _ <- negotiateInitiator rawIO ["/noise"]

  -- Step 2: Noise XX handshake (initiator)
  (noiseSess, HandshakeResult remotePeerId _remotePK) <-
    performStreamHandshake identityKP Outbound rawIO

  -- Step 3: Create encrypted StreamIO
  sendRef <- newIORef noiseSess
  recvRef <- newIORef noiseSess
  bufRef  <- newIORef BS.empty
  let encryptedIO = noiseSessionToStreamIO sendRef recvRef bufRef rawIO

  -- Step 4: multistream-select → "/yamux/1.0.0" (over encrypted channel)
  Accepted _ <- negotiateInitiator encryptedIO ["/yamux/1.0.0"]

  -- Step 5: Initialize Yamux session (client = odd IDs)
  let yamuxWrite = streamWrite encryptedIO
      yamuxRead  = \n -> readExact encryptedIO n
  yamuxSess <- newSession RoleClient yamuxWrite yamuxRead
  muxer <- yamuxToMuxerSession yamuxSess

  -- Build Connection
  stateVar <- newTVarIO ConnOpen
  pure Connection
    { connPeerId     = remotePeerId
    , connDirection  = Outbound
    , connLocalAddr  = rcLocalAddr rawConn
    , connRemoteAddr = rcRemoteAddr rawConn
    , connSecurity   = "/noise"
    , connMuxer      = "/yamux/1.0.0"
    , connSession    = muxer
    , connState      = stateVar
    }

-- | Upgrade an inbound (listener) raw connection.
-- Pipeline: mss(/noise) → Noise XX → mss(/yamux/1.0.0) → Yamux server
upgradeInbound :: KeyPair -> RawConnection -> IO Connection
upgradeInbound identityKP rawConn = do
  let rawIO = rcStreamIO rawConn

  -- Step 1: multistream-select → "/noise"
  Accepted _ <- negotiateResponder rawIO ["/noise"]

  -- Step 2: Noise XX handshake (responder)
  (noiseSess, HandshakeResult remotePeerId _remotePK) <-
    performStreamHandshake identityKP Inbound rawIO

  -- Step 3: Create encrypted StreamIO
  sendRef <- newIORef noiseSess
  recvRef <- newIORef noiseSess
  bufRef  <- newIORef BS.empty
  let encryptedIO = noiseSessionToStreamIO sendRef recvRef bufRef rawIO

  -- Step 4: multistream-select → "/yamux/1.0.0" (over encrypted channel)
  Accepted _ <- negotiateResponder encryptedIO ["/yamux/1.0.0"]

  -- Step 5: Initialize Yamux session (server = even IDs)
  let yamuxWrite = streamWrite encryptedIO
      yamuxRead  = \n -> readExact encryptedIO n
  yamuxSess <- newSession RoleServer yamuxWrite yamuxRead
  muxer <- yamuxToMuxerSession yamuxSess

  -- Build Connection
  stateVar <- newTVarIO ConnOpen
  pure Connection
    { connPeerId     = remotePeerId
    , connDirection  = Inbound
    , connLocalAddr  = rcLocalAddr rawConn
    , connRemoteAddr = rcRemoteAddr rawConn
    , connSecurity   = "/noise"
    , connMuxer      = "/yamux/1.0.0"
    , connSession    = muxer
    , connState      = stateVar
    }
