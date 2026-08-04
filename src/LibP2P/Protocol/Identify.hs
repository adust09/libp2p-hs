-- | Identify protocol implementation (specs/identify).
--
-- Protocol ID: /ipfs/id/1.0.0
--
-- After a connection is established, both sides exchange IdentifyInfo
-- messages to learn about each other's capabilities, listen addresses,
-- and agent version. Like all libp2p protobuf streams, the message is
-- varint-length-delimited on the wire: uvarint(len) ++ protobuf. This
-- matches the delimited reader/writer used by go-libp2p (pbio),
-- rust-libp2p, and js-libp2p.
--
-- Also implements Identify Push (/ipfs/id/push/1.0.0) for proactive
-- updates when local state changes.
module LibP2P.Protocol.Identify
  ( -- * Protocol IDs
    identifyProtocolId
  , identifyPushProtocolId
    -- * Protocol logic
  , handleIdentify
  , requestIdentify
  , handleIdentifyPush
  , pushIdentify
  , mergeIdentify
    -- * Building local info
  , buildLocalIdentify
    -- * Registration
  , registerIdentifyHandlers
    -- * Wire framing
  , encodeFramedIdentify
  , readFramedIdentify
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Exception (SomeException, catch)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import LibP2P.Core.Varint (decodeUvarint, encodeUvarint)
import LibP2P.Crypto.Protobuf (encodePublicKey)
import LibP2P.Crypto.Key (kpPublic)
import LibP2P.Multiaddr.Codec (encodeProtocols)
import LibP2P.Multiaddr (Multiaddr (..))
import LibP2P.MultistreamSelect.Negotiation
  ( ProtocolId
  , StreamIO (..)
  , negotiateInitiator
  , NegotiationResult (..)
  )
import LibP2P.Protocol.Identify.Message
  ( IdentifyInfo (..)
  , decodeIdentify
  , encodeIdentify
  , maxIdentifySize
  )
import LibP2P.Switch.ConnPool (allConns)
import LibP2P.Switch.Types
  ( ActiveListener (..)
  , Connection (..)
  , MuxerSession (..)
  , Switch (..)
  )

-- | Identify protocol ID.
identifyProtocolId :: ProtocolId
identifyProtocolId = "/ipfs/id/1.0.0"

-- | Identify Push protocol ID.
identifyPushProtocolId :: ProtocolId
identifyPushProtocolId = "/ipfs/id/push/1.0.0"

-- | Handle an inbound Identify request (responder side).
--
-- Sends our local IdentifyInfo as a varint-length-prefixed protobuf,
-- then closes the stream (per specs/identify: respond and close).
-- The connection provides the remote address used for observedAddr.
handleIdentify :: Switch -> Connection -> StreamIO -> IO ()
handleIdentify sw conn stream = do
  info <- buildLocalIdentify sw (Just conn)
  streamWrite stream (encodeFramedIdentify info)
  streamClose stream

-- | Request Identify from a remote peer (initiator side).
--
-- Opens a new stream, negotiates /ipfs/id/1.0.0, then reads one
-- varint-length-prefixed protobuf message.
requestIdentify :: Connection -> IO (Either String IdentifyInfo)
requestIdentify conn = do
  stream <- muxOpenStream (connSession conn)
  result <- negotiateInitiator stream [identifyProtocolId]
  case result of
    Accepted _ -> readFramedIdentify stream maxIdentifySize
    NoProtocol -> pure (Left "remote does not support identify")

-- | Handle an inbound Identify Push (responder side).
--
-- Reads the pushed varint-length-prefixed IdentifyInfo from the remote
-- peer. The length prefix is the message boundary — identify push has
-- no stream-close boundary to fall back on.
--
-- The pushed info is merged into the existing peer entry via
-- 'mergeIdentify': pushes may be partial updates, so fields absent
-- from the message must not erase what we already know.
handleIdentifyPush :: Switch -> Connection -> StreamIO -> IO ()
handleIdentifyPush sw conn stream = do
  infoOrErr <- readFramedIdentify stream maxIdentifySize
  case infoOrErr of
    Left _ -> pure ()
    Right info -> atomically $ do
      store <- readTVar (swPeerStore sw)
      let merged = maybe info (`mergeIdentify` info)
                     (Map.lookup (connPeerId conn) store)
      writeTVar (swPeerStore sw) (Map.insert (connPeerId conn) merged store)

-- | Merge a received (possibly partial) Identify update into the
-- previously known info for a peer.
--
-- Per specs/identify: "missing fields should be ignored, as peers may
-- choose to send partial updates containing only the fields whose
-- values have changed." Optional fields keep the known value when the
-- update omits them; repeated fields (protobuf cannot distinguish
-- absent from empty) keep the known list when the update's is empty
-- and are replaced wholesale otherwise, matching go-libp2p.
mergeIdentify :: IdentifyInfo -> IdentifyInfo -> IdentifyInfo
mergeIdentify known update = IdentifyInfo
  { idProtocolVersion = idProtocolVersion update <|> idProtocolVersion known
  , idAgentVersion    = idAgentVersion update <|> idAgentVersion known
  , idPublicKey       = idPublicKey update <|> idPublicKey known
  , idListenAddrs     = replaceUnlessEmpty (idListenAddrs known) (idListenAddrs update)
  , idObservedAddr    = idObservedAddr update <|> idObservedAddr known
  , idProtocols       = replaceUnlessEmpty (idProtocols known) (idProtocols update)
  }
  where
    replaceUnlessEmpty old [] = old
    replaceUnlessEmpty _ new  = new

-- | Push our current IdentifyInfo to every connected peer (sender side
-- of /ipfs/id/push/1.0.0).
--
-- Per specs/identify: open a stream to each remote peer, negotiate the
-- push protocol id, send one Identify message and close the stream.
-- Call this whenever local state advertised via identify changes
-- (listen addresses, registered protocols). Failures on individual
-- peers (e.g. push protocol not supported) are ignored.
pushIdentify :: Switch -> IO ()
pushIdentify sw = do
  conns <- atomically $ allConns (swConnPool sw)
  mapM_ (\conn -> pushToConn conn `catch` \(_ :: SomeException) -> pure ()) conns
  where
    pushToConn conn = do
      stream <- muxOpenStream (connSession conn)
      result <- negotiateInitiator stream [identifyPushProtocolId]
      case result of
        Accepted _ -> do
          info <- buildLocalIdentify sw (Just conn)
          streamWrite stream (encodeFramedIdentify info)
          streamClose stream
        NoProtocol -> streamClose stream

-- | Build our local IdentifyInfo from Switch state.
buildLocalIdentify :: Switch -> Maybe Connection -> IO IdentifyInfo
buildLocalIdentify sw mConn = do
  (protocols, listenAddrs) <- atomically $ do
    protos <- Map.keys <$> readTVar (swProtocols sw)
    listeners <- readTVar (swListeners sw)
    pure (protos, map alAddress listeners)
  pure IdentifyInfo
    { idProtocolVersion = Just "ipfs/0.1.0"
    , idAgentVersion    = Just "libp2p-hs/0.1.0"
    , idPublicKey       = Just (encodePublicKey (kpPublic (swIdentityKey sw)))
    , idListenAddrs     = map (\(Multiaddr ps) -> encodeProtocols ps) listenAddrs
    , idObservedAddr    = (\(Multiaddr ps) -> encodeProtocols ps) . connRemoteAddr <$> mConn
    , idProtocols       = protocols
    }

-- | Register Identify protocol handlers on the Switch.
--
-- Registers:
--   /ipfs/id/1.0.0      — respond to Identify requests
--   /ipfs/id/push/1.0.0 — handle Identify Push from remote
registerIdentifyHandlers :: Switch -> IO ()
registerIdentifyHandlers sw = do
  atomically $ do
    protos <- readTVar (swProtocols sw)
    let protos' = Map.insert identifyProtocolId (handleIdentify sw) protos
        protos'' = Map.insert identifyPushProtocolId (handleIdentifyPush sw) protos'
    writeTVar (swProtocols sw) protos''

-- | Encode an IdentifyInfo with its uvarint length prefix, as written
-- on the wire: uvarint(len) ++ protobuf.
encodeFramedIdentify :: IdentifyInfo -> BS.ByteString
encodeFramedIdentify info =
  let payload = encodeIdentify info
  in encodeUvarint (fromIntegral (BS.length payload)) <> payload

-- | Read one varint-length-prefixed Identify message from a stream.
--
-- Reads the uvarint length prefix, then exactly that many payload
-- bytes, and decodes the protobuf. Rejects messages larger than
-- maxSize before reading the payload.
readFramedIdentify :: StreamIO -> Int -> IO (Either String IdentifyInfo)
readFramedIdentify stream maxSize = readFramed `catch` onError
  where
    onError :: SomeException -> IO (Either String IdentifyInfo)
    onError e = pure (Left ("identify stream read failed: " ++ show e))

    readFramed = do
      varintBytes <- readVarintBytes stream
      case decodeUvarint varintBytes of
        Left err -> pure (Left ("identify length prefix decode error: " ++ err))
        Right (len, _) -> do
          let msgLen = fromIntegral len :: Int
          if msgLen > maxSize
            then pure (Left ("identify message too large: "
                             ++ show msgLen ++ " > " ++ show maxSize))
            else do
              payload <- readExact stream msgLen
              case decodeIdentify payload of
                Left parseErr ->
                  pure (Left ("identify protobuf decode error: " ++ show parseErr))
                Right info -> pure (Right info)

-- | Read the bytes of one unsigned varint from a stream (up to 10 bytes).
readVarintBytes :: StreamIO -> IO BS.ByteString
readVarintBytes stream = go [] (0 :: Int)
  where
    go acc n
      | n >= 10 = pure (BS.pack (reverse acc))  -- max varint length
      | otherwise = do
          b <- streamReadByte stream
          if b < 0x80
            then pure (BS.pack (reverse (b : acc)))
            else go (b : acc) (n + 1)

-- | Read exactly n bytes from a stream.
readExact :: StreamIO -> Int -> IO BS.ByteString
readExact stream n = BS.pack <$> mapM (const (streamReadByte stream)) [1 .. n]
