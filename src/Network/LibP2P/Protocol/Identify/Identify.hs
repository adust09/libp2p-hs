-- | Identify protocol implementation (docs/07-protocols.md).
--
-- Protocol ID: /ipfs/id/1.0.0
--
-- After a connection is established, both sides exchange IdentifyInfo
-- messages to learn about each other's capabilities, listen addresses,
-- and agent version. The message has no length prefix — the boundary
-- is determined by stream closure.
--
-- Also implements Identify Push (/ipfs/id/push/1.0.0) for proactive
-- updates when local state changes.
module Network.LibP2P.Protocol.Identify.Identify
  ( -- * Protocol IDs
    identifyProtocolId
  , identifyPushProtocolId
    -- * Protocol logic
  , handleIdentify
  , requestIdentify
  , handleIdentifyPush
    -- * Building local info
  , buildLocalIdentify
    -- * Registration
  , registerIdentifyHandlers
    -- * Helpers
  , readUntilEOF
  ) where

import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Exception (SomeException, catch)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.Crypto.Protobuf (encodePublicKey)
import Network.LibP2P.Crypto.Key (kpPublic)
import Network.LibP2P.MultistreamSelect.Negotiation
  ( ProtocolId
  , StreamIO (..)
  , negotiateInitiator
  , NegotiationResult (..)
  )
import Network.LibP2P.Protocol.Identify.Message
  ( IdentifyInfo (..)
  , decodeIdentify
  , encodeIdentify
  , maxIdentifySize
  )
import Network.LibP2P.Switch.Types
  ( Connection (..)
  , MuxerSession (..)
  , Switch (..)
  , StreamHandler
  )

-- | Identify protocol ID.
identifyProtocolId :: ProtocolId
identifyProtocolId = "/ipfs/id/1.0.0"

-- | Identify Push protocol ID.
identifyPushProtocolId :: ProtocolId
identifyPushProtocolId = "/ipfs/id/push/1.0.0"

-- | Handle an inbound Identify request (responder side).
--
-- Sends our local IdentifyInfo as protobuf to the stream, then closes.
-- The remote side reads until EOF.
handleIdentify :: Switch -> StreamIO -> PeerId -> IO ()
handleIdentify sw stream _remotePeerId = do
  info <- buildLocalIdentify sw Nothing
  let encoded = encodeIdentify info
  streamWrite stream encoded
  -- Stream closure (by muxer) signals end of message

-- | Request Identify from a remote peer (initiator side).
--
-- Opens a new stream, negotiates /ipfs/id/1.0.0, reads until EOF,
-- then decodes the protobuf message.
requestIdentify :: Connection -> IO (Either String IdentifyInfo)
requestIdentify conn = do
  stream <- muxOpenStream (connSession conn)
  result <- negotiateInitiator stream [identifyProtocolId]
  case result of
    Accepted _ -> do
      bytesOrErr <- readUntilEOF stream maxIdentifySize
      case bytesOrErr of
        Left err -> pure (Left err)
        Right bs -> case decodeIdentify bs of
          Left parseErr -> pure (Left (show parseErr))
          Right info -> pure (Right info)
    NoProtocol -> pure (Left "remote does not support identify")

-- | Handle an inbound Identify Push (responder side).
--
-- Reads the pushed IdentifyInfo from the remote peer.
handleIdentifyPush :: Switch -> StreamIO -> PeerId -> IO ()
handleIdentifyPush sw stream remotePeerId = do
  bytesOrErr <- readUntilEOF stream maxIdentifySize
  case bytesOrErr of
    Left _ -> pure ()
    Right bs -> case decodeIdentify bs of
      Left _ -> pure ()
      Right info -> atomically $ do
        store <- readTVar (swPeerStore sw)
        writeTVar (swPeerStore sw) (Map.insert remotePeerId info store)

-- | Build our local IdentifyInfo from Switch state.
buildLocalIdentify :: Switch -> Maybe Connection -> IO IdentifyInfo
buildLocalIdentify sw _mConn = do
  protocols <- atomically $ Map.keys <$> readTVar (swProtocols sw)
  pure IdentifyInfo
    { idProtocolVersion = Just "ipfs/0.1.0"
    , idAgentVersion    = Just "libp2p-hs/0.1.0"
    , idPublicKey       = Just (encodePublicKey (kpPublic (swIdentityKey sw)))
    , idListenAddrs     = []  -- TODO: populate from transport listeners
    , idObservedAddr    = Nothing  -- TODO: populate from connection remote addr
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

-- | Read bytes from a StreamIO until EOF, up to a maximum size.
--
-- Identify uses stream closure as message boundary (no length prefix).
-- Accumulates bytes until streamReadByte throws (EOF/stream closed).
readUntilEOF :: StreamIO -> Int -> IO (Either String BS.ByteString)
readUntilEOF stream maxSize = go []  0
  where
    go acc size
      | size >= maxSize = pure (Left "message exceeds maximum size")
      | otherwise = do
          result <- (Right <$> streamReadByte stream) `catch`
                    (\(_ :: SomeException) -> pure (Left ()))
          case result of
            Left () -> pure (Right (BS.pack (reverse acc)))
            Right b -> go (b : acc) (size + 1)
