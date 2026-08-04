-- | Peer routing records (RFC 0003).
--
-- A PeerRecord is a self-certified statement of a peer's dialable
-- addresses, distributed inside a "LibP2P.Crypto.SignedEnvelope"
-- (RFC 0002). Consumers verify the envelope signature and that the
-- signing key derives the peer id named in the record, giving
-- third-party-relayable address information that cannot be forged.
--
-- Wire format (go-libp2p @core/peer/pb/peer_record.proto@):
--
-- > message PeerRecord {
-- >   message AddressInfo { bytes multiaddr = 1; }
-- >   bytes peer_id = 1;
-- >   uint64 seq = 2;
-- >   repeated AddressInfo addresses = 3;
-- > }
--
-- Envelope parameters (go-libp2p @core/peer/record.go@ — note the RFC
-- 0003 draft text uses different strings; the deployed go-libp2p values
-- are authoritative for interop):
--
--   * domain: @libp2p-peer-record@
--   * payload type: the raw multicodec bytes @0x03 0x01@
--     (multicodec table name @libp2p-peer-record@)
module LibP2P.Crypto.PeerRecord
  ( -- * Record type
    PeerRecord (..)
  , timestampSeq
    -- * Protobuf codec
  , encodePeerRecord
  , decodePeerRecord
    -- * Envelope integration
  , peerRecordEnvelopeDomain
  , peerRecordEnvelopePayloadType
  , sealPeerRecord
  , openPeerRecordEnvelope
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import LibP2P.Crypto.Key (KeyPair (..))
import LibP2P.Crypto.PeerId (fromPublicKey, peerIdBytes)
import LibP2P.Crypto.SignedEnvelope
  ( SignedEnvelope (..)
  , createEnvelope
  , decodeSignedEnvelope
  , verifyEnvelope
  )
import qualified Proto3.Wire.Decode as Decode
import Proto3.Wire.Decode (Parser, RawMessage, at, embedded', one, parse, repeated)
import qualified Proto3.Wire.Encode as Encode
import Proto3.Wire.Types (FieldNumber (..))
import System.IO.Unsafe (unsafePerformIO)

-- | A routing record: which addresses a peer claims to be reachable at.
data PeerRecord = PeerRecord
  { prPeerId    :: !ByteString   -- ^ Peer id bytes (multihash of the public key)
  , prSeq       :: !Word64       -- ^ Monotonic sequence number (see 'timestampSeq')
  , prAddresses :: ![ByteString] -- ^ Binary multiaddrs (one per wrapped AddressInfo)
  } deriving (Show, Eq)

-- | Domain separation string for peer-record envelopes
-- (go-libp2p @PeerRecordEnvelopeDomain@).
peerRecordEnvelopeDomain :: ByteString
peerRecordEnvelopeDomain = "libp2p-peer-record"

-- | Envelope payload type: the @libp2p-peer-record@ multicodec bytes
-- (go-libp2p @PeerRecordEnvelopePayloadType@).
peerRecordEnvelopePayloadType :: ByteString
peerRecordEnvelopePayloadType = BS.pack [0x03, 0x01]

{-# NOINLINE lastSeqRef #-}
lastSeqRef :: IORef Word64
lastSeqRef = unsafePerformIO (newIORef 0)

-- | A timestamp-based, strictly monotonic sequence number: the current
-- Unix time in nanoseconds, bumped past the previous value if the clock
-- has not advanced (mirrors go-libp2p @peer.TimestampSeq@).
timestampSeq :: IO Word64
timestampSeq = do
  now <- floor . (* 1e9) . toRational <$> getPOSIXTime
  atomicModifyIORef' lastSeqRef $ \prev ->
    let next = max now (prev + 1) in (next, next)

-- | Encode a PeerRecord to protobuf wire format. Proto3 default values
-- (empty peer_id, zero seq) are omitted, matching go-libp2p's encoder.
encodePeerRecord :: PeerRecord -> ByteString
encodePeerRecord pr = BL.toStrict $ Encode.toLazyByteString $
     (if BS.null (prPeerId pr)
        then mempty
        else Encode.byteString (FieldNumber 1) (prPeerId pr))
  <> (if prSeq pr == 0
        then mempty
        else Encode.uint64 (FieldNumber 2) (prSeq pr))
  <> foldMap
       (Encode.embedded (FieldNumber 3) . Encode.byteString (FieldNumber 1))
       (prAddresses pr)

-- | Decode a PeerRecord from protobuf wire format.
decodePeerRecord :: ByteString -> Either String PeerRecord
decodePeerRecord bs = case parse peerRecordParser bs of
  Left err     -> Left $ "PeerRecord decode error: " ++ show err
  Right record -> Right record

peerRecordParser :: Parser RawMessage PeerRecord
peerRecordParser = PeerRecord
  <$> at (one Decode.byteString BS.empty) (FieldNumber 1)
  <*> at (one Decode.uint64 0) (FieldNumber 2)
  <*> at (repeated (embedded' addressInfoParser)) (FieldNumber 3)

addressInfoParser :: Parser RawMessage ByteString
addressInfoParser = at (one Decode.byteString BS.empty) (FieldNumber 1)

-- | Seal a PeerRecord into a SignedEnvelope with the given identity key.
-- The caller is responsible for the record's peer id matching the key
-- (a mismatched record still seals, but no verifier will accept it).
sealPeerRecord :: KeyPair -> PeerRecord -> Either String SignedEnvelope
sealPeerRecord kp record =
  createEnvelope (kpPrivate kp) (kpPublic kp)
    peerRecordEnvelopeDomain peerRecordEnvelopePayloadType
    (encodePeerRecord record)

-- | Open an encoded peer-record envelope: decode it, check the payload
-- type, verify the signature under the peer-record domain, decode the
-- record, and require that the envelope's key derives the peer id the
-- record claims. Any failure rejects the envelope.
openPeerRecordEnvelope :: ByteString -> Either String (SignedEnvelope, PeerRecord)
openPeerRecordEnvelope bs = do
  env <- decodeSignedEnvelope bs
  unless (sePayloadType env == peerRecordEnvelopePayloadType) $
    Left "peer record envelope: unexpected payload type"
  unless (verifyEnvelope env peerRecordEnvelopeDomain) $
    Left "peer record envelope: invalid signature"
  record <- decodePeerRecord (sePayload env)
  unless (peerIdBytes (fromPublicKey (sePublicKey env)) == prPeerId record) $
    Left "peer record envelope: key does not derive the record's peer id"
  Right (env, record)
