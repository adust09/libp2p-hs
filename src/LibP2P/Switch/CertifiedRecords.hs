{-# LANGUAGE ScopedTypeVariables #-}

-- | Certified peer records: the signed routing records a node has
-- accepted, and the freshness rule that governs replacing them
-- (specs/RFC/0003-routing-records.md).
--
-- RFC 0003, Peer Store APIs:
--
-- > a receiving peer MUST keep track of the latest @seq@ value received
-- > for each peer and reject incoming records unless they contain a
-- > greater @seq@ value than the last received.
--
-- Verifying an envelope's signature says only that the peer signed it at
-- some point, not that it is current. Without the sequence check a
-- correctly signed but older record replayed at a peer rolls its
-- certified addresses back to stale state, which is exactly what signing
-- them was supposed to prevent.
--
-- The state lives here rather than in 'LibP2P.Protocol.Identify.IdentifyInfo'
-- because a record does not only arrive over Identify: Identify Push and
-- GossipSub peer exchange carry the same envelopes and must be held to
-- the same rule, and none of them is an Identify message.
module LibP2P.Switch.CertifiedRecords
  ( -- * Types
    CertifiedRecord (..)
    -- * Verification
  , verifyPeerRecord
    -- * Freshness
  , consumeCertifiedRecord
  , lookupCertifiedRecord
  ) where

import Control.Concurrent.STM (STM, TVar, modifyTVar', readTVar)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import LibP2P.Crypto.PeerRecord (PeerRecord (..), openPeerRecordEnvelope)
import LibP2P.Crypto.SignedEnvelope (SignedEnvelope (..))

-- | A signed peer record this node has verified and accepted, kept
-- alongside the envelope it came in so it can be forwarded verbatim
-- (a record is only self-certifying while its signature travels with it).
data CertifiedRecord = CertifiedRecord
  { crSeq       :: !Word64       -- ^ Sequence number of the accepted record
  , crEnvelope  :: !ByteString   -- ^ The encoded envelope, exactly as received
  , crAddresses :: ![ByteString] -- ^ Certified binary multiaddrs
  } deriving (Show, Eq)

-- | Verify an envelope against an authenticated peer id.
--
-- The envelope must open (valid signature, expected domain and payload
-- type) and its signing key must derive the peer id the security
-- handshake authenticated — otherwise the sender is making a claim about
-- an identity it does not hold.
--
-- This says nothing about whether the record is current; that is
-- 'consumeCertifiedRecord'.
verifyPeerRecord :: PeerId -> ByteString -> Either String CertifiedRecord
verifyPeerRecord peer envBytes = do
  (env, record) <- openPeerRecordEnvelope envBytes
  if fromPublicKey (sePublicKey env) == peer
    then Right CertifiedRecord
      { crSeq       = prSeq record
      , crEnvelope  = envBytes
      , crAddresses = prAddresses record
      }
    else Left "signed peer record was not signed by the authenticated peer"

-- | Apply the RFC 0003 freshness rule and, when the record wins, retain
-- it. Returns whether the record was accepted.
--
-- A first record for a peer is always accepted. After that only a
-- strictly greater @seq@ is, so an equal or lower one leaves the retained
-- record untouched — replaying a record we already hold changes nothing.
--
-- Note that go-libp2p is more permissive here: @pstoremem@ rejects only
-- @lastState.Seq > rec.Seq@, accepting an equal sequence number as a TTL
-- refresh for its address book. This implementation has no address TTL
-- for such a refresh to renew, so it follows the RFC's wording instead.
consumeCertifiedRecord
  :: TVar (Map PeerId CertifiedRecord)
  -> PeerId
  -> CertifiedRecord
  -> STM Bool
consumeCertifiedRecord recordsVar peer record = do
  known <- Map.lookup peer <$> readTVar recordsVar
  let fresher = maybe True ((crSeq record >) . crSeq) known
  if fresher
    then do
      modifyTVar' recordsVar (Map.insert peer record)
      pure True
    else pure False

-- | The record currently retained for a peer, if any.
lookupCertifiedRecord
  :: TVar (Map PeerId CertifiedRecord)
  -> PeerId
  -> STM (Maybe CertifiedRecord)
lookupCertifiedRecord recordsVar peer = Map.lookup peer <$> readTVar recordsVar
