-- | Inbound pubsub message validation (specs/pubsub/README.md).
--
-- Under @StrictSign@ the consuming side must enforce that @from@, @seqno@ and
-- @signature@ are present, that the signature verifies against the author's
-- public key, and that the key matches the @from@ peer ID. Messages that fail
-- are dropped without propagation. Under @StrictNoSign@ the signing fields must
-- be absent.
--
-- The signed bytes are @"libp2p-pubsub:" <> protobuf(msg without signature and
-- key)@, symmetric with the signing path in "LibP2P.Protocol.GossipSub.Router".
module LibP2P.Protocol.GossipSub.Validation
  ( ValidationError (..)
  , validateMessage
  , signaturePrefix
  , marshalForSigning
  , signingBytes
  ) where

import Prelude
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import LibP2P.Core.Multihash (HashFunction (..), validateMultihash)
import LibP2P.Crypto.Key (PublicKey, verify)
import LibP2P.Crypto.PeerId (PeerId (..), fromPublicKey)
import LibP2P.Crypto.Protobuf (decodePublicKey)
import LibP2P.Protocol.GossipSub.Message (encodePubSubMessageBS)
import LibP2P.Protocol.GossipSub.Types

-- | Why an inbound message was rejected.
data ValidationError
  = MissingField String       -- ^ A field required by StrictSign is absent
  | UnexpectedField String    -- ^ A signing field is present under StrictNoSign
  | MalformedKey String       -- ^ The key field (or inlined key) failed to decode
  | KeyPeerIdMismatch         -- ^ The public key does not derive the @from@ peer ID
  | BadSignature              -- ^ Signature verification failed
  deriving (Show, Eq)

-- | Domain separation prefix for pubsub message signatures.
signaturePrefix :: ByteString
signaturePrefix = "libp2p-pubsub:"

-- | Marshal a message for signature computation.
-- Per the libp2p spec, the signed data excludes both signature and key fields.
marshalForSigning :: PubSubMessage -> ByteString
marshalForSigning msg = encodePubSubMessageBS
  (msg { msgSignature = Nothing, msgKey = Nothing })

-- | The exact bytes covered by a message signature.
signingBytes :: PubSubMessage -> ByteString
signingBytes msg = signaturePrefix <> marshalForSigning msg

-- | Validate an inbound message against the configured signature policy.
validateMessage :: SignaturePolicy -> PubSubMessage -> Either ValidationError ()
validateMessage StrictSign msg = do
  from <- required "from" (msgFrom msg)
  _    <- required "seqno" (msgSeqNo msg)
  sig  <- required "signature" (msgSignature msg)
  pk   <- authorKey from (msgKey msg)
  let PeerId derived = fromPublicKey pk
  unless (derived == from) (Left KeyPeerIdMismatch)
  unless (verify pk (signingBytes msg) sig) (Left BadSignature)
validateMessage StrictNoSign msg = do
  forbid "signature" (msgSignature msg)
  forbid "key" (msgKey msg)
  forbid "from" (msgFrom msg)
  forbid "seqno" (msgSeqNo msg)

-- | Resolve the author's public key: from the explicit key field when present,
-- otherwise from the identity multihash inlined in the @from@ peer ID.
-- Implementations omit the key field whenever the peer ID inlines it (small
-- keys such as Ed25519), so both forms must be accepted.
authorKey :: ByteString -> Maybe ByteString -> Either ValidationError PublicKey
authorKey from Nothing =
  case validateMultihash from of
    Left err -> Left (MalformedKey ("from is not a valid multihash: " <> err))
    Right (SHA256, _) -> Left (MissingField "key")
    Right (Identity, inlined) -> decodeKey inlined
authorKey _ (Just keyBytes) = decodeKey keyBytes

decodeKey :: ByteString -> Either ValidationError PublicKey
decodeKey bs = either (Left . MalformedKey) Right (decodePublicKey bs)

required :: String -> Maybe a -> Either ValidationError a
required name = maybe (Left (MissingField name)) Right

forbid :: String -> Maybe a -> Either ValidationError ()
forbid name v = when (maybe False (const True) v) (Left (UnexpectedField name))
