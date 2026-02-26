-- | Noise XX handshake for libp2p secure channels.
--
-- Implements the Noise_XX_25519_ChaChaPoly_SHA256 handshake pattern
-- with libp2p-specific payload injection (identity key + signature).
--
-- Uses cacophony for the core Noise protocol state machine.
module Network.LibP2P.Security.Noise.Handshake
  ( -- * Handshake types
    HandshakeResult (..)
  , NoisePayload (..)
  , HandshakeState
    -- * Payload encoding
  , encodeNoisePayload
  , decodeNoisePayload
  , buildHandshakePayload
  , validateHandshakePayload
    -- * Static key signing
  , signStaticKey
  , verifyStaticKey
    -- * Handshake lifecycle
  , initHandshakeInitiator
  , initHandshakeResponder
  , writeHandshakeMsg
  , readHandshakeMsg
  , sessionComplete
    -- * Convenience
  , performFullHandshake
  , performFullHandshakeWithSessions
    -- * Re-exports for payload decoding
  , decodePublicKey
  ) where

import Crypto.Noise
  ( HandshakeRole (..)
  , NoiseResult (..)
  , NoiseState
  , convert
  , defaultHandshakeOpts
  , handshakeComplete
  , noiseState
  , readMessage
  , setLocalEphemeral
  , setLocalStatic
  , writeMessage
  )
import Crypto.Noise.Cipher.ChaChaPoly1305 (ChaChaPoly1305)
import Crypto.Noise.DH (dhGenKey, dhPubToBytes)
import qualified Crypto.Noise.DH as DH
import Crypto.Noise.DH.Curve25519 (Curve25519)
import Crypto.Noise.HandshakePatterns (noiseXX)
import Crypto.Noise.Hash.SHA256 (SHA256)
import Data.ByteArray (ScrubbedBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Network.LibP2P.Core.Varint (decodeUvarint, encodeUvarint)
import Network.LibP2P.Crypto.Key
  ( KeyPair (..)
  , PrivateKey (..)
  , PublicKey (..)
  , verify
  )
import qualified Network.LibP2P.Crypto.Key as Key
import Network.LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import Network.LibP2P.Crypto.Protobuf (decodePublicKey, encodePublicKey)
import Network.LibP2P.Security.Noise.Session (NoiseSession, mkNoiseSession)

-- | Type alias for the Noise state with our fixed cipher suite.
type CacophonyState = NoiseState ChaChaPoly1305 Curve25519 SHA256

-- | Opaque handshake state wrapping cacophony's NoiseState.
newtype HandshakeState = HandshakeState
  { hsNoiseState :: CacophonyState
  }

-- | Result of a successful Noise handshake.
data HandshakeResult = HandshakeResult
  { hrRemotePeerId :: !PeerId
  , hrRemotePublicKey :: !PublicKey
  }
  deriving (Show, Eq)

-- | Noise handshake payload (protobuf-encoded in messages 2 and 3).
data NoisePayload = NoisePayload
  { npIdentityKey :: !ByteString -- ^ Serialized PublicKey protobuf
  , npIdentitySig :: !ByteString -- ^ Signature over "noise-libp2p-static-key:" || static_pubkey
  }
  deriving (Show, Eq)

-- | The prefix for the signed data in Noise handshake.
noiseStaticKeyPrefix :: ByteString
noiseStaticKeyPrefix = "noise-libp2p-static-key:"

-- | Sign the Noise static public key with the identity private key.
signStaticKey :: PrivateKey -> ByteString -> Either String ByteString
signStaticKey sk noiseStaticPubKey =
  let payload = noiseStaticKeyPrefix <> noiseStaticPubKey
   in Key.sign sk payload

-- | Verify a signature over the Noise static public key.
verifyStaticKey :: PublicKey -> ByteString -> ByteString -> Bool
verifyStaticKey pk noiseStaticPubKey sig =
  let payload = noiseStaticKeyPrefix <> noiseStaticPubKey
   in verify pk payload sig

-- | Build a handshake payload from an identity key pair and Noise static pubkey.
buildHandshakePayload :: Key.KeyPair -> ByteString -> NoisePayload
buildHandshakePayload identityKP noiseStaticPub =
  let identKey = encodePublicKey (kpPublic identityKP)
      identSig = case signStaticKey (kpPrivate identityKP) noiseStaticPub of
        Right s -> s
        Left err -> error $ "buildHandshakePayload: " <> err
   in NoisePayload identKey identSig

-- | Validate a handshake payload (decode identity key and check structure).
-- Does NOT verify the signature (caller must provide the remote Noise static key).
validateHandshakePayload :: NoisePayload -> Either String PublicKey
validateHandshakePayload np = decodePublicKey (npIdentityKey np)

-- | Encode a NoisePayload as a minimal protobuf message.
encodeNoisePayload :: NoisePayload -> ByteString
encodeNoisePayload (NoisePayload identKey identSig) =
  -- Field 1: tag 0x0a (field 1, wire type 2 = length-delimited)
  BS.singleton 0x0a
    <> encodeUvarint (fromIntegral (BS.length identKey))
    <> identKey
    -- Field 2: tag 0x12 (field 2, wire type 2 = length-delimited)
    <> BS.singleton 0x12
    <> encodeUvarint (fromIntegral (BS.length identSig))
    <> identSig

-- | Decode a NoisePayload from protobuf bytes.
decodeNoisePayload :: ByteString -> Either String NoisePayload
decodeNoisePayload bs = do
  (identKey, rest1) <- decodeField 0x0a bs
  (identSig, _rest2) <- decodeField 0x12 rest1
  Right (NoisePayload identKey identSig)
  where
    decodeField :: Word8 -> ByteString -> Either String (ByteString, ByteString)
    decodeField expectedTag input
      | BS.null input = Left "decodeNoisePayload: unexpected end of input"
      | BS.head input /= expectedTag =
          Left $ "decodeNoisePayload: expected tag " <> show expectedTag <> " got " <> show (BS.head input)
      | otherwise = do
          let rest = BS.tail input
          (len, rest2) <- decodeUvarint rest
          let fieldLen = fromIntegral len :: Int
          if BS.length rest2 < fieldLen
            then Left "decodeNoisePayload: not enough bytes for field"
            else Right (BS.take fieldLen rest2, BS.drop fieldLen rest2)

-- | Initialize a handshake state for the initiator role.
-- Returns (HandshakeState, noiseStaticPublicKey).
initHandshakeInitiator :: Key.KeyPair -> IO (HandshakeState, ByteString)
initHandshakeInitiator _identityKP = do
  noiseStaticKP <- dhGenKey :: IO (DH.KeyPair Curve25519)
  noiseEphemeralKP <- dhGenKey :: IO (DH.KeyPair Curve25519)
  let noiseStaticPub = convert (dhPubToBytes (snd noiseStaticKP)) :: ByteString
  let opts = setLocalStatic (Just noiseStaticKP)
           . setLocalEphemeral (Just noiseEphemeralKP)
           $ defaultHandshakeOpts InitiatorRole ""
  let ns = noiseState opts noiseXX :: CacophonyState
  pure (HandshakeState ns, noiseStaticPub)

-- | Initialize a handshake state for the responder role.
-- Returns (HandshakeState, noiseStaticPublicKey).
initHandshakeResponder :: Key.KeyPair -> IO (HandshakeState, ByteString)
initHandshakeResponder _identityKP = do
  noiseStaticKP <- dhGenKey :: IO (DH.KeyPair Curve25519)
  noiseEphemeralKP <- dhGenKey :: IO (DH.KeyPair Curve25519)
  let noiseStaticPub = convert (dhPubToBytes (snd noiseStaticKP)) :: ByteString
  let opts = setLocalStatic (Just noiseStaticKP)
           . setLocalEphemeral (Just noiseEphemeralKP)
           $ defaultHandshakeOpts ResponderRole ""
  let ns = noiseState opts noiseXX :: CacophonyState
  pure (HandshakeState ns, noiseStaticPub)

-- | Write a handshake message with the given payload.
-- Returns (ciphertext, updatedState).
writeHandshakeMsg :: HandshakeState -> ByteString -> Either String (ByteString, HandshakeState)
writeHandshakeMsg hs payload =
  let sb = convert payload :: ScrubbedBytes
   in case writeMessage sb (hsNoiseState hs) of
        NoiseResultMessage ct ns' ->
          Right (convert ct, HandshakeState ns')
        NoiseResultException ex ->
          Left $ "writeHandshakeMsg: " <> show ex
        NoiseResultNeedPSK _ ->
          Left "writeHandshakeMsg: unexpected PSK request"

-- | Read a handshake message and extract the decrypted payload.
-- Returns (plaintext, updatedState).
readHandshakeMsg :: HandshakeState -> ByteString -> Either String (ByteString, HandshakeState)
readHandshakeMsg hs ciphertext =
  let sb = convert ciphertext :: ScrubbedBytes
   in case readMessage sb (hsNoiseState hs) of
        NoiseResultMessage pt ns' ->
          Right (convert pt, HandshakeState ns')
        NoiseResultException ex ->
          Left $ "readHandshakeMsg: " <> show ex
        NoiseResultNeedPSK _ ->
          Left "readHandshakeMsg: unexpected PSK request"

-- | Check whether the handshake is complete.
sessionComplete :: HandshakeState -> Bool
sessionComplete = handshakeComplete . hsNoiseState

-- | Perform a full 3-message XX handshake between two peers.
-- Returns the remote PeerId as seen by each side.
performFullHandshake :: Key.KeyPair -> Key.KeyPair -> IO (Either String (PeerId, PeerId))
performFullHandshake aliceIdentity bobIdentity = do
  (aliceInit, aliceNoiseStaticPub) <- initHandshakeInitiator aliceIdentity
  (bobInit, bobNoiseStaticPub) <- initHandshakeResponder bobIdentity
  pure $ do
    -- Message 1: Alice → Bob (empty payload)
    (msg1, aliceState1) <- writeHandshakeMsg aliceInit BS.empty
    (_payload1, bobState1) <- readHandshakeMsg bobInit msg1

    -- Message 2: Bob → Alice (Bob's identity payload)
    let bobPayload = encodeNoisePayload $ buildHandshakePayload bobIdentity bobNoiseStaticPub
    (msg2, bobState2) <- writeHandshakeMsg bobState1 bobPayload
    (payload2, aliceState2) <- readHandshakeMsg aliceState1 msg2

    -- Decode Bob's identity
    bobNP <- decodeNoisePayload payload2
    bobPubKey <- decodePublicKey (npIdentityKey bobNP)
    let bobRemotePeerId = fromPublicKey bobPubKey

    -- Message 3: Alice → Bob (Alice's identity payload)
    let alicePayload = encodeNoisePayload $ buildHandshakePayload aliceIdentity aliceNoiseStaticPub
    (msg3, _aliceFinal) <- writeHandshakeMsg aliceState2 alicePayload
    (payload3, _bobFinal) <- readHandshakeMsg bobState2 msg3

    -- Decode Alice's identity
    aliceNP <- decodeNoisePayload payload3
    alicePubKey <- decodePublicKey (npIdentityKey aliceNP)
    let aliceRemotePeerId = fromPublicKey alicePubKey

    Right (bobRemotePeerId, aliceRemotePeerId)

-- | Perform a full handshake and return transport sessions for both sides.
performFullHandshakeWithSessions :: Key.KeyPair -> Key.KeyPair -> IO (Either String (NoiseSession, NoiseSession))
performFullHandshakeWithSessions aliceIdentity bobIdentity = do
  (aliceInit, aliceNoiseStaticPub) <- initHandshakeInitiator aliceIdentity
  (bobInit, bobNoiseStaticPub) <- initHandshakeResponder bobIdentity
  pure $ do
    -- Message 1: Alice → Bob (empty payload)
    (msg1, aliceState1) <- writeHandshakeMsg aliceInit BS.empty
    (_payload1, bobState1) <- readHandshakeMsg bobInit msg1

    -- Message 2: Bob → Alice (Bob's identity payload)
    let bobPayload = encodeNoisePayload $ buildHandshakePayload bobIdentity bobNoiseStaticPub
    (msg2, bobState2) <- writeHandshakeMsg bobState1 bobPayload
    (_payload2, aliceState2) <- readHandshakeMsg aliceState1 msg2

    -- Message 3: Alice → Bob (Alice's identity payload)
    let alicePayload = encodeNoisePayload $ buildHandshakePayload aliceIdentity aliceNoiseStaticPub
    (msg3, aliceFinal) <- writeHandshakeMsg aliceState2 alicePayload
    (_payload3, bobFinal) <- readHandshakeMsg bobState2 msg3

    -- Convert to transport sessions
    Right (mkNoiseSession (hsNoiseState aliceFinal), mkNoiseSession (hsNoiseState bobFinal))
