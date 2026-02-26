-- | Post-handshake Noise transport session.
--
-- After the XX handshake completes, cacophony's NoiseState transitions
-- to transport mode with two CipherStates (one for each direction).
-- This module wraps that state for encrypted message exchange.
module Network.LibP2P.Security.Noise.Session
  ( NoiseSession
  , mkNoiseSession
  , encryptMessage
  , decryptMessage
  ) where

import Crypto.Noise
  ( NoiseResult (..)
  , NoiseState
  , convert
  , readMessage
  , writeMessage
  )
import Crypto.Noise.Cipher.ChaChaPoly1305 (ChaChaPoly1305)
import Crypto.Noise.DH.Curve25519 (Curve25519)
import Crypto.Noise.Hash.SHA256 (SHA256)
import Data.ByteArray (ScrubbedBytes)
import Data.ByteString (ByteString)

-- | Type alias for the Noise state with our fixed cipher suite.
type CacophonyState = NoiseState ChaChaPoly1305 Curve25519 SHA256

-- | A post-handshake transport session for encrypted communication.
newtype NoiseSession = NoiseSession { nsState :: CacophonyState }

-- | Create a NoiseSession from a completed handshake state.
mkNoiseSession :: CacophonyState -> NoiseSession
mkNoiseSession = NoiseSession

-- | Encrypt a plaintext message for sending.
-- Returns (ciphertext, updatedSession).
encryptMessage :: NoiseSession -> ByteString -> Either String (ByteString, NoiseSession)
encryptMessage (NoiseSession ns) plaintext =
  let sb = convert plaintext :: ScrubbedBytes
   in case writeMessage sb ns of
        NoiseResultMessage ct ns' ->
          Right (convert ct, NoiseSession ns')
        NoiseResultException ex ->
          Left $ "encryptMessage: " <> show ex
        NoiseResultNeedPSK _ ->
          Left "encryptMessage: unexpected PSK request"

-- | Decrypt a received ciphertext message.
-- Returns (plaintext, updatedSession).
decryptMessage :: NoiseSession -> ByteString -> Either String (ByteString, NoiseSession)
decryptMessage (NoiseSession ns) ciphertext =
  let sb = convert ciphertext :: ScrubbedBytes
   in case readMessage sb ns of
        NoiseResultMessage pt ns' ->
          Right (convert pt, NoiseSession ns')
        NoiseResultException ex ->
          Left $ "decryptMessage: " <> show ex
        NoiseResultNeedPSK _ ->
          Left "decryptMessage: unexpected PSK request"
