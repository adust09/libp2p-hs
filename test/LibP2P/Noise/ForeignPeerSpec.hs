-- | Foreign-peer tests for the production Noise XX handshake path.
--
-- Every other Noise test in this repository pairs two instances of the
-- production code with each other, which can only validate internal
-- consistency: both ends build their messages from the same functions, so
-- they can never disagree. These tests drive one endpoint BY HAND from the
-- low-level handshake primitives, so the remote side can deviate from
-- anything our own production path would send — in particular an
-- identity_sig computed over the wrong Noise static key (a replayed or
-- forged payload). This is the closest a single-implementation suite can
-- get to a foreign peer before the cross-implementation interop harness
-- (#129) lands.
module LibP2P.Noise.ForeignPeerSpec (spec) where

import Control.Concurrent.Async (concurrently, withAsync)
import Control.Exception (IOException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (KeyPair (..), PublicKey)
import LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..), mkMemoryStreamPair)
import LibP2P.Noise.Handshake
  ( HandshakeResult (..)
  , NoisePayload (..)
  , buildHandshakePayload
  , decodeNoisePayload
  , decodePublicKey
  , encodeNoisePayload
  , initHandshakeInitiator
  , initHandshakeResponder
  , readHandshakeMsg
  , writeHandshakeMsg
  )
import LibP2P.Switch.Types (Direction (..))
import LibP2P.Switch.Upgrade
  ( performStreamHandshake
  , readFramedMessage
  , writeFramedMessage
  )
import Test.Hspec

-- | Generate a test identity (PeerId, KeyPair).
mkTestIdentity :: IO (PeerId, KeyPair)
mkTestIdentity = do
  Right kp <- generateKeyPair
  pure (fromPublicKey (kpPublic kp), kp)

-- | A hand-rolled Noise XX responder driven from the low-level handshake
-- primitives. 'sigTarget' selects which bytes the identity signature
-- covers: 'id' signs the actual session static key (an honest peer);
-- @const other@ signs unrelated bytes (a forged or replayed payload).
-- Returns the initiator identity extracted from the msg3 payload.
runForeignResponder
  :: KeyPair -> (ByteString -> ByteString) -> StreamIO -> IO (PeerId, PublicKey)
runForeignResponder identityKP sigTarget stream = do
  (st0, staticPub) <- initHandshakeResponder identityKP

  -- Message 1: <- e (empty payload)
  msg1 <- readFramedMessage stream
  (_p1, st1) <- either fail pure $ readHandshakeMsg st0 msg1

  -- Message 2: -> e, ee, s, es (identity payload, signature over sigTarget)
  let payload = encodeNoisePayload $ buildHandshakePayload identityKP (sigTarget staticPub)
  (msg2, st2) <- either fail pure $ writeHandshakeMsg st1 payload
  writeFramedMessage stream msg2

  -- Message 3: <- s, se (initiator identity payload)
  msg3 <- readFramedMessage stream
  (p3, _stFinal) <- either fail pure $ readHandshakeMsg st2 msg3
  np <- either fail pure $ decodeNoisePayload p3
  pk <- either fail pure $ decodePublicKey (npIdentityKey np)
  pure (fromPublicKey pk, pk)

-- | A hand-rolled Noise XX initiator, mirror of 'runForeignResponder'.
-- Returns the responder identity extracted from the msg2 payload.
runForeignInitiator
  :: KeyPair -> (ByteString -> ByteString) -> StreamIO -> IO (PeerId, PublicKey)
runForeignInitiator identityKP sigTarget stream = do
  (st0, staticPub) <- initHandshakeInitiator identityKP

  -- Message 1: -> e (empty payload)
  (msg1, st1) <- either fail pure $ writeHandshakeMsg st0 BS.empty
  writeFramedMessage stream msg1

  -- Message 2: <- e, ee, s, es (responder identity payload)
  msg2 <- readFramedMessage stream
  (p2, st2) <- either fail pure $ readHandshakeMsg st1 msg2
  np <- either fail pure $ decodeNoisePayload p2
  pk <- either fail pure $ decodePublicKey (npIdentityKey np)

  -- Message 3: -> s, se (identity payload, signature over sigTarget)
  let payload = encodeNoisePayload $ buildHandshakePayload identityKP (sigTarget staticPub)
  (msg3, _stFinal) <- either fail pure $ writeHandshakeMsg st2 payload
  writeFramedMessage stream msg3
  pure (fromPublicKey pk, pk)

-- | The production handshake terminates with this error when the payload
-- signature does not bind the identity key to the session's static key.
isSignatureRejection :: IOException -> Bool
isSignatureRejection e = "identity signature verification failed" `isInfixOf` show e

-- | Static key bytes that no session actually uses: signing these
-- simulates a replayed payload (signed for a different Noise session)
-- or a forged one (attacker without the identity private key).
foreignStaticKey :: ByteString
foreignStaticKey = BS.replicate 32 0x5A

spec :: Spec
spec = do
  describe "Production handshake against a hand-rolled foreign peer" $ do
    it "initiator extracts the foreign responder's identity from the payload" $ do
      (pidA, kpA) <- mkTestIdentity
      (pidB, kpB) <- mkTestIdentity
      (streamA, streamB) <- mkMemoryStreamPair
      ((_sess, result), (foreignSeenPid, foreignSeenKey)) <-
        concurrently
          (performStreamHandshake kpA Outbound streamA)
          (runForeignResponder kpB id streamB)
      -- Production side sees the foreign peer's identity ...
      hrRemotePeerId result `shouldBe` pidB
      hrRemotePublicKey result `shouldBe` kpPublic kpB
      -- ... and the foreign peer sees ours.
      foreignSeenPid `shouldBe` pidA
      foreignSeenKey `shouldBe` kpPublic kpA

    it "responder extracts the foreign initiator's identity from the payload" $ do
      (pidA, kpA) <- mkTestIdentity
      (pidB, kpB) <- mkTestIdentity
      (streamA, streamB) <- mkMemoryStreamPair
      ((_sess, result), (foreignSeenPid, foreignSeenKey)) <-
        concurrently
          (performStreamHandshake kpA Inbound streamA)
          (runForeignInitiator kpB id streamB)
      hrRemotePeerId result `shouldBe` pidB
      hrRemotePublicKey result `shouldBe` kpPublic kpB
      foreignSeenPid `shouldBe` pidA
      foreignSeenKey `shouldBe` kpPublic kpA

    it "initiator rejects a responder whose identity_sig covers the wrong static key" $ do
      (_pidA, kpA) <- mkTestIdentity
      (_pidB, kpB) <- mkTestIdentity
      (streamA, streamB) <- mkMemoryStreamPair
      -- The foreign responder advertises kpB's identity but its signature
      -- covers a static key from some other session — exactly what a MitM
      -- replaying an intercepted payload looks like on the wire. It blocks
      -- waiting for msg3 (which never comes) and is cancelled by withAsync.
      withAsync (runForeignResponder kpB (const foreignStaticKey) streamB) $ \_ ->
        performStreamHandshake kpA Outbound streamA
          `shouldThrow` isSignatureRejection

    it "responder rejects an initiator whose identity_sig covers the wrong static key" $ do
      (_pidA, kpA) <- mkTestIdentity
      (_pidB, kpB) <- mkTestIdentity
      (streamA, streamB) <- mkMemoryStreamPair
      withAsync (runForeignInitiator kpB (const foreignStaticKey) streamB) $ \_ ->
        performStreamHandshake kpA Inbound streamA
          `shouldThrow` isSignatureRejection

    it "initiator rejects a responder that advertises another peer's identity key" $ do
      -- Eve presents Bob's public key but cannot produce Bob's signature
      -- over her own session's static key: she signs with her own key.
      (_pidA, kpA) <- mkTestIdentity
      (_pidBob, kpBob) <- mkTestIdentity
      (_pidEve, kpEve) <- mkTestIdentity
      (streamA, streamB) <- mkMemoryStreamPair
      let impersonate staticPub =
            let honest = buildHandshakePayload kpEve staticPub
                bobKey = npIdentityKey (buildHandshakePayload kpBob staticPub)
             in honest { npIdentityKey = bobKey }
          eveResponder stream = do
            (st0, staticPub) <- initHandshakeResponder kpEve
            msg1 <- readFramedMessage stream
            (_p1, st1) <- either fail pure $ readHandshakeMsg st0 msg1
            let payload = encodeNoisePayload (impersonate staticPub)
            (msg2, _st2) <- either fail pure $ writeHandshakeMsg st1 payload
            writeFramedMessage stream msg2
      withAsync (eveResponder streamB) $ \_ ->
        performStreamHandshake kpA Outbound streamA
          `shouldThrow` isSignatureRejection
