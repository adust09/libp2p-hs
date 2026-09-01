module LibP2P.Switch.CertifiedRecordsSpec (spec) where

import Control.Concurrent.STM (atomically, newTVarIO)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (KeyPair, kpPublic)
import LibP2P.Crypto.PeerId (PeerId, fromPublicKey, peerIdBytes)
import LibP2P.Crypto.PeerRecord (PeerRecord (..), sealPeerRecord)
import LibP2P.Crypto.SignedEnvelope (encodeSignedEnvelope)
import LibP2P.Multiaddr.Codec (encodeProtocols)
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.Switch.CertifiedRecords
import Test.Hspec

-- | Seal a record for @kp@'s peer advertising a single address on @port@.
sealAt :: KeyPair -> Word64 -> Int -> BS.ByteString
sealAt kp seqNo port =
  let peer = fromPublicKey (kpPublic kp)
      record = PeerRecord
        { prPeerId    = peerIdBytes peer
        , prSeq       = seqNo
        , prAddresses = [encodeProtocols [IP4 0x7f000001, TCP (fromIntegral port)]]
        }
  in either (error . ("sealPeerRecord failed: " ++)) encodeSignedEnvelope
       (sealPeerRecord kp record)

addrAt :: Int -> BS.ByteString
addrAt port = encodeProtocols [IP4 0x7f000001, TCP (fromIntegral port)]

spec :: Spec
spec = do
  describe "verifyPeerRecord" $ do
    it "accepts an envelope signed by the authenticated peer" $ do
      Right kp <- generateKeyPair
      let peer = fromPublicKey (kpPublic kp)
      case verifyPeerRecord peer (sealAt kp 7 4001) of
        Left err -> expectationFailure ("expected the record to verify: " ++ err)
        Right record -> do
          crSeq record `shouldBe` 7
          crAddresses record `shouldBe` [addrAt 4001]

    it "rejects an envelope signed by a different peer" $ do
      Right signer <- generateKeyPair
      Right other <- generateKeyPair
      let otherPeer = fromPublicKey (kpPublic other)
      verifyPeerRecord otherPeer (sealAt signer 7 4001)
        `shouldSatisfy` either (const True) (const False)

    it "rejects bytes that are not an envelope" $ do
      Right kp <- generateKeyPair
      let peer = fromPublicKey (kpPublic kp)
      verifyPeerRecord peer (BS.pack [0xDE, 0xAD, 0xBE, 0xEF])
        `shouldSatisfy` either (const True) (const False)

  -- specs/RFC/0003-routing-records.md: "a receiving peer MUST keep track
  -- of the latest seq value received for each peer and reject incoming
  -- records unless they contain a greater seq value than the last
  -- received."
  describe "consumeCertifiedRecord" $ do
    it "accepts the first record for a peer" $ do
      (kp, peer) <- freshPeer
      records <- newTVarIO mempty
      Right record <- pure (verifyPeerRecord peer (sealAt kp 1 4001))
      accepted <- atomically (consumeCertifiedRecord records peer record)
      accepted `shouldBe` True
      retained <- atomically (lookupCertifiedRecord records peer)
      fmap crSeq retained `shouldBe` Just 1

    it "accepts a record with a greater sequence number and retains it" $ do
      (kp, peer) <- freshPeer
      records <- newTVarIO mempty
      _ <- consume records peer kp 1 4001
      accepted <- consume records peer kp 2 9999
      accepted `shouldBe` True
      retained <- atomically (lookupCertifiedRecord records peer)
      fmap crAddresses retained `shouldBe` Just [addrAt 9999]

    it "rejects a record with an equal sequence number and keeps the retained one" $ do
      (kp, peer) <- freshPeer
      records <- newTVarIO mempty
      _ <- consume records peer kp 3 4001
      accepted <- consume records peer kp 3 9999
      accepted `shouldBe` False
      retained <- atomically (lookupCertifiedRecord records peer)
      fmap crAddresses retained `shouldBe` Just [addrAt 4001]

    it "rejects a record with a lower sequence number and keeps the retained one" $ do
      (kp, peer) <- freshPeer
      records <- newTVarIO mempty
      _ <- consume records peer kp 5 4001
      accepted <- consume records peer kp 4 9999
      accepted `shouldBe` False
      retained <- atomically (lookupCertifiedRecord records peer)
      fmap crAddresses retained `shouldBe` Just [addrAt 4001]

    it "tracks sequence numbers per peer" $ do
      (kpA, peerA) <- freshPeer
      (kpB, peerB) <- freshPeer
      records <- newTVarIO mempty
      _ <- consume records peerA kpA 9 4001
      -- peerB has no retained record, so a low sequence number is still a
      -- first record for it.
      accepted <- consume records peerB kpB 1 9999
      accepted `shouldBe` True
  where
    freshPeer = do
      Right kp <- generateKeyPair
      pure (kp, fromPublicKey (kpPublic kp))

    consume records peer kp seqNo port = do
      Right record <- pure (verifyPeerRecord peer (sealAt kp seqNo port))
      atomically (consumeCertifiedRecord records peer record)
