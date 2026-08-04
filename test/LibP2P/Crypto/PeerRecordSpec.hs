module LibP2P.Crypto.PeerRecordSpec (spec) where

import Test.Hspec

import Control.Monad (replicateM)
import qualified Data.ByteString as BS
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (KeyPair (..))
import LibP2P.Crypto.PeerId (fromPublicKey, peerIdBytes)
import LibP2P.Crypto.PeerRecord
import LibP2P.Crypto.SignedEnvelope
  ( SignedEnvelope (..)
  , createEnvelope
  , encodeSignedEnvelope
  )

-- | A fresh Ed25519 identity.
newKeyPair :: IO KeyPair
newKeyPair = either (error . ("keygen failed: " <>)) id <$> generateKeyPair

-- | A PeerRecord owned by the given key pair.
recordFor :: KeyPair -> [BS.ByteString] -> PeerRecord
recordFor kp addrs = PeerRecord
  { prPeerId    = peerIdBytes (fromPublicKey (kpPublic kp))
  , prSeq       = 42
  , prAddresses = addrs
  }

-- | /ip4/127.0.0.1/tcp/4001 in binary multiaddr form.
addr4001 :: BS.ByteString
addr4001 = BS.pack [0x04, 0x7f, 0x00, 0x00, 0x01, 0x06, 0x0f, 0xa1]

spec :: Spec
spec = do
  describe "PeerRecord protobuf (RFC 0003)" $ do
    it "encode → decode round-trip preserves all fields" $ do
      kp <- newKeyPair
      let record = recordFor kp [addr4001, BS.pack [0x04, 10, 0, 0, 1, 0x06, 0x1f, 0x90]]
      decodePeerRecord (encodePeerRecord record) `shouldBe` Right record

    it "encodes go-libp2p field numbers at the byte level" $ do
      -- core/peer/pb/peer_record.proto: peer_id = 1 (bytes), seq = 2
      -- (uint64), addresses = 3 (repeated AddressInfo{multiaddr = 1}).
      let record = PeerRecord (BS.pack [0xAA]) 5 [addr4001]
      encodePeerRecord record `shouldBe` BS.concat
        [ BS.pack [0x0A, 0x01, 0xAA]  -- peer_id = 1, length-delimited
        , BS.pack [0x10, 0x05]        -- seq = 2, varint
        , BS.pack [0x1A, 0x0A, 0x0A, 0x08], addr4001  -- addresses = 3, nested AddressInfo
        ]

    it "decodes a record with no addresses" $ do
      let record = PeerRecord (BS.pack [0x01, 0x02]) 7 []
      decodePeerRecord (encodePeerRecord record) `shouldBe` Right record

    it "rejects malformed bytes" $ do
      decodePeerRecord (BS.pack [0xFF, 0xFF, 0xFF]) `shouldSatisfy` \r ->
        case r of
          Left _  -> True
          Right _ -> False

  describe "envelope constants (go-libp2p core/peer)" $ do
    it "payload type is the libp2p-peer-record multicodec bytes 0x03 0x01" $
      peerRecordEnvelopePayloadType `shouldBe` BS.pack [0x03, 0x01]

    it "domain string is libp2p-peer-record" $
      peerRecordEnvelopeDomain `shouldBe` "libp2p-peer-record"

  describe "timestampSeq" $ do
    it "is strictly monotonic across calls" $ do
      seqs <- replicateM 5 timestampSeq
      and (zipWith (<) seqs (drop 1 seqs)) `shouldBe` True

  describe "sealPeerRecord / openPeerRecordEnvelope" $ do
    it "seal → encode → open round-trip recovers the record" $ do
      kp <- newKeyPair
      let record = recordFor kp [addr4001]
      case sealPeerRecord kp record of
        Left err -> expectationFailure $ "sealPeerRecord failed: " ++ err
        Right env -> do
          sePayloadType env `shouldBe` peerRecordEnvelopePayloadType
          case openPeerRecordEnvelope (encodeSignedEnvelope env) of
            Left err -> expectationFailure $ "open failed: " ++ err
            Right (env', record') -> do
              record' `shouldBe` record
              sePublicKey env' `shouldBe` kpPublic kp

    it "rejects an envelope signed under a different domain" $ do
      kp <- newKeyPair
      let record = recordFor kp [addr4001]
      case createEnvelope (kpPrivate kp) (kpPublic kp) "some-other-domain"
             peerRecordEnvelopePayloadType (encodePeerRecord record) of
        Left err -> expectationFailure $ "createEnvelope failed: " ++ err
        Right env ->
          openPeerRecordEnvelope (encodeSignedEnvelope env)
            `shouldSatisfy` isLeft

    it "rejects an envelope with a tampered payload" $ do
      kp <- newKeyPair
      let record = recordFor kp [addr4001]
      case sealPeerRecord kp record of
        Left err -> expectationFailure $ "sealPeerRecord failed: " ++ err
        Right env -> do
          let tamperedRecord = record { prSeq = 43 }
              tampered = env { sePayload = encodePeerRecord tamperedRecord }
          openPeerRecordEnvelope (encodeSignedEnvelope tampered)
            `shouldSatisfy` isLeft

    it "rejects an envelope with the wrong payload type" $ do
      kp <- newKeyPair
      let record = recordFor kp [addr4001]
      case createEnvelope (kpPrivate kp) (kpPublic kp) peerRecordEnvelopeDomain
             (BS.pack [0x03, 0x02]) (encodePeerRecord record) of
        Left err -> expectationFailure $ "createEnvelope failed: " ++ err
        Right env ->
          openPeerRecordEnvelope (encodeSignedEnvelope env)
            `shouldSatisfy` isLeft

    it "rejects a record whose peer id is not derived from the signing key" $ do
      kpSigner <- newKeyPair
      kpOther  <- newKeyPair
      -- Valid signature by kpSigner over a record claiming kpOther's id
      let record = recordFor kpOther [addr4001]
      case sealPeerRecord kpSigner record of
        Left err -> expectationFailure $ "sealPeerRecord failed: " ++ err
        Right env ->
          openPeerRecordEnvelope (encodeSignedEnvelope env)
            `shouldSatisfy` isLeft
  where
    isLeft :: Either a b -> Bool
    isLeft (Left _)  = True
    isLeft (Right _) = False
