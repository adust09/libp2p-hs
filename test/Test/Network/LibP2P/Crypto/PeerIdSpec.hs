module Test.Network.LibP2P.Crypto.PeerIdSpec (spec) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Network.LibP2P.Crypto.Ed25519 (generateKeyPair, keyPairFromSeed)
import Network.LibP2P.Crypto.Key
import Network.LibP2P.Crypto.PeerId
import Network.LibP2P.Crypto.Protobuf
import Test.Hspec

spec :: Spec
spec = do
  describe "Protobuf encoding" $ do
    it "encodes Ed25519 pubkey to 36 bytes" $ do
      -- From docs/02-peer-identity.md:
      -- Ed25519 pubkey 0x1ed1e8fa... → protobuf 0x08011220... (36 bytes)
      let rawPubKey =
            BS.pack
              [ 0x1e, 0xd1, 0xe8, 0xfa, 0xe2, 0xc4, 0xa1, 0x44
              , 0xb8, 0xbe, 0x8f, 0xd4, 0xb4, 0x7b, 0xf3, 0xd3
              , 0xb3, 0x4b, 0x87, 0x1c, 0x3c, 0xac, 0xf6, 0x01
              , 0x0f, 0x0e, 0x42, 0xd4, 0x74, 0xfc, 0xe2, 0x7e
              ]
      let pk = PublicKey Ed25519 rawPubKey
      let encoded = encodePublicKey pk
      -- Should be: 08 01 12 20 <32 bytes>
      BS.length encoded `shouldBe` 36
      BS.take 4 encoded `shouldBe` BS.pack [0x08, 0x01, 0x12, 0x20]
      BS.drop 4 encoded `shouldBe` rawPubKey

    it "produces deterministic encoding (field order: Type=1, Data=2)" $ do
      let rawPubKey = BS.replicate 32 0xAA
      let pk = PublicKey Ed25519 rawPubKey
      let e1 = encodePublicKey pk
      let e2 = encodePublicKey pk
      e1 `shouldBe` e2

  describe "Protobuf round-trip" $ do
    it "decode(encode(pk)) == pk for Ed25519" $ do
      let rawPubKey = BS.replicate 32 0xBB
      let pk = PublicKey Ed25519 rawPubKey
      decodePublicKey (encodePublicKey pk) `shouldBe` Right pk

  describe "PeerId derivation" $ do
    it "Ed25519 pubkey (36 bytes serialized) uses identity multihash" $ do
      -- From docs: 36 ≤ 42, so identity multihash:
      -- 00 24 08 01 12 20 <32 bytes>
      let rawPubKey =
            BS.pack
              [ 0x1e, 0xd1, 0xe8, 0xfa, 0xe2, 0xc4, 0xa1, 0x44
              , 0xb8, 0xbe, 0x8f, 0xd4, 0xb4, 0x7b, 0xf3, 0xd3
              , 0xb3, 0x4b, 0x87, 0x1c, 0x3c, 0xac, 0xf6, 0x01
              , 0x0f, 0x0e, 0x42, 0xd4, 0x74, 0xfc, 0xe2, 0x7e
              ]
      let pk = PublicKey Ed25519 rawPubKey
      let pid = fromPublicKey pk
      let bytes = peerIdBytes pid
      -- 38 bytes: 0x00 (identity) + 0x24 (36) + 36 bytes protobuf
      BS.length bytes `shouldBe` 38
      BS.take 2 bytes `shouldBe` BS.pack [0x00, 0x24]
      BS.index bytes 2 `shouldBe` 0x08
      BS.index bytes 3 `shouldBe` 0x01

    it "PeerId from known key matches expected bytes" $ do
      let rawPubKey =
            BS.pack
              [ 0x1e, 0xd1, 0xe8, 0xfa, 0xe2, 0xc4, 0xa1, 0x44
              , 0xb8, 0xbe, 0x8f, 0xd4, 0xb4, 0x7b, 0xf3, 0xd3
              , 0xb3, 0x4b, 0x87, 0x1c, 0x3c, 0xac, 0xf6, 0x01
              , 0x0f, 0x0e, 0x42, 0xd4, 0x74, 0xfc, 0xe2, 0x7e
              ]
      let pk = PublicKey Ed25519 rawPubKey
      let pid = fromPublicKey pk
      let expected =
            BS.pack $
              [0x00, 0x24, 0x08, 0x01, 0x12, 0x20]
                <> [ 0x1e, 0xd1, 0xe8, 0xfa, 0xe2, 0xc4, 0xa1, 0x44
                   , 0xb8, 0xbe, 0x8f, 0xd4, 0xb4, 0x7b, 0xf3, 0xd3
                   , 0xb3, 0x4b, 0x87, 0x1c, 0x3c, 0xac, 0xf6, 0x01
                   , 0x0f, 0x0e, 0x42, 0xd4, 0x74, 0xfc, 0xe2, 0x7e
                   ]
      peerIdBytes pid `shouldBe` expected

  describe "Base58 encoding" $ do
    it "Ed25519 PeerId starts with 12D3KooW" $ do
      let rawPubKey = BS.replicate 32 0x42
      let pk = PublicKey Ed25519 rawPubKey
      let pid = fromPublicKey pk
      let b58 = toBase58 pid
      T.take 8 b58 `shouldBe` ("12D3KooW" :: Text)

    it "base58 round-trip: fromBase58(toBase58(pid)) == pid" $ do
      let rawPubKey = BS.replicate 32 0x42
      let pk = PublicKey Ed25519 rawPubKey
      let pid = fromPublicKey pk
      fromBase58 (toBase58 pid) `shouldBe` Right pid

  describe "Ed25519 key generation" $ do
    it "generates a valid key pair" $ do
      result <- generateKeyPair
      case result of
        Right kp -> do
          let pk = publicKey kp
          pkType pk `shouldBe` Ed25519
          BS.length (pkBytes pk) `shouldBe` 32
        Left err -> expectationFailure err

    it "creates key pair from seed" $ do
      let seed = BS.replicate 32 0x01
      case keyPairFromSeed seed of
        Right kp -> do
          pkType (publicKey kp) `shouldBe` Ed25519
          BS.length (pkBytes (publicKey kp)) `shouldBe` 32
        Left err -> expectationFailure err

  describe "Sign and verify" $ do
    it "sign then verify succeeds" $ do
      Right kp <- generateKeyPair
      let msg = "hello libp2p" :: BS.ByteString
      case sign (kpPrivate kp) msg of
        Right sig -> verify (kpPublic kp) msg sig `shouldBe` True
        Left err -> expectationFailure err

    it "verify with wrong message fails" $ do
      Right kp <- generateKeyPair
      case sign (kpPrivate kp) ("correct" :: BS.ByteString) of
        Right sig -> verify (kpPublic kp) ("wrong" :: BS.ByteString) sig `shouldBe` False
        Left err -> expectationFailure err

    it "verify with wrong key fails" $ do
      Right kp1 <- generateKeyPair
      Right kp2 <- generateKeyPair
      case sign (kpPrivate kp1) ("message" :: BS.ByteString) of
        Right sig -> verify (kpPublic kp2) ("message" :: BS.ByteString) sig `shouldBe` False
        Left err -> expectationFailure err

    it "sign with invalid key returns Left" $ do
      let badKey = PrivateKey Ed25519 (BS.pack [0x00]) -- invalid 1-byte key
      sign badKey "test" `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
