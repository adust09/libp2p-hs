-- | Peer ID derivation pinned to the official peer-ids spec test vectors.
--
-- specs/peer-ids/peer-ids.md ("Test vectors", "Peer Ids"): a Peer ID is a
-- multihash of the protobuf-encoded public key — identity multihash when
-- the encoding is at most 42 bytes, SHA-256 otherwise — rendered either as
-- base58btc (legacy) or as a CIDv1 with the libp2p-key (0x72) multicodec
-- in base32 (multibase prefix 'b').
--
-- The vector bytes live in "LibP2P.Crypto.SpecVectors" verbatim from the
-- spec. The expected Peer ID strings below were computed independently
-- from those bytes with a from-scratch implementation of the spec
-- algorithm (SHA-256/identity multihash + base58btc/base32), not with
-- this library; the Ed25519, secp256k1 and ECDSA base58 values also match
-- the ones recorded in issue #176.
module LibP2P.Crypto.SpecVectorsSpec (spec) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import LibP2P.Crypto.Key
import LibP2P.Crypto.PeerId
import LibP2P.Crypto.Protobuf
import LibP2P.Crypto.SpecVectors
import Test.Hspec

data KeyVector = KeyVector
  { kvName :: String
  , kvPrivHex :: String
  , kvPubHex :: String
  , kvBase58 :: Text
  , kvCIDv1 :: Text
  }

vectors :: [KeyVector]
vectors =
  [ KeyVector
      "Ed25519"
      ed25519PrivHex
      ed25519PubHex
      "12D3KooWBtg3aaRMjxwedh83aGiUkwSxDwUZkzuJcfaqUmo7R3pq"
      "bafzaajaiaejcahwr5d5ofrfbis4l5d6uwr57hu5tjodrypfm6yaq6dsc2r2pzyt6"
  , KeyVector
      "secp256k1"
      secp256k1PrivHex
      secp256k1PubHex
      "16Uiu2HAmLhLvBoYaoZfaMUKuibM6ac163GwKY74c5kiSLg5KvLpY"
      "bafzaajiiaijcca3xo7uzjzcsyilaj6i54cj44qk7kqzpoao5rti2pjx6udtdbp6kte"
  , KeyVector
      "ECDSA (P-256)"
      ecdsaPrivHex
      ecdsaPubHex
      "QmVMT29id3TUASyfZZ6k9hmNyc2nYabCo4uMSpDw4zrgDk"
      "bafzbeidigywdclqvl5hxfefwp5onbffcfife7pza57mmfb4tiqmtkdjw64"
  , KeyVector
      "RSA (4096-bit)"
      rsaPrivHex
      rsaPubHex
      "QmaeANgBs1DTSxWSrPPtobgQuxW8XTfsS4ydbK4rCHzqxG"
      "bafzbeifwzcumbiyql7bhv7fe7mixg6i7aohegq75k234m63bnw6dbicmzu"
  ]

spec :: Spec
spec = describe "Peer ID derivation (peer-ids spec test vectors)" $
  mapM_ vectorSpec vectors

vectorSpec :: KeyVector -> Spec
vectorSpec v = describe (kvName v) $ do
  it "derives the expected base58btc Peer ID from the private key vector (full pipeline)" $
    case decodePrivateKey (unhex (kvPrivHex v)) >>= keyPairFromPrivateKey of
      Left err -> expectationFailure err
      Right kp -> toBase58 (fromPublicKey (kpPublic kp)) `shouldBe` kvBase58 v

  it "derives the expected base58btc Peer ID from the public key vector" $
    withPub v $ \pk ->
      toBase58 (fromPublicKey pk) `shouldBe` kvBase58 v

  it "derives the expected CIDv1 (libp2p-key, base32) Peer ID from the public key vector" $
    withPub v $ \pk ->
      toCIDv1 (fromPublicKey pk) `shouldBe` kvCIDv1 v

  it "uses the multihash form the spec mandates for this key size" $
    withPub v $ \pk -> do
      let encoded = unhex (kvPubHex v)
          mh = peerIdBytes (fromPublicKey pk)
      if BS.length encoded <= 42
        then -- identity multihash: 0x00, length, then the encoding verbatim
          mh `shouldBe` BS.pack [0x00, fromIntegral (BS.length encoded)] <> encoded
        else do
          -- SHA-256 multihash: 0x12, 0x20, then a 32-byte digest
          BS.take 2 mh `shouldBe` BS.pack [0x12, 0x20]
          BS.length mh `shouldBe` 34

  it "parses both textual forms back to the same Peer ID" $
    withPub v $ \pk -> do
      let pid = fromPublicKey pk
      parsePeerId (kvBase58 v) `shouldBe` Right pid
      parsePeerId (kvCIDv1 v) `shouldBe` Right pid

-- | Decode the protobuf PublicKey vector and run a check on it.
withPub :: KeyVector -> (PublicKey -> Expectation) -> Expectation
withPub v check =
  case decodePublicKey (unhex (kvPubHex v)) of
    Left err -> expectationFailure err
    Right pk -> check pk
