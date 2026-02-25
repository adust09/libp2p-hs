# Chapter 2: Peer Identity

Peer identity is the foundation of libp2p. Every peer has a cryptographic key
pair. The public key is encoded in protobuf, hashed, and the result is the
peer's unique identity (Peer ID). This chapter covers every detail needed to
implement key serialization, Peer ID derivation, and signing.

## Protobuf Definitions

```protobuf
syntax = "proto2";

enum KeyType {
    RSA = 0;
    Ed25519 = 1;
    Secp256k1 = 2;
    ECDSA = 3;
}

message PublicKey {
    required KeyType Type = 1;
    required bytes Data = 2;
}

message PrivateKey {
    required KeyType Type = 1;
    required bytes Data = 2;
}
```

Source: https://github.com/libp2p/go-libp2p/blob/master/core/crypto/pb/crypto.proto

**Critical:** `PrivateKey` messages are **never transmitted over the wire**. They
are only used for on-disk storage. `PublicKey` messages are transmitted during
Noise handshakes and in Identify messages.

## Deterministic Protobuf Encoding

libp2p requires **deterministic encoding** of the `PublicKey` message. Standard
protobuf allows non-deterministic serialization, so implementations must enforce:

1. **Minimal varint encoding**: Use the fewest bytes possible for each varint.
2. **Tag order**: Fields must be serialized in field number order (Type=1 first,
   then Data=2).
3. **All fields present**: Both `Type` and `Data` must be included.
4. **No extra fields**: No unknown fields or extensions.

This is critical because the serialized bytes are hashed to produce the Peer ID.
Non-deterministic encoding would produce different Peer IDs for the same key.

### Encoding Walkthrough

For an Ed25519 public key (32 bytes), the protobuf encoding is:

```
Field 1 (Type), varint, value 1 (Ed25519):
  Tag:  0x08  (field 1, wire type 0 = varint)
  Data: 0x01  (KeyType.Ed25519 = 1)

Field 2 (Data), bytes, 32-byte key:
  Tag:  0x12  (field 2, wire type 2 = length-delimited)
  Len:  0x20  (32 in varint)
  Data: <32 bytes of Ed25519 public key>

Total: 2 + 2 + 32 = 36 bytes
```

## Key Types

### Ed25519 (KeyType = 1) — Recommended

Implementations MUST support Ed25519. This is the default key type for new peers.

**Public key encoding (`Data` field):**
- Raw 32-byte Ed25519 public key
- No additional wrapping or encoding

**Private key encoding (`Data` field):**
- Preferred (64 bytes): `[32-byte private key seed][32-byte public key]`
- Legacy (96 bytes): `[32-byte private key seed][32-byte public key][32-byte public key]`
  - If encountered, verify both copies of the public key match; error if they differ

**Signing:**
- Standard Ed25519 signature as per [RFC 8032 §5.1](https://tools.ietf.org/html/rfc8032#section-5.1)
- Sign the raw message (no pre-hashing)
- Signature is 64 bytes

### RSA (KeyType = 0)

Implementations SHOULD support RSA for interoperability with the IPFS DHT and
bootstrap nodes.

**Public key encoding (`Data` field):**
- DER-encoded PKIX (SubjectPublicKeyInfo) format
- This is the standard ASN.1 DER encoding of an RSA public key

**Private key encoding (`Data` field):**
- PKCS#1 ASN.1 DER encoding

**Signing:**
1. Hash the message with SHA-256
2. Sign with RSASSA-PKCS1-v1_5 ([RFC 3447 §8.2](https://tools.ietf.org/html/rfc3447#section-8.2))

**Note:** RSA keys are large. A 2048-bit RSA public key serializes to ~294 bytes
in protobuf, which means the Peer ID will use SHA-256 multihash (not identity).

### Secp256k1 (KeyType = 2) — Optional

**Public key encoding (`Data` field):**
- Standard Bitcoin compressed EC point encoding (33 bytes)
- Format: `0x02` or `0x03` prefix + 32-byte X coordinate

**Private key encoding (`Data` field):**
- Raw 32-byte scalar

**Signing:**
1. Hash the message with SHA-256
2. Sign using ECDSA with the secp256k1 curve
3. Encode signature in DER format per [BIP-0062](https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki)

### ECDSA (KeyType = 3) — Optional

Uses the NIST P-256 curve.

**Public key encoding (`Data` field):**
- ASN.1 DER encoding (SubjectPublicKeyInfo)

**Private key encoding (`Data` field):**
- DER-encoded PKIX format

**Signing:**
1. Hash the message with SHA-256
2. Sign with deterministic ECDSA ([RFC 6979](https://tools.ietf.org/html/rfc6979))
3. Encode signature in DER-encoded ASN.1

## Peer ID Derivation

A Peer ID is a [multihash](https://github.com/multiformats/multihash) of the
serialized `PublicKey` protobuf message.

### Algorithm

```
1. Serialize the PublicKey message to bytes (deterministic encoding)
2. If len(serialized) <= 42:
     peer_id = multihash(identity, serialized)
     // identity multihash = 0x00 <varint length> <raw bytes>
3. If len(serialized) > 42:
     digest = SHA-256(serialized)
     peer_id = multihash(sha2-256, digest)
     // sha2-256 multihash = 0x12 0x20 <32-byte digest>
```

### Which Keys Use Identity vs SHA-256?

| Key Type | Serialized Size | Multihash |
|----------|----------------|-----------|
| Ed25519 | 36 bytes | **Identity** (key embedded in Peer ID) |
| RSA 2048 | ~294 bytes | SHA-256 |
| RSA 4096 | ~550 bytes | SHA-256 |
| Secp256k1 | 37 bytes | **Identity** (key embedded in Peer ID) |
| ECDSA P-256 | ~93 bytes | SHA-256 |

Ed25519 keys serialize to 36 bytes (2 bytes tag+type + 2 bytes tag+length + 32
bytes key), which is ≤ 42, so the identity multihash is used. This means the
full public key is embedded directly in the Peer ID — no hash lookup needed to
recover the key.

### Multihash Format

```
Multihash = <hash-function-code><digest-size><digest>
```

All values are unsigned varints (LEB128), except the digest which is raw bytes.

**Identity multihash** (code = 0x00):
```
0x00 <length as varint> <raw serialized PublicKey bytes>
```

**SHA-256 multihash** (code = 0x12):
```
0x12 0x20 <32-byte SHA-256 digest>
```

### Worked Example: Ed25519

Given an Ed25519 public key (32 bytes, hex):
```
1ed1e8fae2c4a144b8be8fd4b47bf3d3b34b871c3cacf6010f0e42d474fce27e
```

Step 1 — Protobuf encode:
```
08 01 12 20 1ed1e8fae2c4a144b8be8fd4b47bf3d3b34b871c3cacf6010f0e42d474fce27e
```
(36 bytes)

Step 2 — 36 ≤ 42, so use identity multihash:
```
00 24 08 01 12 20 1ed1e8fae2c4a144b8be8fd4b47bf3d3b34b871c3cacf6010f0e42d474fce27e
```
(38 bytes: 0x00 = identity, 0x24 = 36, then 36 bytes of protobuf)

This 38-byte multihash is the Peer ID.

## String Representation

### Legacy Format (base58btc)

Encode the raw multihash bytes with base58btc (Bitcoin alphabet).

- Ed25519 Peer IDs start with `12D3KooW...` (because the identity multihash
  prefix `0x00 0x24 0x08 0x01...` maps to those characters)
- SHA-256 Peer IDs start with `Qm...` or `16Uiu2HA...`

Example: `12D3KooWD3eckifWpRn9wQpMG9R9hX3sD158z7EqHWmweQAJU5SA`

### New Format (CIDv1)

Encode as a CIDv1 with the `libp2p-key` multicodec (0x72):

```
CIDv1 = <multibase-prefix><cid-version><multicodec><multihash>
       = <base32-prefix><0x01><0x72><peer-id-multihash>
```

Default multibase is base32 (lowercase, no padding), prefix `b`.

Example: `bafzbeie5745rpv2m6tjyuugywy4d5ewrqgqqhfnf445he3omzpjbx5xqxe`

### Decoding Rules

1. If the string starts with `1` or `Qm` → base58btc-encoded multihash
2. If the string starts with a multibase prefix → CIDv1
   - Decode multibase
   - Verify CID version = 1
   - Verify multicodec = 0x72 (`libp2p-key`)
   - Extract multihash = Peer ID
3. Otherwise → invalid

Implementations MUST support parsing both formats.
Implementations SHOULD display using the legacy format (base58btc) until the
CIDv1 format is widely adopted.

## Test Vectors

From the official spec (hex-encoded protobuf bytes):

| Key | Hex Bytes |
|-----|-----------|
| Ed25519 private | `080112407e0830617c4a7de83925dfb2694556b12936c477a0e1feb2e148ec9da60fee7d1ed1e8fae2c4a144b8be8fd4b47bf3d3b34b871c3cacf6010f0e42d474fce27e` |
| Ed25519 public | `080112201ed1e8fae2c4a144b8be8fd4b47bf3d3b34b871c3cacf6010f0e42d474fce27e` |
| Secp256k1 private | `0802122053DADF1D5A164D6B4ACDB15E24AA4C5B1D3461BDBD42ABEDB0A4404D56CED8FB` |
| Secp256k1 public | `08021221037777e994e452c21604f91de093ce415f5432f701dd8cd1a7a6fea0e630bfca99` |
| ECDSA private | `08031279307702010104203E5B1FE9...` (see spec for full) |
| ECDSA public | `0803125b3059301306072a8648ce3d...` (see spec for full) |

For each test vector, implementations should verify:
1. The private key can derive the corresponding public key
2. The public key protobuf encoding matches the expected hex
3. The Peer ID derived from the public key is correct

## Haskell Implementation Notes

For the Haskell implementation, consider:

- **Ed25519**: Use the `crypton` or `cryptonite` library (`Crypto.PubKey.Ed25519`)
- **RSA**: Use `crypton` (`Crypto.PubKey.RSA`) with ASN.1 via `asn1-encoding`
- **Protobuf**: Use `proto-lens` or manual encoding (only 2 fields, manual may be simpler)
- **Multihash**: Use or implement a small multihash library
- **Base58**: Use the `base58-bytestring` package

The protobuf encoding is simple enough (2 fields, both required) that a manual
encoder/decoder may be preferable to a full protobuf library dependency. The
key constraint is deterministic encoding.

## Spec Reference

- https://github.com/libp2p/specs/blob/master/peer-ids/peer-ids.md
