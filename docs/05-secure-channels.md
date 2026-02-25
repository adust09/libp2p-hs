# Chapter 5: Secure Channels

Every libp2p connection must be encrypted and authenticated. This chapter covers
the two supported security protocols: Noise (recommended) and TLS 1.3.

## Noise Protocol

The Noise protocol is the recommended security channel for libp2p. It uses a
single, fixed cipher suite with no negotiation.

### Protocol ID

```
/noise
```

### Noise Protocol Name

```
Noise_XX_25519_ChaChaPoly_SHA256
```

This name fully specifies:
- **XX**: Handshake pattern (mutual authentication, both parties transmit static keys)
- **25519**: X25519 Diffie-Hellman key exchange
- **ChaChaPoly**: ChaCha20-Poly1305 AEAD cipher
- **SHA256**: SHA-256 hash function

There is **no cipher negotiation**. All libp2p Noise implementations use exactly
this cipher suite.

### Key Architecture

libp2p Noise uses **two distinct key pairs**:

1. **Identity key pair**: The peer's long-term libp2p identity key (Ed25519, RSA,
   etc.). Used to derive the Peer ID.
2. **Noise static key pair**: An X25519 key pair used specifically for the Noise
   handshake. This is separate from the identity key because:
   - Not all identity key types are compatible with Noise DH operations
   - Separation prevents static key reuse across protocols

Implementations MAY generate a new Noise static key pair per session or reuse
one across sessions. Implementations SHOULD NOT persist the Noise static key
to disk.

### XX Handshake Pattern

```
XX:
  -> e
  <- e, ee, s, es
  -> s, se
```

Three messages are exchanged:

| Message | Direction | Tokens | Content |
|---------|-----------|--------|---------|
| 1 | Initiator → Responder | `e` | Initiator's ephemeral public key |
| 2 | Responder → Initiator | `e, ee, s, es` | Responder's ephemeral key + DH + responder's static key (encrypted) + DH |
| 3 | Initiator → Responder | `s, se` | Initiator's static key (encrypted) + DH |

**Token meanings:**
- `e`: Send ephemeral public key
- `s`: Send static public key (encrypted after first DH)
- `ee`: DH(ephemeral_initiator, ephemeral_responder)
- `es`: DH(ephemeral_initiator, static_responder)
- `se`: DH(static_initiator, ephemeral_responder)

### Handshake Sequence Diagram

```
Initiator (Alice)                          Responder (Bob)
     │                                          │
     │  ── Message 1: [e] ──────────────────►   │
     │     (Alice's ephemeral pubkey,            │
     │      plaintext, no payload)               │
     │                                          │
     │  ◄── Message 2: [e, ee, s, es] ────────  │
     │     (Bob's ephemeral pubkey,              │
     │      Bob's static pubkey (encrypted),     │
     │      handshake payload (encrypted):       │
     │        - Bob's libp2p identity key        │
     │        - signature over Noise static key  │
     │        - optional extensions)             │
     │                                          │
     │  ── Message 3: [s, se] ──────────────►   │
     │     (Alice's static pubkey (encrypted),   │
     │      handshake payload (encrypted):       │
     │        - Alice's libp2p identity key      │
     │        - signature over Noise static key  │
     │        - optional extensions)             │
     │                                          │
     │  ══ Secure channel established ══════     │
     │  (Two CipherState objects for             │
     │   bidirectional encryption)               │
```

### Static Key Authentication

The Noise static key is authenticated by signing it with the libp2p identity
private key. This proves the Noise participant possesses the claimed identity.

**Signature data:**
```
"noise-libp2p-static-key:" || noise_static_public_key
```

The prefix is the literal UTF-8 string `noise-libp2p-static-key:` (including
the colon), followed by the Noise static public key encoded per
[RFC 7748 §5](https://tools.ietf.org/html/rfc7748#section-5) (32 bytes for
X25519).

The signature is produced using the identity private key's signing algorithm
(e.g., Ed25519 signature for Ed25519 identity keys, RSASSA-PKCS1-v1_5 for RSA).

### Handshake Payload

Messages 2 and 3 contain an encrypted payload with the following protobuf:

```protobuf
syntax = "proto2";

message NoiseExtensions {
    repeated bytes webtransport_certhashes = 1;
    repeated string stream_muxers = 2;
}

message NoiseHandshakePayload {
    optional bytes identity_key = 1;
    optional bytes identity_sig = 2;
    optional NoiseExtensions extensions = 4;
}
```

**Fields:**
- `identity_key`: Serialized `PublicKey` protobuf (as defined in Chapter 2)
- `identity_sig`: Signature of `"noise-libp2p-static-key:" || noise_static_pubkey`
  using the identity private key
- `extensions`: Optional. The `stream_muxers` field can be used to negotiate the
  muxer during the handshake (avoiding a round-trip for multistream-select muxer
  negotiation)

**Message 1 (initiator's first message):** The initiator MUST NOT send extensions
in message 1 (it is not encrypted and has no secrecy guarantee).

**Message 2 (responder):** Has forward secrecy but the sender has NOT yet
authenticated the responder. The payload might be sent to an active attacker.

**Message 3 (initiator):** Has forward secrecy and the initiator has verified
the responder.

### Payload Validation

Upon receiving a handshake payload, the recipient MUST:

1. Decode `identity_key` into a usable public key
2. Validate `identity_sig` against the Noise static public key received in the
   handshake
3. If the signature is invalid → terminate the connection immediately
4. Derive the remote Peer ID from `identity_key` and verify it matches
   expectations (if dialing a specific peer)

### Wire Format

All Noise messages (handshake and transport) are framed as:

```
┌──────────────────────┬──────────────────────────────┐
│  noise_message_len   │       noise_message          │
│   (2 bytes, BE)      │     (variable length)        │
└──────────────────────┴──────────────────────────────┘
```

- `noise_message_len`: 2-byte **big-endian** unsigned integer, length of
  `noise_message` in bytes
- `noise_message`: A Noise message (max 65535 bytes)

### Handshake Phase

During the handshake, each `noise_message` is a Noise handshake message as
defined in the [Noise spec](https://noiseprotocol.org/noise.html#message-format).

### Transport Phase (Post-Handshake)

After the handshake completes, each peer has two `CipherState` objects:
- One for **encrypting** outgoing data
- One for **decrypting** incoming data

Each `noise_message` is now an AEAD ciphertext:

```
noise_message = encrypted_payload || authentication_tag
                                     (16 bytes)
```

The plaintext payload is encrypted with ChaCha20-Poly1305. The 16-byte
authentication tag provides integrity and authenticity.

**Maximum message size:** 65535 bytes (limited by the 2-byte length prefix).
The maximum plaintext per message is 65535 - 16 = 65519 bytes.

**Nonce exhaustion:** If more than 2^64 - 1 messages are exchanged, the
connection MUST be terminated (nonce reuse would break security).

### Complete Handshake Byte Flow Example

```
Initiator → Responder:
  [2 bytes: length] [32 bytes: ephemeral pubkey]

Responder → Initiator:
  [2 bytes: length] [32 bytes: ephemeral pubkey]
                     [encrypted: static pubkey (32+16 bytes)]
                     [encrypted: payload protobuf + 16 byte tag]

Initiator → Responder:
  [2 bytes: length] [encrypted: static pubkey (32+16 bytes)]
                     [encrypted: payload protobuf + 16 byte tag]
```

Message 1 size: 2 + 32 = 34 bytes
Message 2 size: 2 + 32 + 48 + (payload_len + 16) bytes
Message 3 size: 2 + 48 + (payload_len + 16) bytes

## TLS 1.3

TLS 1.3 is an alternative security protocol, primarily used by QUIC.

### Protocol ID

```
/tls/1.0.0
```

### How It Works in libp2p

libp2p uses TLS 1.3 with a special twist: instead of relying on a certificate
authority (CA), each peer generates a **self-signed certificate** that embeds
its libp2p identity public key.

The certificate contains a custom extension (OID `1.3.6.1.4.1.53594.1.1`) with
a signed payload that binds the TLS certificate key to the libp2p identity key.

### ALPN

When used with QUIC, the ALPN (Application-Layer Protocol Negotiation) protocol
ID is `libp2p`.

### Peer ID Verification

After the TLS handshake:
1. Extract the libp2p public key from the remote peer's certificate extension
2. Verify the signature binding the TLS key to the identity key
3. Derive the Peer ID from the extracted public key
4. Verify it matches the expected Peer ID (if known)

### When to Use TLS vs Noise

| Aspect | Noise | TLS 1.3 |
|--------|-------|---------|
| Protocol ID | `/noise` | `/tls/1.0.0` |
| Used with TCP | Yes (primary) | Yes |
| Used with QUIC | No (QUIC has built-in TLS) | Yes (built into QUIC) |
| Cipher negotiation | None (fixed suite) | Standard TLS negotiation |
| Implementation complexity | Moderate | Uses existing TLS libraries |
| Handshake messages | 3 (1.5 RTT) | 2 (1 RTT) |

For a Haskell implementation starting with TCP, **implement Noise first**.
TLS 1.3 becomes relevant when adding QUIC support.

## Haskell Implementation Notes

- **Noise framework**: The `cacophony` Haskell package implements the Noise
  Protocol Framework. It supports XX pattern and the required cipher suite.
- **X25519**: Available in `crypton` (`Crypto.PubKey.Curve25519`)
- **ChaCha20-Poly1305**: Available in `crypton` (`Crypto.Cipher.ChaChaPoly1305`)
- **TLS 1.3**: The `tls` package by Vincent Hanquez supports TLS 1.3

## Spec References

- Noise spec: https://github.com/libp2p/specs/tree/master/noise
- TLS spec: https://github.com/libp2p/specs/tree/master/tls
- Noise Protocol Framework: https://noiseprotocol.org/noise.html
