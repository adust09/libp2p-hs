# Chapter 7: Protocol Negotiation

Protocol negotiation is the mechanism by which two libp2p peers agree on which
protocol to use for a given connection or stream. This chapter covers
multistream-select (the universal negotiation protocol), the Identify protocol
(for exchanging peer metadata after connection establishment), and the Ping
protocol (for liveness checking). Together, these three protocols form the
foundation layer that every higher-level libp2p protocol depends on.

## multistream-select

multistream-select is used everywhere in libp2p: to negotiate the security
protocol on a raw connection, to negotiate the stream multiplexer, and to
negotiate application protocols on each new stream. It is the first thing
spoken on every new channel.

### Protocol ID

```
/multistream/1.0.0
```

### Wire Format

Every multistream-select message is a UTF-8 encoded string with the following
framing:

```
┌──────────────────────┬──────────────────────────────┬─────┐
│  length (uvarint)    │   message (UTF-8 bytes)      │ \n  │
└──────────────────────┴──────────────────────────────┴─────┘
```

- **length**: An unsigned varint (LEB128) encoding the total byte count of the
  message content **including** the trailing newline `\n` (0x0a).
- **message**: The UTF-8 encoded protocol ID or command.
- **`\n`**: A single newline byte (0x0a) that terminates every message.

The newline is part of the message and is counted in the length prefix.

### Unsigned Varint (LEB128) Encoding

The length prefix uses the multiformats unsigned varint format, which is a
variant of unsigned LEB128:

- Unsigned integers are serialized 7 bits at a time, starting with the **least
  significant bits**.
- The **most significant bit** (MSB, bit 7) of each byte is a continuation flag:
  `1` means more bytes follow, `0` means this is the last byte.
- Integers MUST be minimally encoded (no leading zero bytes except for the
  value 0 itself).
- Implementations MUST restrict varint size to a maximum of 9 bytes (63 bits).

| Value | Hex | Varint Bytes |
|-------|-----|--------------|
| 0 | 0x00 | `00` |
| 1 | 0x01 | `01` |
| 127 | 0x7f | `7f` |
| 128 | 0x80 | `80 01` |
| 255 | 0xff | `ff 01` |
| 300 | 0x012c | `ac 02` |
| 16384 | 0x4000 | `80 80 01` |

Encoding algorithm:

```
while value >= 0x80:
    emit (value & 0x7F) | 0x80
    value >>= 7
emit value & 0x7F
```

Decoding algorithm:

```
result = 0
shift  = 0
loop:
    byte = read_next_byte()
    result |= (byte & 0x7F) << shift
    if (byte & 0x80) == 0:
        return result
    shift += 7
    if shift >= 63:
        error("varint too long")
```

### Protocol ID Conventions

Protocol IDs follow a path-like convention:

```
/<organization-or-project>/<protocol-name>/<version>
```

Examples:

| Protocol ID | Description |
|-------------|-------------|
| `/multistream/1.0.0` | multistream-select itself |
| `/noise` | Noise security protocol |
| `/tls/1.0.0` | TLS 1.3 security protocol |
| `/yamux/1.0.0` | Yamux stream multiplexer |
| `/mplex/6.7.0` | mplex stream multiplexer (deprecated) |
| `/ipfs/id/1.0.0` | Identify protocol |
| `/ipfs/id/push/1.0.0` | Identify Push protocol |
| `/ipfs/ping/1.0.0` | Ping protocol |
| `/ipfs/kad/1.0.0` | Kademlia DHT |

Protocol IDs are compared using **exact string matching** by default.
Implementations MAY support custom match functions for more flexible routing.

### Handshake Flow

Both sides begin by sending the multistream-select protocol ID. Both sides MAY
send this initial message simultaneously, without waiting for the other side.
If either side receives anything other than `/multistream/1.0.0\n` as the first
message, the negotiation is aborted.

After the multistream header exchange, the **Initiator** proposes a protocol by
sending its protocol ID. The **Responder** either:
- **Accepts**: Echoes back the same protocol ID.
- **Rejects**: Sends `na\n`.

If rejected, the Initiator may propose a different protocol or close the channel.

### Sequence Diagram

```
Initiator                                          Responder
    │                                                   │
    │  ── /multistream/1.0.0\n ──────────────────────►  │
    │  ◄── /multistream/1.0.0\n ──────────────────────  │
    │     (both sides may send simultaneously)          │
    │                                                   │
    │  ── /noise\n ──────────────────────────────────►  │
    │                                                   │
    │  ◄── na\n ─────────────────────────────────────── │
    │     (Responder does not support /noise)           │
    │                                                   │
    │  ── /tls/1.0.0\n ─────────────────────────────►  │
    │                                                   │
    │  ◄── /tls/1.0.0\n ────────────────────────────── │
    │     (Responder echoes back = accepted)            │
    │                                                   │
    │  ═══ Protocol agreed: /tls/1.0.0 ═══════════     │
    │  (subsequent bytes are TLS handshake data)        │
```

### Optimistic Pipelining

Implementations SHOULD pipeline the multistream header and the first protocol
proposal into a single write to avoid an extra round trip:

```
Initiator                                          Responder
    │                                                   │
    │  ── /multistream/1.0.0\n + /noise\n ──────────►  │
    │                                                   │
    │  ◄── /multistream/1.0.0\n + /noise\n ──────────  │
    │                                                   │
    │  ═══ Negotiation complete in 1 RTT ═══════════   │
```

Both the multistream header and the protocol proposal are sent as separate
length-prefixed messages concatenated in the same TCP segment. The responder
similarly pipelines its multistream header with its response.

### The `ls` Command

A peer may request the list of protocols supported by the remote side by
sending `ls\n`:

```
ls\n
```

On the wire:

```
0x03 0x6c 0x73 0x0a
 │    │    │    └── newline '\n'
 │    │    └─────── 's' (0x73)
 │    └──────────── 'l' (0x6c)
 └───────────────── varint length: 3 (includes 'l', 's', '\n')
```

The response is a single length-prefixed block containing all supported
protocol IDs, each individually length-prefixed:

```
<varint-total-response-size>
  <varint-protocol-length><protocol>\n
  <varint-protocol-length><protocol>\n
  ...
  \n
```

The outer varint gives the total byte count of the entire response payload
(all inner protocol entries plus the trailing newline). Each inner protocol
entry has its own varint length prefix and `\n` suffix. The response ends
with a bare `\n` unless there are zero protocols.

Support for `ls` is OPTIONAL. Implementations MUST NOT depend on a remote peer
supporting `ls`.

### The `na` Response

When a proposed protocol is not supported, the responder sends `na\n`:

```
0x03 0x6e 0x61 0x0a
 │    │    │    └── newline '\n'
 │    │    └─────── 'a' (0x61)
 │    └──────────── 'n' (0x6e)
 └───────────────── varint length: 3
```

### Simultaneous Open (Deprecated)

In NAT hole-punching scenarios, both peers may act as initiators simultaneously.
The simultaneous open extension used the protocol ID
`/libp2p/simultaneous-connect` to resolve this by having both sides generate
a random 64-bit integer and exchanging it prefixed with `select:`. The peer
with the higher integer becomes the initiator.

This extension is **deprecated** as of 2023-09-05 (see
[specs#573](https://github.com/libp2p/specs/issues/573)).

### Complete Wire Example: Negotiating Noise

Below is a complete hex dump of a successful multistream-select negotiation
for the Noise protocol. Each message shows the varint length prefix, the
UTF-8 payload, and the trailing newline.

**Step 1: Initiator sends multistream header**

The string `/multistream/1.0.0\n` is 20 bytes (19 characters + newline):

```hex
Bytes: 14 2f 6d 75 6c 74 69 73 74 72 65 61 6d 2f 31 2e 30 2e 30 0a
       ││ └─────────────────────────────────────────────────────────┘
       ││   "/multistream/1.0.0" + '\n' (19 + 1 = 20 bytes)
       │└── varint length: 0x14 = 20
       └─── (part of varint)
```

Breakdown:
```
14          varint: 20 (0x14)
2f          '/'
6d 75 6c 74 69 73 74 72 65 61 6d   "multistream"
2f          '/'
31 2e 30 2e 30   "1.0.0"
0a          '\n'
```

**Step 2: Responder sends multistream header**

Identical to Step 1:
```hex
14 2f 6d 75 6c 74 69 73 74 72 65 61 6d 2f 31 2e 30 2e 30 0a
```

**Step 3: Initiator proposes Noise**

The string `/noise\n` is 7 bytes (6 characters + newline):

```hex
Bytes: 07 2f 6e 6f 69 73 65 0a
       │  └─────────────────────┘
       │   "/noise" + '\n' (6 + 1 = 7 bytes)
       └── varint: 7 (0x07)
```

**Step 4: Responder accepts (echoes back)**

```hex
07 2f 6e 6f 69 73 65 0a
```

**Complete exchange on the wire (initiator's perspective):**

```hex
SEND: 14 2f6d756c746973747265616d2f312e302e30 0a   /multistream/1.0.0\n
RECV: 14 2f6d756c746973747265616d2f312e302e30 0a   /multistream/1.0.0\n
SEND: 07 2f6e6f697365 0a                            /noise\n
RECV: 07 2f6e6f697365 0a                            /noise\n
--- Noise handshake begins ---
```

**Example with rejection (Initiator proposes TLS, rejected, then Noise):**

```hex
SEND: 14 2f6d756c746973747265616d2f312e302e30 0a   /multistream/1.0.0\n
RECV: 14 2f6d756c746973747265616d2f312e302e30 0a   /multistream/1.0.0\n
SEND: 0b 2f746c732f312e302e30 0a                    /tls/1.0.0\n
RECV: 03 6e61 0a                                     na\n
SEND: 07 2f6e6f697365 0a                            /noise\n
RECV: 07 2f6e6f697365 0a                            /noise\n
--- Noise handshake begins ---
```

### Where multistream-select Is Used

multistream-select is used at multiple points during connection establishment:

```
Raw TCP Connection
    │
    ├── multistream-select: negotiate security protocol
    │   (e.g., /noise or /tls/1.0.0)
    │
    ├── Security handshake (Noise XX or TLS 1.3)
    │
    ├── multistream-select: negotiate stream multiplexer
    │   (e.g., /yamux/1.0.0)
    │
    ├── Multiplexer setup
    │
    └── For each new stream:
        └── multistream-select: negotiate application protocol
            (e.g., /ipfs/id/1.0.0, /ipfs/kad/1.0.0, etc.)
```

## Identify Protocol (`/ipfs/id/1.0.0`)

The Identify protocol allows peers to exchange basic information after a
connection is established: public keys, listen addresses, observed address,
supported protocols, and software version.

### Protocol IDs

| Variant | Protocol ID | Direction |
|---------|-------------|-----------|
| Identify | `/ipfs/id/1.0.0` | Queried peer sends info, then closes stream |
| Identify Push | `/ipfs/id/push/1.0.0` | Initiating peer sends info, then closes stream |

### How Identify Works

1. After a new connection is fully established (security + multiplexing), a
   peer opens a new stream using multistream-select with `/ipfs/id/1.0.0`.
2. The **responder** (the peer being identified) sends a single `Identify`
   protobuf message and closes the stream.
3. The **initiator** reads the message and updates its local metadata store.

The Identify exchange typically happens immediately after connection setup. It
is the mechanism by which peers learn each other's listen addresses, supported
protocols, and observed addresses (useful for NAT detection).

### Identify Sequence Diagram

```
Peer A (Initiator)                             Peer B (Responder)
    │                                               │
    │  ── Open stream ─────────────────────────────►│
    │  ── multistream: /ipfs/id/1.0.0\n ──────────►│
    │  ◄── multistream: /ipfs/id/1.0.0\n ──────────│
    │                                               │
    │  ◄── Identify protobuf message ───────────────│
    │  ◄── (stream closed by Peer B) ───────────────│
    │                                               │
    │  Peer A now knows:                            │
    │   - Peer B's public key                       │
    │   - Peer B's listen addresses                 │
    │   - Peer A's observed address (NAT detection) │
    │   - Peer B's supported protocols              │
    │   - Peer B's agent version                    │
```

### Identify Push

When a peer's information changes at runtime (e.g., new listen address
obtained), it uses Identify Push to notify connected peers:

```
Peer A (Pushes update)                         Peer B (Receives update)
    │                                               │
    │  ── Open stream ─────────────────────────────►│
    │  ── multistream: /ipfs/id/push/1.0.0\n ─────►│
    │  ◄── multistream: /ipfs/id/push/1.0.0\n ─────│
    │                                               │
    │  ── Identify protobuf message ───────────────►│
    │  ── (stream closed by Peer A) ───────────────►│
    │                                               │
    │  Peer B updates its local metadata for Peer A │
```

Note the reversed data flow: with Identify, the responder sends the message;
with Identify Push, the initiator sends the message.

For Identify Push, missing fields should be ignored. Peers MAY send partial
updates containing only the fields whose values have changed.

### Protobuf Definition

```protobuf
syntax = "proto2";

message Identify {
    optional string protocolVersion = 5;
    optional string agentVersion = 6;
    optional bytes publicKey = 1;
    repeated bytes listenAddrs = 2;
    optional bytes observedAddr = 4;
    repeated string protocols = 3;
    optional bytes signedPeerRecord = 8;
}
```

Note: The `signedPeerRecord` field (tag 8) is not in the original spec document
but is present in production implementations (go-libp2p, rust-libp2p). It
contains a serialized `SignedEnvelope` holding a `PeerRecord` signed by the
sending peer. It provides the same address information as `listenAddrs` but in
a cryptographically signed form.

### Field Descriptions

| Field | Tag | Type | Description |
|-------|-----|------|-------------|
| `publicKey` | 1 | `optional bytes` | Peer's public key, marshalled as the `PublicKey` protobuf (see Chapter 2). Used to derive the remote Peer ID. |
| `listenAddrs` | 2 | `repeated bytes` | Multiaddrs on which the peer is listening, each encoded as raw binary multiaddr bytes. |
| `protocols` | 3 | `repeated string` | List of protocol IDs the peer supports (e.g., `/ipfs/kad/1.0.0`). A peer SHOULD only advertise protocols for which it accepts inbound streams. |
| `observedAddr` | 4 | `optional bytes` | The multiaddr of the **initiator** as observed by the responder. Binary-encoded multiaddr. Useful for NAT detection. |
| `protocolVersion` | 5 | `optional string` | Protocol family version string. Example: `ipfs/0.1.0`. Optional but recommended. |
| `agentVersion` | 6 | `optional string` | Free-form implementation identifier. Format: `agent-name/version`. Example: `go-libp2p/0.35.0`. |
| `signedPeerRecord` | 8 | `optional bytes` | Serialized `SignedEnvelope` containing a `PeerRecord`. Provides signed, authenticated address information. |

### Protobuf Field Encoding Reference

Protobuf uses varint-encoded field tags where `tag = (field_number << 3) | wire_type`:

| Field | Field Number | Wire Type | Tag Byte(s) |
|-------|-------------|-----------|-------------|
| `publicKey` | 1 | 2 (length-delimited) | `0x0a` |
| `listenAddrs` | 2 | 2 (length-delimited) | `0x12` |
| `protocols` | 3 | 2 (length-delimited) | `0x1a` |
| `observedAddr` | 4 | 2 (length-delimited) | `0x22` |
| `protocolVersion` | 5 | 2 (length-delimited) | `0x2a` |
| `agentVersion` | 6 | 2 (length-delimited) | `0x32` |
| `signedPeerRecord` | 8 | 2 (length-delimited) | `0x42` |

### Identify Message Wire Format

The Identify message is sent as a raw protobuf (no additional framing beyond
what the stream provides). The stream is closed after the message is sent,
which signals the end of the message to the receiver.

Example partial wire dump of an Identify message:

```hex
2a 09 69 70 66 73 2f 30 2e 31 2e 30
│  │  └──────────────────────────────── "ipfs/0.1.0" (9 bytes)
│  └─────────────────────────────────── varint length: 9
└────────────────────────────────────── tag: field 5, wire type 2 (0x2a)

32 10 67 6f 2d 6c 69 62 70 32 70 2f 30 2e 33 35 2e 30
│  │  └──────────────────────────────────────────────── "go-libp2p/0.35.0" (16 bytes)
│  └─────────────────────────────────────────────────── varint length: 16
└────────────────────────────────────────────────────── tag: field 6, wire type 2 (0x32)

0a 24 08 01 12 20 <32 bytes of Ed25519 public key>
│  │  │  │  │  │
│  │  │  │  │  └── varint length: 32 (the raw key)
│  │  │  │  └───── tag for PublicKey.data: field 2, wire type 2
│  │  │  └──────── varint value: 1 (Ed25519 key type)
│  │  └─────────── tag for PublicKey.type: field 1, wire type 0
│  └────────────── varint length: 36 (2 + 2 + 32)
└───────────────── tag: field 1 (publicKey), wire type 2 (0x0a)

1a 0f 2f 69 70 66 73 2f 6b 61 64 2f 31 2e 30 2e 30
│  │  └──────────────────────────────────────────── "/ipfs/kad/1.0.0" (15 bytes)
│  └─────────────────────────────────────────────── varint length: 15
└────────────────────────────────────────────────── tag: field 3 (protocols), wire type 2 (0x1a)
```

## Ping Protocol (`/ipfs/ping/1.0.0`)

The Ping protocol is a simple liveness check used to verify connectivity and
measure round-trip time (RTT) between two peers. It operates over an already
established libp2p connection.

### Protocol ID

```
/ipfs/ping/1.0.0
```

### Wire Format

The ping protocol has no framing beyond the raw bytes:

1. The **dialing peer** sends exactly **32 bytes** of random binary data.
2. The **listening peer** echoes back the same **32 bytes**.
3. The dialing peer measures the RTT from when it sent the bytes to when it
   received them.

```
┌────────────────────────────────────────────┐
│            32 bytes of random data         │
└────────────────────────────────────────────┘
```

No length prefix. No framing. No protobuf. Just 32 raw bytes sent and 32 raw
bytes echoed back.

### Sequence Diagram

```
Dialer                                         Listener
    │                                               │
    │  ── Open stream ─────────────────────────────►│
    │  ── multistream: /ipfs/ping/1.0.0\n ────────►│
    │  ◄── multistream: /ipfs/ping/1.0.0\n ────────│
    │                                               │
    │  ── 32 random bytes ─────────────────────────►│
    │  ◄── 32 bytes (echo) ────────────────────────│  RTT measured
    │                                               │
    │  ── 32 random bytes ─────────────────────────►│  (optional repeat)
    │  ◄── 32 bytes (echo) ────────────────────────│
    │                                               │
    │  ── close write ─────────────────────────────►│
    │  ◄── close stream ───────────────────────────│
```

### Behavioral Rules

- The dialing peer MAY repeat the ping by sending another 32 bytes of random
  data on the same stream. The listening peer SHOULD loop and echo each
  payload.
- The dialing peer SHOULD close the write side of the stream after sending the
  last payload.
- The listening peer SHOULD finish writing the echoed payload, then close the
  stream.
- The dialing peer MUST NOT keep more than **one** outbound ping stream per
  peer.
- The listening peer SHOULD accept at most **two** streams per peer, because
  cross-stream behavior is non-linear and stream writes occur asynchronously
  (the listener may perceive the dialer closing and opening the wrong streams).

### Wire Example

```hex
SEND (32 bytes, random):
a1 b2 c3 d4 e5 f6 07 18 29 3a 4b 5c 6d 7e 8f 90
01 12 23 34 45 56 67 78 89 9a ab bc cd de ef f0

RECV (32 bytes, identical echo):
a1 b2 c3 d4 e5 f6 07 18 29 3a 4b 5c 6d 7e 8f 90
01 12 23 34 45 56 67 78 89 9a ab bc cd de ef f0
```

### Timeout Behavior

The spec does not prescribe a specific timeout. In practice:

| Implementation | Default Ping Timeout |
|----------------|---------------------|
| go-libp2p | 60 seconds |
| rust-libp2p | 20 seconds |
| js-libp2p | 10 seconds |

Implementations SHOULD enforce a reasonable timeout and consider the peer
unreachable if the echo is not received within it.

## Length-Prefixed Protobuf Encoding

Many libp2p protocols use a common pattern for sending protobuf messages over
streams: a **varint length prefix** followed by the **protobuf payload**.

```
┌──────────────────────┬──────────────────────────────┐
│  length (uvarint)    │   protobuf message (bytes)   │
└──────────────────────┴──────────────────────────────┘
```

- **length**: Unsigned varint (same LEB128 format as multistream-select)
  encoding the byte length of the protobuf message.
- **protobuf message**: The serialized protobuf bytes.

This pattern is used by:

| Protocol | Message Type |
|----------|-------------|
| Kademlia DHT | `Message` (requests and responses) |
| Circuit Relay v2 | `HopMessage`, `StopMessage` |
| AutoNAT | `Message` |
| GossipSub | `RPC` |
| DCUtR | `HolePunch` |

The Identify protocol is a notable **exception**: it sends the protobuf
message without a length prefix, relying on stream closure to delimit the
message.

### Reading Length-Prefixed Messages

```
1. Read varint from stream → message_length
2. Read exactly message_length bytes from stream → raw_bytes
3. Deserialize raw_bytes as the expected protobuf message type
```

### Writing Length-Prefixed Messages

```
1. Serialize protobuf message → raw_bytes
2. Encode len(raw_bytes) as unsigned varint → length_prefix
3. Write length_prefix ++ raw_bytes to stream
```

### Maximum Message Sizes

Protocols typically enforce a maximum message size to prevent memory exhaustion.
Common limits:

| Protocol | Max Message Size |
|----------|-----------------|
| Identify | ~64 KiB (practical limit) |
| Kademlia DHT | ~1 MiB |
| GossipSub | ~1 MiB |
| Circuit Relay v2 | ~4 KiB |

Implementations SHOULD validate the varint length before allocating memory and
reject messages exceeding the protocol's maximum size.

## Haskell Implementation Notes

### multistream-select

- Varint encoding/decoding can use the `Data.Binary.Get` and
  `Data.Binary.Put` monads from the `binary` package, or manual
  `ByteString` manipulation.
- The `bytestring` package provides efficient byte-level operations.
- Consider using `Data.ByteString.Builder` for efficient construction of
  outgoing messages (varint + payload + newline).

Key data type sketch:

```haskell
import Data.ByteString (ByteString)
import Data.Word (Word8)

-- | A protocol identifier, e.g., "/noise" or "/yamux/1.0.0"
newtype ProtocolId = ProtocolId { unProtocolId :: ByteString }
  deriving (Eq, Ord, Show)

-- | Result of a multistream-select negotiation
data NegotiationResult
  = Accepted ProtocolId
  | Rejected
  | NegotiationError String
  deriving (Eq, Show)

-- | Encode a varint (unsigned LEB128)
encodeUvarint :: Word -> ByteString

-- | Decode a varint from a stream, returning the value and remaining bytes
decodeUvarint :: ByteString -> Either String (Word, ByteString)

-- | Encode a multistream-select message (varint-length + payload + newline)
encodeMssMessage :: ByteString -> ByteString

-- | Negotiate a protocol as the initiator
negotiateInitiator
  :: (ByteString -> IO ())    -- ^ send function
  -> IO ByteString            -- ^ receive function
  -> [ProtocolId]             -- ^ protocols to try, in preference order
  -> IO NegotiationResult

-- | Handle negotiation as the responder
negotiateResponder
  :: (ByteString -> IO ())    -- ^ send function
  -> IO ByteString            -- ^ receive function
  -> [ProtocolId]             -- ^ locally supported protocols
  -> IO NegotiationResult
```

### Identify

- Use the `proto-lens` or `proto3-wire` package for protobuf
  encoding/decoding.
- The `Identify` message is straightforward proto2 with all optional or
  repeated fields.
- Multiaddr values in `listenAddrs` and `observedAddr` are raw binary
  multiaddr bytes (see Chapter 3).

```haskell
data IdentifyMessage = IdentifyMessage
  { idProtocolVersion  :: Maybe Text
  , idAgentVersion     :: Maybe Text
  , idPublicKey        :: Maybe ByteString   -- serialized PublicKey protobuf
  , idListenAddrs      :: [ByteString]       -- binary multiaddrs
  , idObservedAddr     :: Maybe ByteString   -- binary multiaddr
  , idProtocols        :: [Text]             -- supported protocol IDs
  , idSignedPeerRecord :: Maybe ByteString   -- serialized SignedEnvelope
  } deriving (Eq, Show)

-- | Run the Identify protocol as the querying peer
runIdentify :: Stream -> IO IdentifyMessage

-- | Handle an incoming Identify request
handleIdentify :: Stream -> IdentifyMessage -> IO ()

-- | Send an Identify Push to a remote peer
pushIdentify :: Stream -> IdentifyMessage -> IO ()
```

### Ping

- Ping is the simplest protocol to implement. It needs a CSPRNG for the
  32-byte payload.
- Use `Crypto.Random` from the `crypton` package (or `entropy` package)
  for generating random bytes.

```haskell
import Data.ByteString (ByteString)

pingPayloadSize :: Int
pingPayloadSize = 32

-- | Send a ping and measure RTT
sendPing :: Stream -> IO (Either PingError NominalDiffTime)

-- | Handle incoming pings (echo loop)
handlePing :: Stream -> IO ()

data PingError
  = PingTimeout
  | PingMismatch       -- echoed bytes differ from sent bytes
  | PingStreamError String
  deriving (Eq, Show)
```

### Relevant Haskell Packages

| Package | Purpose |
|---------|---------|
| `binary` | Varint encoding/decoding, binary serialization |
| `bytestring` | Efficient byte-level operations |
| `proto-lens` | Protobuf code generation and serialization |
| `proto3-wire` | Low-level protobuf wire format encoding |
| `crypton` | CSPRNG for ping payloads |
| `stm` | Concurrent state management for protocol handlers |
| `async` | Spawning concurrent protocol handler tasks |
| `network` | Low-level socket operations |
| `text` | UTF-8 text handling for protocol IDs |
| `time` | RTT measurement for Ping |

## Spec References

- multistream-select: https://github.com/multiformats/multistream-select
- Unsigned varint: https://github.com/multiformats/unsigned-varint
- Connections (multistream-select in context): https://github.com/libp2p/specs/blob/master/connections/README.md
- Identify: https://github.com/libp2p/specs/blob/master/identify/README.md
- Ping: https://github.com/libp2p/specs/blob/master/ping/ping.md
- Simultaneous open (deprecated): https://github.com/libp2p/specs/blob/master/connections/simopen.md
- go-libp2p Identify protobuf: https://github.com/libp2p/go-libp2p/blob/master/p2p/protocol/identify/pb/identify.proto
