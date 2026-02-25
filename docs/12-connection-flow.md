# Chapter 12: Connection Flow

This chapter provides a complete end-to-end walkthrough of libp2p connection
establishment, tying together transports (Chapter 4), secure channels
(Chapter 5), stream multiplexing (Chapter 6), and protocol negotiation into a
single narrative. It traces exactly what bytes flow over the wire from the
moment a TCP socket opens to the point where application data is exchanged.

## Overview

Establishing a libp2p connection over TCP requires multiple sequential
negotiations. The raw transport provides only a bidirectional byte stream; on
top of it, the peers must agree on a security protocol, perform a cryptographic
handshake, agree on a stream multiplexer, and then open individual streams for
application protocols. Each agreement step uses multistream-select.

The full pipeline looks like this:

```
TCP connect
    |
    v
multistream-select --> agree on security protocol (/noise)
    |
    v
Noise XX handshake (3 messages)
    |
    v
multistream-select --> agree on muxer protocol (/yamux/1.0.0)
    |
    v
Yamux session established
    |
    v
Stream 1: Identify exchange (/ipfs/id/1.0.0)
Stream 2+: Application protocols
```

QUIC collapses this entire pipeline into a single round trip because TLS 1.3
and native multiplexing are built into the transport itself.

## TCP Connection Example (Full Detail)

This section walks through every message exchanged between two peers, Alice
(initiator/dialer) and Bob (responder/listener), establishing a connection over
TCP with Noise security and Yamux multiplexing.

### Step 1: TCP Three-Way Handshake

```
Alice                                          Bob
  |                                              |
  |-------- SYN (seq=x) ----------------------->|
  |                                              |
  |<------- SYN-ACK (seq=y, ack=x+1) ----------|
  |                                              |
  |-------- ACK (ack=y+1) --------------------->|
  |                                              |
  |  TCP connection established                  |
```

This is standard TCP. After the three-way handshake, both sides have a
reliable, bidirectional byte stream. No libp2p data has been exchanged yet.

**Cost: 1.5 RTT** (the ACK can carry data, but typically does not in libp2p).

### Step 2: multistream-select for Security Protocol

Both peers send the multistream-select protocol identifier simultaneously,
without waiting for the other side. Messages are UTF-8 strings, each prefixed
with its length as an unsigned varint, and terminated with a newline (`\n`).

```
Alice                                          Bob
  |                                              |
  |-- "/multistream/1.0.0\n" ------------------>|
  |                                              |
  |<-- "/multistream/1.0.0\n" ------------------ |
  |                                              |
  |-- "/noise\n" ------------------------------>|
  |                                              |
  |<-- "/noise\n" ------------------------------ |
  |                                              |
  |  Security protocol agreed: Noise             |
```

**Wire bytes for `/multistream/1.0.0\n`:**

```
13 2f 6d 75 6c 74 69 73 74 72 65 61 6d 2f 31 2e 30 2e 30 0a
^^                                                         ^^
varint(19)    "/multistream/1.0.0"                         \n
```

- Varint `0x13` = 19 (length of the string including `\n`)
- `2f 6d 75 6c 74 69 73 74 72 65 61 6d 2f 31 2e 30 2e 30` = `/multistream/1.0.0`
- `0a` = `\n`

**Wire bytes for `/noise\n`:**

```
07 2f 6e 6f 69 73 65 0a
^^                   ^^
varint(7)  "/noise"  \n
```

The initiator proposes `/noise` and the responder echoes it back to confirm
agreement. If the responder did not support Noise, it would respond with `na\n`
(`03 6e 61 0a`), and the initiator would propose an alternative.

Both sides can pipeline: Alice sends `/multistream/1.0.0\n` and `/noise\n` in
the same TCP segment without waiting for Bob's response. This is called
**optimistic sending** and saves a round trip in the common case.

**Cost: 1 RTT** (with pipelining; 2 RTT without).

### Step 3: Noise XX Handshake

After both sides agree on `/noise`, the Noise XX handshake begins. All Noise
messages are framed with a 2-byte big-endian length prefix (see Chapter 5).

```
Alice (Initiator)                              Bob (Responder)
  |                                              |
  |-- Noise Msg 1: [e] ----------------------->|
  |   [2-byte len][32-byte ephemeral pubkey]     |
  |   (34 bytes total)                           |
  |                                              |
  |<-- Noise Msg 2: [e, ee, s, es] ------------ |
  |   [2-byte len][32-byte eph pubkey]           |
  |   [48-byte encrypted static pubkey]          |
  |   [encrypted payload + 16-byte tag]          |
  |   Payload contains:                          |
  |     - Bob's libp2p identity key (protobuf)   |
  |     - Signature over Noise static key        |
  |     - Extensions: supported muxers (optional)|
  |                                              |
  |-- Noise Msg 3: [s, se] ------------------->|
  |   [2-byte len]                               |
  |   [48-byte encrypted static pubkey]          |
  |   [encrypted payload + 16-byte tag]          |
  |   Payload contains:                          |
  |     - Alice's libp2p identity key (protobuf) |
  |     - Signature over Noise static key        |
  |     - Extensions: supported muxers (optional)|
  |                                              |
  |  Secure channel established                  |
  |  (Two CipherState objects for each direction)|
```

**Message sizes (approximate):**

| Message | Direction | Size |
|---------|-----------|------|
| Msg 1 | Alice -> Bob | 2 + 32 = 34 bytes |
| Msg 2 | Bob -> Alice | 2 + 32 + 48 + (payload + 16) bytes |
| Msg 3 | Alice -> Bob | 2 + 48 + (payload + 16) bytes |

The payload in Messages 2 and 3 is a protobuf-encoded `NoiseHandshakePayload`:

```protobuf
message NoiseHandshakePayload {
    optional bytes identity_key = 1;   // serialized PublicKey protobuf
    optional bytes identity_sig = 2;   // signature of "noise-libp2p-static-key:" || static_pubkey
    optional NoiseExtensions extensions = 4;
}
```

For an Ed25519 identity key, the payload is approximately 100-120 bytes. After
encryption and the authentication tag, Messages 2 and 3 are each roughly
200-250 bytes.

After the handshake, each peer validates the other's identity:

1. Decode `identity_key` into a public key
2. Verify `identity_sig` against the Noise static public key
3. Derive the Peer ID from `identity_key`
4. If dialing a specific peer, verify the Peer ID matches the expected value

From this point, all data is encrypted with ChaCha20-Poly1305. Each encrypted
message is framed as `[2-byte length][ciphertext || 16-byte auth tag]`.

**Cost: 1.5 RTT** (3 messages).

### Step 4: multistream-select for Stream Multiplexer

The muxer negotiation happens over the now-encrypted channel. The wire format
is the same as Step 2, but all bytes are encrypted before transmission.

```
Alice                                          Bob
  |                                              |
  |== encrypted ================================ |
  |                                              |
  |-- "/multistream/1.0.0\n" ------------------>|
  |                                              |
  |<-- "/multistream/1.0.0\n" ------------------ |
  |                                              |
  |-- "/yamux/1.0.0\n" ----------------------->|
  |                                              |
  |<-- "/yamux/1.0.0\n" ----------------------- |
  |                                              |
  |  Muxer agreed: Yamux                         |
```

**Wire bytes for `/yamux/1.0.0\n` (before encryption):**

```
0d 2f 79 61 6d 75 78 2f 31 2e 30 2e 30 0a
^^                                      ^^
varint(13)   "/yamux/1.0.0"             \n
```

Again, both sides can pipeline. With pipelining, this adds roughly 1 RTT.

**Cost: 1 RTT** (with pipelining).

### Step 5: Yamux Session Established

The Yamux session is now active. No explicit session-setup message is needed;
Yamux starts immediately. Streams are opened by sending a frame with the SYN
flag set.

- Alice (initiator/client) uses **odd** stream IDs: 1, 3, 5, ...
- Bob (responder/server) uses **even** stream IDs: 2, 4, 6, ...

### Step 6: Identify Exchange

Immediately after the connection is upgraded, both peers typically open a
stream to perform the Identify protocol exchange. The initiator usually opens
stream 1 for this purpose.

```
Alice                                          Bob
  |                                              |
  |== Yamux Stream 1: Identify ================ |
  |                                              |
  |-- Yamux [SYN, StreamID=1, Data] ----------->|
  |   multistream-select: "/multistream/1.0.0\n"|
  |   protocol: "/ipfs/id/1.0.0\n"              |
  |                                              |
  |<-- Yamux [ACK, StreamID=1, Data] ----------- |
  |   multistream-select: "/multistream/1.0.0\n"|
  |   protocol: "/ipfs/id/1.0.0\n"              |
  |                                              |
  |<-- Yamux [Data, StreamID=1] ---------------- |
  |   Identify message (protobuf):               |
  |     - publicKey: Bob's public key            |
  |     - listenAddrs: Bob's listen addresses    |
  |     - observedAddr: Alice's address as       |
  |       seen by Bob                            |
  |     - protocols: ["/ipfs/id/1.0.0",          |
  |       "/ipfs/id/push/1.0.0", "/ipfs/ping/   |
  |       1.0.0", ...]                           |
  |     - protocolVersion: "ipfs/0.1.0"          |
  |     - agentVersion: "go-libp2p/0.30.0"       |
  |                                              |
  |<-- Yamux [FIN, StreamID=1] ----------------- |
  |   Bob closes his side of the stream          |
  |                                              |
  |-- Yamux [FIN, StreamID=1] ----------------->|
  |   Alice closes her side                      |
```

The Identify message is a single protobuf:

```protobuf
message Identify {
    optional string protocolVersion = 5;
    optional string agentVersion = 6;
    optional bytes publicKey = 1;
    repeated bytes listenAddrs = 2;
    optional bytes observedAddr = 4;
    repeated string protocols = 3;
}
```

The responder sends the Identify message and closes the stream. The initiator
reads the message and closes its side. Both peers now know each other's
capabilities and addresses.

**Note:** In practice, Bob also opens stream 2 to query Alice's Identify
information, so both peers learn about each other.

### Step 7: Application Streams

After the Identify exchange, the connection is fully operational. Either peer
can open new streams for application protocols:

```
Alice                                          Bob
  |                                              |
  |== Yamux Stream 3: Application Protocol ===== |
  |                                              |
  |-- Yamux [SYN, StreamID=3, Data] ----------->|
  |   multistream-select for /my-app/1.0.0       |
  |                                              |
  |<-- Yamux [ACK, StreamID=3, Data] ----------- |
  |   multistream-select echo /my-app/1.0.0      |
  |                                              |
  |<============ application data =============>|
```

## Complete Sequence Diagram: TCP + Noise + Yamux

The following diagram shows every message in chronological order for a complete
connection from TCP SYN to the first application data exchange.

```
Alice (Initiator)                                            Bob (Responder)
  |                                                              |
  |  ===== TCP Three-Way Handshake =====                         |
  |                                                              |
  |-------- TCP SYN ------------------------------------------>|  RTT 0.5
  |<------- TCP SYN-ACK ----------------------------------------|  RTT 1.0
  |-------- TCP ACK ------------------------------------------->|  RTT 1.5
  |                                                              |
  |  ===== multistream-select: Security Protocol =====           |
  |  (pipelined: both messages in one TCP segment)               |
  |                                                              |
  |-- varint|"/multistream/1.0.0\n" + varint|"/noise\n" ------>|  RTT 1.5
  |<-- varint|"/multistream/1.0.0\n" + varint|"/noise\n" ------|  RTT 2.0
  |                                                              |
  |  ===== Noise XX Handshake =====                              |
  |                                                              |
  |-- [len][ephemeral_pubkey_alice]  (Noise msg 1) ----------->|  RTT 2.0
  |<-- [len][eph_bob|enc(s_bob)|enc(payload_bob)]  (msg 2) ----|  RTT 2.5
  |-- [len][enc(s_alice)|enc(payload_alice)]  (Noise msg 3) -->|  RTT 3.0
  |                                                              |
  |  ===== Encrypted from here on =====                          |
  |                                                              |
  |  ===== multistream-select: Muxer Protocol =====              |
  |  (pipelined: both messages in one encrypted frame)           |
  |                                                              |
  |-- ENC(varint|"/multistream/1.0.0\n"                         |
  |       + varint|"/yamux/1.0.0\n") ------------------------->|  RTT 3.0
  |<-- ENC(varint|"/multistream/1.0.0\n"                        |
  |       + varint|"/yamux/1.0.0\n") --------------------------|  RTT 3.5
  |                                                              |
  |  ===== Yamux Session Active =====                            |
  |                                                              |
  |  ===== Stream 1: Identify =====                              |
  |                                                              |
  |-- Yamux[SYN,ID=1] + mss(/ipfs/id/1.0.0) ----------------->|  RTT 3.5
  |<-- Yamux[ACK,ID=1] + mss(/ipfs/id/1.0.0)                   |
  |    + Yamux[Data,ID=1](Identify protobuf)                    |
  |    + Yamux[FIN,ID=1] --------------------------------------|  RTT 4.0
  |-- Yamux[FIN,ID=1] ---------------------------------------->|  RTT 4.5
  |                                                              |
  |  ===== Stream 3: Application Protocol =====                 |
  |                                                              |
  |-- Yamux[SYN,ID=3] + mss(/my-app/1.0.0) ------------------>|  RTT 4.5
  |<-- Yamux[ACK,ID=3] + mss echo + first app data ------------|  RTT 5.0
  |                                                              |
  |  ===== Application data flowing =====                        |
```

**Total RTT to first application data: ~5 RTT** (with aggressive pipelining).
Without pipelining, this can be 7-8 RTT.

Note that Noise message 1 can be pipelined with the multistream-select
messages for security negotiation, reducing the effective cost. Smart
implementations combine messages into as few TCP segments as possible.

## Early Muxer Negotiation Optimization

The standard flow requires a full multistream-select round trip for muxer
negotiation after the Noise handshake completes. The **inlined muxer
negotiation** optimization eliminates this by embedding muxer preferences in
the Noise handshake payloads.

### How It Works

The `NoiseExtensions` protobuf has a `stream_muxers` field:

```protobuf
message NoiseExtensions {
    repeated bytes webtransport_certhashes = 1;
    repeated string stream_muxers = 2;
}
```

During the Noise XX handshake:

1. **Message 2** (Responder -> Initiator): The responder includes its
   supported muxers in `extensions.stream_muxers`, ordered by preference.
2. **Message 3** (Initiator -> Responder): The initiator includes its
   supported muxers (or a single chosen muxer from the responder's list),
   ordered by preference.

The selected muxer is the first entry in the initiator's list that both peers
support.

```
Noise XX with inlined muxer negotiation:

  -> e
  <- e, ee, s, es, [ "/mplex/6.7.0", "/yamux/1.0.0" ]
  -> s, se, [ "/yamux/1.0.0" ]

  Result: /yamux/1.0.0
```

### Sequence Diagram with Optimization

```
Alice (Initiator)                                    Bob (Responder)
  |                                                      |
  |  (TCP handshake + security mss omitted)              |
  |                                                      |
  |-- Noise Msg 1: [e] ------------------------------>|
  |                                                      |
  |<-- Noise Msg 2: [e, ee, s, es] -------------------|
  |   payload.extensions.stream_muxers =                 |
  |     ["/yamux/1.0.0", "/mplex/6.7.0"]                |
  |                                                      |
  |-- Noise Msg 3: [s, se] --------------------------->|
  |   payload.extensions.stream_muxers =                 |
  |     ["/yamux/1.0.0"]                                 |
  |                                                      |
  |  Secure channel + Yamux ready immediately            |
  |  (no separate muxer negotiation needed)              |
  |                                                      |
  |-- Yamux[SYN,ID=1] + mss(/ipfs/id/1.0.0) -------->|
  |  ...                                                 |
```

**Savings: 1 RTT** (the entire multistream-select muxer negotiation step is
eliminated).

### Backward Compatibility

If the remote peer does not include `stream_muxers` in its Noise extensions,
the initiator falls back to the standard multistream-select muxer negotiation.
This makes the optimization fully backward compatible.

### Privacy Note

Unlike TLS ALPN (which exposes the muxer list in the unencrypted ClientHello),
Noise extensions are sent inside encrypted Noise handshake messages (Messages 2
and 3). An on-path observer cannot see which muxers are being negotiated.

## QUIC Connection Example

QUIC dramatically simplifies connection establishment. TLS 1.3 provides
security, and QUIC itself provides native stream multiplexing. There is no need
for multistream-select at the connection level.

### Connection Flow

```
Alice (Initiator)                                    Bob (Responder)
  |                                                      |
  |  ===== QUIC Handshake (over UDP) =====               |
  |                                                      |
  |-- QUIC Initial ---------------------------------->|
  |   (TLS ClientHello with ALPN: "libp2p")              |
  |   (crypto + connection parameters)                   |
  |                                                      |
  |<-- QUIC Handshake --------------------------------|
  |   (TLS ServerHello + EncryptedExtensions)            |
  |   (server certificate with libp2p identity key)      |
  |   (TLS Finished)                                     |
  |                                                      |
  |-- QUIC Handshake Complete ----------------------->|
  |   (TLS Finished + client certificate)                |
  |                                                      |
  |  Connection established: 1 RTT                       |
  |  Security: TLS 1.3 (verified identity keys)          |
  |  Multiplexing: QUIC native streams                   |
  |                                                      |
  |  ===== Stream 0: Identify =====                      |
  |                                                      |
  |-- QUIC stream + mss(/ipfs/id/1.0.0) ------------->|
  |<-- mss echo + Identify protobuf + FIN ------------|
  |                                                      |
  |  ===== Stream 4: Application =====                   |
  |                                                      |
  |-- QUIC stream + mss(/my-app/1.0.0) -------------->|
  |<-- mss echo + app data ---------------------------|
```

### Key Differences from TCP

| Aspect | TCP + Noise + Yamux | QUIC |
|--------|-------------------|------|
| Transport protocol | TCP (reliable stream) | UDP (QUIC packets) |
| Security setup | multistream-select + Noise XX | TLS 1.3 built into QUIC |
| Muxer setup | multistream-select + Yamux | Native QUIC streams |
| Connection-level negotiation | 2 x multistream-select | None (ALPN only) |
| Stream-level negotiation | multistream-select per stream | multistream-select per stream |
| RTT to first app data | ~5 RTT | ~2.5 RTT |
| Head-of-line blocking | Yes (TCP-level) | No (per-stream) |

### ALPN

QUIC uses the TLS ALPN extension with the protocol ID `libp2p` to identify
itself as a libp2p connection. This is set during the TLS ClientHello. No
further connection-level protocol negotiation is needed.

### Peer Authentication in QUIC

Each peer generates a self-signed X.509 certificate that contains:

1. A standard TLS key pair (e.g., ECDSA P-256)
2. A custom X.509 extension (OID `1.3.6.1.4.1.53594.1.1`) containing:
   - The libp2p public key
   - A signature binding the TLS key to the libp2p identity key

After the TLS handshake, each peer extracts the libp2p identity from the
remote certificate extension and derives the Peer ID.

### Stream-Level Protocol Negotiation

Even in QUIC, individual streams still use multistream-select for protocol
negotiation. This is because QUIC provides the raw multiplexed streams, but
does not know about libp2p application protocols.

```
QUIC Stream:
  [multistream-select header exchange]
  [protocol proposal and agreement]
  [application data]
```

## Stream Lifecycle

Streams within a Yamux session follow a well-defined lifecycle. Each stream
passes through the following states:

### Open: SYN

A stream is opened by sending a frame (Data or Window Update) with the SYN
flag set. The frame includes the new stream ID.

```hex
Yamux header (12 bytes):
00       Version: 0
00       Type: Data (0x00)
00 01    Flags: SYN (0x0001)
00 00 00 03  StreamID: 3
00 00 00 00  Length: 0
```

The responder acknowledges with a frame carrying the ACK flag:

```hex
00       Version: 0
01       Type: Window Update (0x01)
00 02    Flags: ACK (0x0002)
00 00 00 03  StreamID: 3
00 04 00 00  Length: 262144 (initial window)
```

### Negotiate: multistream-select

After the stream is open, both sides perform multistream-select to agree on
the protocol for this stream:

```
Initiator                          Responder
  |                                    |
  |-- "/multistream/1.0.0\n" -------->|
  |<-- "/multistream/1.0.0\n" --------|
  |-- "/my-protocol/1.0.0\n" -------->|
  |<-- "/my-protocol/1.0.0\n" --------|
  |                                    |
  |  Protocol agreed, data flows       |
```

### Data Exchange

Application data is sent in Yamux Data frames:

```hex
00       Version: 0
00       Type: Data (0x00)
00 00    Flags: none
00 00 00 03  StreamID: 3
00 00 00 0c  Length: 12
[12 bytes of application data]
```

Each Data frame can carry up to the available send window worth of bytes.
The maximum Noise message is 65535 bytes (2-byte length prefix), so practical
Yamux data frames are bounded by both the Yamux length field and the Noise
encryption frame size.

### Half-Close: FIN

When one side is done sending, it sends a frame with the FIN flag:

```hex
00       Version: 0
00       Type: Data (0x00)
00 04    Flags: FIN (0x0004)
00 00 00 03  StreamID: 3
00 00 00 00  Length: 0
```

After sending FIN, the peer can still **receive** data on this stream. The
stream is "half-closed" from the sender's perspective.

### Full Close

When both sides have sent FIN, the stream is fully closed:

```
Initiator                          Responder
  |                                    |
  |-- Data [FIN, StreamID=3] -------->|  Initiator done sending
  |                                    |
  |<-- Data [FIN, StreamID=3] --------|  Responder done sending
  |                                    |
  |  Stream fully closed               |
```

Both peers can now reclaim the stream's resources.

### Reset: RST

A stream can be abruptly terminated by sending a frame with the RST flag:

```hex
00       Version: 0
00       Type: Data (0x00)
00 08    Flags: RST (0x0008)
00 00 00 03  StreamID: 3
00 00 00 00  Length: 0
```

RST is used for abnormal termination: protocol errors, timeouts, or
application-level cancellation. After RST, no more data may be sent or received
on the stream.

### Stream Lifecycle State Machine

```
           SYN sent/received
                  |
                  v
            +-----------+
            |   Open    |
            +-----------+
              /       \
      FIN sent/     RST sent/
      FIN rcvd      RST rcvd
            /           \
           v             v
  +---------------+  +----------+
  |  Half-Closed  |  |  Reset   |
  +---------------+  +----------+
         |
    FIN from other side
         |
         v
  +---------------+
  |    Closed     |
  +---------------+
```

## Identify Exchange After Connection

The Identify protocol (`/ipfs/id/1.0.0`) is automatically run on every new
connection. It serves three purposes:

### 1. Exchange Peer Information

Each peer learns the other's:
- Public key and Peer ID (confirmation of the security handshake)
- Listen addresses (multiaddrs where the peer can be reached)
- Supported protocols (what the peer can handle)
- Agent version (implementation name and version)

### 2. Observed Address Discovery

The `observedAddr` field tells the initiator what source address the responder
sees. This is critical for NAT discovery:

```
Alice is behind NAT. Her local address is 192.168.1.100:12345.
She dials Bob at 203.0.113.5:4001.

Bob sees Alice's connection coming from 198.51.100.50:54321
(the NAT's external address and mapped port).

Bob sends observedAddr = /ip4/198.51.100.50/tcp/54321 in Identify.
```

Alice now knows her external address as seen by Bob. She can:
- Advertise this address to other peers
- Use it for hole-punching coordination
- Confirm whether she is behind a NAT

Multiple Identify exchanges with different peers help build confidence in the
observed address. If multiple peers report the same external address, it is
likely correct.

### 3. Protocol Discovery

The `protocols` field lists all protocols the remote peer supports. This
allows the local peer to know in advance which protocols will succeed on
this connection, without trial and error.

### Identify Push

When a peer's information changes (e.g., new listen address, new protocol
support), it can proactively notify connected peers using `identify/push`
(`/ipfs/id/push/1.0.0`). The pushing peer opens a stream, sends an Identify
message with the updated fields, and closes the stream.

## Simultaneous Open

When both peers dial each other at the same time (common during NAT hole
punching), both sides believe they are the initiator. This creates a conflict
in multistream-select, which assumes a clear initiator/responder distinction.

### The Problem

```
Alice dials Bob         Bob dials Alice
     |                        |
     v                        v
  Both send "/multistream/1.0.0\n" as initiator
  Both send "/noise\n" as initiator
  --> Deadlock: no one is acting as responder
```

### Historical Approach: multistream-select Simultaneous Open Extension (Deprecated)

The original solution used a `/libp2p/simultaneous-connect` protocol extension
in multistream-select, where both peers would exchange random 64-bit integers
to break the tie. The peer with the higher integer became the initiator.

This extension is **deprecated** as of September 2023 (see libp2p/specs #573).

### Current Approach: Coordination via DCUtR

Modern libp2p implementations handle simultaneous open through the **Direct
Connection Upgrade through Relay** (DCUtR) protocol. Instead of both peers
blindly dialing each other, they coordinate via a relay connection:

1. Both peers are connected to a relay
2. One peer (A) sends a `CONNECT` message through the relay to peer B
3. Peer B responds with a `CONNECT` message containing its observed addresses
4. Both peers simultaneously attempt direct connections with **coordinated
   roles**: the peer that initiated the DCUtR exchange acts as the dialer
   (initiator), and the other acts as the listener (responder)

This avoids the need for tie-breaking in multistream-select entirely.

### Peer ID-Based Tie Breaking

In cases where simultaneous connections do occur (e.g., both peers happen to
dial each other independently), implementations typically compare Peer IDs.
The peer with the lexicographically smaller Peer ID keeps its outbound
connection (acts as initiator), and the other peer's connection is closed or
repurposed as the responder side.

## Connection Establishment Timing

### RTT Count Comparison

```
                        TCP + Noise + Yamux          QUIC
                        ─────────────────────        ─────────
TCP/UDP handshake:       1.5 RTT (SYN/SYN-ACK/ACK)  0 RTT (UDP, no handshake)
Security negotiation:    1.0 RTT (mss for /noise)    0 RTT (built into QUIC)
Security handshake:      1.5 RTT (Noise XX: 3 msgs)  1.0 RTT (TLS 1.3: 2 msgs)
Muxer negotiation:       1.0 RTT (mss for /yamux)    0 RTT (native)
                        ─────────────────────        ─────────
Subtotal (connection):   5.0 RTT                      1.0 RTT
                        ─────────────────────        ─────────
First stream open +
 protocol negotiation:   1.0 RTT                      1.0 RTT
                        ─────────────────────        ─────────
Total to first app data: 6.0 RTT                      2.0 RTT
```

**With optimizations:**

| Optimization | Savings |
|-------------|---------|
| Pipelining mss + Noise msg 1 | -0.5 RTT |
| Pipelining mss muxer + Noise msg 3 | -0.5 RTT |
| Inlined muxer negotiation (Noise extensions) | -1.0 RTT |
| **TCP optimized total** | **~4.0 RTT** |

QUIC achieves **~2.0 RTT** to first application data with no special
optimizations needed, since the protocol was designed with minimal round trips
in mind.

### Latency Examples

| Network | RTT | TCP+Noise+Yamux (naive) | TCP (optimized) | QUIC |
|---------|-----|------------------------|-----------------|------|
| Same datacenter | 0.5 ms | 3.0 ms | 2.0 ms | 1.0 ms |
| Same region | 10 ms | 60 ms | 40 ms | 20 ms |
| Cross-continent | 100 ms | 600 ms | 400 ms | 200 ms |
| Satellite | 300 ms | 1800 ms | 1200 ms | 600 ms |

These numbers demonstrate why QUIC is strongly preferred for latency-sensitive
applications.

## Haskell Implementation Notes

### Connection Upgrade as a Composed Function

The connection upgrade pipeline maps naturally to function composition in
Haskell. Each stage transforms the connection:

```haskell
-- Type aliases for clarity
type RawConn     = Connection     -- raw TCP socket
type SecureConn  = Connection     -- encrypted via Noise
type MuxedConn   = YamuxSession   -- multiplexed via Yamux

-- The upgrade pipeline
upgradeConnection
    :: PeerId             -- expected remote peer (optional)
    -> RawConn            -- raw TCP connection
    -> IO MuxedConn       -- fully upgraded connection
upgradeConnection expectedPeer raw = do
    -- Step 1: Negotiate and establish security
    secureConn <- negotiateSecurity raw >>= performHandshake expectedPeer
    -- Step 2: Negotiate and establish muxer
    negotiateMuxer secureConn >>= initYamux
```

### Type Signature for the Upgrader

A more general type signature that captures the modularity:

```haskell
data Upgrader = Upgrader
    { securityProtocols :: [(ProtocolId, SecurityUpgrade)]
    , muxerProtocols    :: [(ProtocolId, MuxerUpgrade)]
    }

type SecurityUpgrade = RawConn -> IO SecureConn
type MuxerUpgrade    = SecureConn -> IO MuxedConn

-- Security upgrade includes negotiation + handshake
type SecurityUpgrade = Connection -> IO (Connection, PeerId)

-- Muxer upgrade includes negotiation + initialization
type MuxerUpgrade = Connection -> IO YamuxSession

-- The full pipeline
upgrade :: Upgrader -> Connection -> IO (YamuxSession, PeerId)
upgrade cfg conn = do
    -- multistream-select for security
    (secProtoId, secUpgrade) <- negotiate (securityProtocols cfg) conn
    (secConn, remotePeer)    <- secUpgrade conn

    -- multistream-select for muxer (or use inlined negotiation)
    (muxProtoId, muxUpgrade) <- negotiate (muxerProtocols cfg) secConn
    session                  <- muxUpgrade secConn

    pure (session, remotePeer)
```

### multistream-select Implementation

```haskell
-- Encode a multistream-select message
encodeMsg :: ByteString -> ByteString
encodeMsg proto =
    let withNewline = proto <> "\n"
        len = BS.length withNewline
    in encodeVarint (fromIntegral len) <> withNewline

-- The negotiation protocol
negotiate
    :: [(ProtocolId, a)]    -- supported protocols with handlers
    -> Connection           -- connection to negotiate over
    -> IO (ProtocolId, a)   -- selected protocol and its handler
negotiate supported conn = do
    -- Send and receive "/multistream/1.0.0\n"
    send conn (encodeMsg "/multistream/1.0.0")
    header <- recv conn
    when (header /= "/multistream/1.0.0") $
        throwIO NegotiationFailed

    -- Propose protocols in order of preference
    tryProtocols supported
  where
    tryProtocols [] = throwIO NoProtocolMatch
    tryProtocols ((proto, handler) : rest) = do
        send conn (encodeMsg proto)
        response <- recv conn
        if response == proto
            then pure (proto, handler)
            else tryProtocols rest  -- got "na", try next
```

### Stream Opener

```haskell
-- Open a new stream with protocol negotiation
openStream
    :: YamuxSession
    -> ProtocolId
    -> IO Stream
openStream session proto = do
    stream <- yamuxOpen session  -- sends SYN, gets ACK
    negotiate [(proto, ())] (streamToConn stream)
    pure stream
```

## Spec References

- Connection establishment: https://github.com/libp2p/specs/blob/master/connections/README.md
- Inlined muxer negotiation: https://github.com/libp2p/specs/blob/master/connections/inlined-muxer-negotiation.md
- Simultaneous open (deprecated): https://github.com/libp2p/specs/blob/master/connections/simopen.md
- Identify protocol: https://github.com/libp2p/specs/blob/master/identify/README.md
- Noise spec: https://github.com/libp2p/specs/tree/master/noise
- Yamux spec: https://github.com/hashicorp/yamux/blob/master/spec.md
- QUIC transport: https://github.com/libp2p/specs/tree/master/quic
- multistream-select: https://github.com/multiformats/multistream-select
- DCUtR (Direct Connection Upgrade through Relay): https://github.com/libp2p/specs/blob/master/relay/DCUtR.md
