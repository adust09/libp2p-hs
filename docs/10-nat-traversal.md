# Chapter 10: NAT Traversal

Network Address Translation (NAT) is the single biggest obstacle to peer-to-peer
connectivity. This chapter covers the full NAT traversal stack in libp2p: detecting
NAT status with AutoNAT, relaying connections through Circuit Relay v2, and
establishing direct connections via hole punching coordinated by DCUtR.

## The NAT Problem

### Why NAT Breaks P2P Connectivity

In a typical home or corporate network, a NAT device (router) maps private IP
addresses to a single public IP address. Outbound connections from a private host
work fine: the NAT creates a mapping from (private_ip:private_port) to
(public_ip:mapped_port) and routes return traffic back. But **inbound connections
from the public Internet fail** because there is no existing mapping to route the
traffic to the correct internal host.

This is catastrophic for P2P networks. If peer A is behind a NAT and peer B
wants to connect to A, B has no routable address to dial. The address A advertises
(e.g., `192.168.1.5:4001`) is meaningless outside A's local network.

```
                    NAT/Firewall
Peer B (public)        │          Peer A (private)
  203.0.113.5 ────X────┤          192.168.1.5
                        │
  B cannot reach A:     │    A's address is not
  no inbound mapping    │    routable from outside
```

### Types of NAT

NAT behavior varies significantly across devices, and the type of NAT determines
which traversal techniques will succeed. The classification comes from RFC 3489:

| NAT Type | Mapping Rule | Filtering Rule | Hole Punching |
|---|---|---|---|
| **Full Cone** | Same mapping for all destinations | No filtering: any external host can send to mapped port | Easy |
| **Address-Restricted Cone** | Same mapping for all destinations | Only hosts A has sent to (by IP) can reply | Moderate |
| **Port-Restricted Cone** | Same mapping for all destinations | Only hosts A has sent to (by IP:port) can reply | Moderate |
| **Symmetric** | Different mapping per destination | Only the specific destination can reply | Very difficult |

**Full Cone (Endpoint-Independent Mapping):** Once a mapping is created, any
external host can send packets to the mapped address. This is the easiest to
traverse.

**Address-Restricted Cone:** The NAT only forwards inbound packets from an IP
address that the internal host has previously sent a packet to. Hole punching
works by having both peers send packets to each other, creating the necessary
mappings.

**Port-Restricted Cone:** Like address-restricted, but the restriction also
applies to the source port. The internal host must have sent a packet to the
specific IP:port combination. Hole punching still works with proper coordination.

**Symmetric NAT:** Creates a different mapping for each destination. This makes
hole punching extremely difficult because the mapped port used when talking to a
relay is different from the mapped port that would be used for a direct connection
to the other peer. The predicted external address is unreliable.

In practice, libp2p's hole punching works well for cone NATs and has limited
success with symmetric NATs. When hole punching fails, peers fall back to relayed
connections.

## AutoNAT Protocol

AutoNAT allows a node to determine whether it is publicly reachable or behind a
NAT. The node asks other peers to dial its addresses; if they succeed, the node
is public. If they fail, the node is behind a NAT.

### AutoNAT v1

#### Protocol ID

```
/libp2p/autonat/1.0.0
```

#### How It Works

1. Node A wants to know its NAT status.
2. A opens a stream to a peer B using `/libp2p/autonat/1.0.0`.
3. A sends a `Dial` message containing a list of its addresses.
4. B dials those addresses (restricted to A's observed IP for security).
5. B responds with a `DialResponse` indicating success or failure.
6. A repeats with multiple peers and uses a threshold to determine status.

If more than 3 peers report a successfully dialed address, the node assumes it is
publicly reachable. If more than 3 peers report unsuccessful dials, the node
assumes it is behind a NAT.

#### Security Constraints

To prevent amplification attacks (as described in RFC 3489, Section 12.1.1), the
server MUST NOT dial any multiaddress unless it is based on the IP address the
requesting node is observed as. This also means implementations MUST NOT accept
dial requests via relayed connections, since the true IP of the requesting node
cannot be validated.

#### Protobuf Definition

```protobuf
syntax = "proto2";

message Message {
    enum MessageType {
        DIAL          = 0;
        DIAL_RESPONSE = 1;
    }

    enum ResponseStatus {
        OK              = 0;
        E_DIAL_ERROR    = 100;
        E_DIAL_REFUSED  = 101;
        E_BAD_REQUEST   = 200;
        E_INTERNAL_ERROR = 300;
    }

    message PeerInfo {
        optional bytes  id    = 1;
        repeated bytes  addrs = 2;
    }

    message Dial {
        optional PeerInfo peer = 1;
    }

    message DialResponse {
        optional ResponseStatus status     = 1;
        optional string         statusText = 2;
        optional bytes          addr       = 3;
    }

    optional MessageType  type         = 1;
    optional Dial         dial         = 2;
    optional DialResponse dialResponse = 3;
}
```

#### Sequence Diagram

```
Node A                              Node B (AutoNAT server)
  │                                       │
  │  ── stream: /libp2p/autonat/1.0.0 ──► │
  │                                       │
  │  ── Dial { peer: {                    │
  │       id: QmA,                        │
  │       addrs: [/ip4/203.0.113.5/tcp/4001]  │
  │     }} ──────────────────────────────► │
  │                                       │
  │                           B dials A's │
  │                           addresses   │
  │                           (same IP    │
  │                            only)      │
  │                                       │
  │  ◄── DialResponse {                   │
  │        status: OK,                    │
  │        addr: /ip4/203.0.113.5/tcp/4001│
  │      } ────────────────────────────── │
  │                                       │
  │  (repeat with 3+ peers to confirm)    │
```

All RPC messages are prefixed with the message length in bytes, encoded as an
unsigned variable-length integer per the
[multiformats unsigned-varint spec](https://github.com/multiformats/unsigned-varint).

### AutoNAT v2

AutoNAT v2 improves on v1 by allowing nodes to test **individual addresses** for
reachability, rather than testing reachability of the node as a whole.

#### Protocol IDs

```
/libp2p/autonat/2/dial-request
/libp2p/autonat/2/dial-back
```

#### Key Differences from v1

1. **Per-address testing:** The server dials exactly one address from a priority-
   ordered list, enabling the client to determine reachability per address.
2. **Nonce verification:** The client sends a `nonce` in the request. The server
   sends this nonce back on the `/libp2p/autonat/2/dial-back` stream, proving it
   actually connected to the client's address.
3. **Amplification attack prevention:** If the server is asked to dial an address
   with a different IP than the client's observed IP, the server requires the
   client to send 30k-100k bytes of data first, making amplification attacks
   unattractive.

#### Protobuf Definition

```protobuf
syntax = "proto3";

message Message {
    oneof msg {
        DialRequest      dialRequest      = 1;
        DialResponse     dialResponse     = 2;
        DialDataRequest  dialDataRequest  = 3;
        DialDataResponse dialDataResponse = 4;
    }
}

message DialRequest {
    repeated bytes addrs = 1;
    fixed64        nonce = 2;
}

message DialDataRequest {
    uint32 addrIdx  = 1;
    uint64 numBytes = 2;
}

enum DialStatus {
    UNUSED           = 0;
    E_DIAL_ERROR     = 100;
    E_DIAL_BACK_ERROR = 101;
    OK               = 200;
}

message DialResponse {
    enum ResponseStatus {
        E_INTERNAL_ERROR  = 0;
        E_REQUEST_REJECTED = 100;
        E_DIAL_REFUSED    = 101;
        OK                = 200;
    }

    ResponseStatus status     = 1;
    uint32         addrIdx    = 2;
    DialStatus     dialStatus = 3;
}

message DialDataResponse {
    bytes data = 1;
}

message DialBack {
    fixed64 nonce = 1;
}

message DialBackResponse {
    enum DialBackStatus {
        OK = 0;
    }

    DialBackStatus status = 1;
}
```

#### Sequence Diagram

```
Client A                                Server B
  │                                          │
  │  ── stream: /libp2p/autonat/2/dial-request ──► │
  │                                          │
  │  ── DialRequest {                        │
  │       addrs: [addr1, addr2, ...],        │
  │       nonce: 0xABCD1234                  │
  │     } ──────────────────────────────────►│
  │                                          │
  │                         B selects first  │
  │                         dialable addr    │
  │                                          │
  │     [If selected addr has different IP   │
  │      than A's observed IP:]              │
  │                                          │
  │  ◄── DialDataRequest {                   │
  │        addrIdx: 0,                       │
  │        numBytes: 30000                   │
  │      } ──────────────────────────────── │
  │                                          │
  │  ── DialDataResponse { data: [...] } ──►│
  │  ── DialDataResponse { data: [...] } ──►│
  │  ── ...  (until numBytes sent) ────────►│
  │                                          │
  │                     B dials selected addr│
  │                                          │
  │  ◄─── (new conn) /libp2p/autonat/2/dial-back ──│
  │  ◄── DialBack { nonce: 0xABCD1234 } ─── │
  │  ── DialBackResponse { status: OK } ───►│
  │                                          │
  │  ◄── DialResponse {                      │
  │        status: OK,                       │
  │        addrIdx: 0,                       │
  │        dialStatus: OK                    │
  │      } ──────────────────────────────── │
  │                                          │
  │  Client verifies nonce matches,          │
  │  confirms addr[0] is reachable.          │
```

## Circuit Relay v2

When a peer is behind a NAT and cannot be reached directly, it can use a relay
peer to receive connections. Circuit Relay v2 provides **limited** relay service
with explicit resource reservations, designed for short-lived connections that
facilitate hole punching rather than long-term proxying.

### Protocol IDs

```
/libp2p/circuit/relay/0.2.0/hop   (client ↔ relay)
/libp2p/circuit/relay/0.2.0/stop  (relay ↔ target)
```

The protocol is split into two subprotocols:

- **Hop protocol:** Client-initiated, used for reserving resources in the relay
  and opening a switched connection to a peer through the relay.
- **Stop protocol:** Governs connection termination between the relay and the
  target peer.

### Relay Address Format

A relay address encodes the path through a relay peer to reach a target peer:

```
/ip4/198.51.100.1/tcp/4001/p2p/QmRelay/p2p-circuit/p2p/QmTarget
```

The format is:

```
<relay-transport-addr>/p2p/<relay-peer-id>/p2p-circuit/p2p/<target-peer-id>
```

The `p2p-circuit` component signals that the connection should be routed through
the relay rather than established directly.

### Protobuf Definition

```protobuf
syntax = "proto3";

message HopMessage {
    enum Type {
        RESERVE = 0;
        CONNECT = 1;
        STATUS  = 2;
    }

    optional Type        type        = 1;
    optional Peer        peer        = 2;
    optional Reservation reservation = 3;
    optional Limit       limit       = 4;
    optional Status      status      = 5;
}

message StopMessage {
    enum Type {
        CONNECT = 0;
        STATUS  = 1;
    }

    optional Type   type   = 1;
    optional Peer   peer   = 2;
    optional Limit  limit  = 3;
    optional Status status = 4;
}

message Peer {
    optional bytes  id    = 1;
    repeated bytes  addrs = 2;
}

message Reservation {
    optional uint64 expire  = 1;  // Unix expiration time (UTC)
    repeated bytes  addrs   = 2;  // relay addrs for reserving peer
    optional bytes  voucher = 3;  // reservation voucher
}

message Limit {
    optional uint32 duration = 1;  // seconds
    optional uint64 data     = 2;  // bytes
}

enum Status {
    UNUSED                  = 0;
    OK                      = 100;
    RESERVATION_REFUSED     = 200;
    RESOURCE_LIMIT_EXCEEDED = 201;
    PERMISSION_DENIED       = 202;
    CONNECTION_FAILED       = 203;
    NO_RESERVATION          = 204;
    MALFORMED_MESSAGE       = 400;
    UNEXPECTED_MESSAGE      = 401;
}
```

### Reservation Voucher

Successful reservations come with a **Reservation Voucher**, a
[Signed Envelope](https://github.com/libp2p/specs/blob/master/RFC/0002-signed-envelopes.md)
with domain `libp2p-relay-rsvp` and multicodec code `0x0302`. The payload:

```protobuf
syntax = "proto3";

message Voucher {
    optional bytes  relay      = 1;  // Peer ID of the relay
    optional bytes  peer       = 2;  // Peer ID of the reserving peer
    optional uint64 expiration = 3;  // UNIX UTC expiration time
}
```

The wire representation is canonicalized: fields are written in field ID order
with no unknown fields.

### Reservation Flow

A private peer reserves a relay slot to make itself reachable via the relay.

```
Private Peer A                          Relay R
     │                                       │
     │  ── stream: .../hop ─────────────────►│
     │                                       │
     │  ── HopMessage {                      │
     │       type: RESERVE                   │
     │     } ───────────────────────────────►│
     │                                       │
     │  ◄── HopMessage {                     │
     │        type: STATUS,                  │
     │        status: OK,                    │
     │        reservation: Reservation {     │
     │          expire: 1700000000,          │
     │          addrs: [/ip4/.../p2p/QmR],   │
     │          voucher: <signed envelope>   │
     │        },                             │
     │        limit: Limit {                 │
     │          duration: 120,               │
     │          data: 131072                 │
     │        }                              │
     │      } ──────────────────────────────│
     │                                       │
     │  A keeps connection alive.            │
     │  A constructs relay addrs:            │
     │   /ip4/.../p2p/QmR/p2p-circuit/p2p/QmA │
     │  A advertises these addrs.            │
     │                                       │
     │  (reservation timeout approaching)    │
     │                                       │
     │  ── HopMessage { type: RESERVE } ───►│
     │  ◄── HopMessage { type: STATUS,      │
     │        status: OK, ... } ────────────│
```

Key points about reservations:

- The reservation remains valid **as long as the connection to the relay is
  maintained**. If the peer disconnects, the reservation is invalidated.
- The `addrs` field in the `Reservation` contains the relay's addresses (without
  the trailing `p2p-circuit` part). The client appends
  `/p2p-circuit/p2p/<own-peer-id>` to construct its full relay address.
- The client is responsible for refreshing the reservation before `expire`.

### Resource Limits

Circuit Relay v2 enforces resource limits to prevent abuse:

| Resource | Field | Description |
|---|---|---|
| **Duration** | `Limit.duration` | Maximum time (seconds) a relayed connection can remain open. 0 = unlimited. |
| **Data** | `Limit.data` | Maximum bytes allowed in each direction. 0 = unlimited. |

If the data limit is exceeded or the duration expires, the relay resets both the
source and destination streams.

Implementations SHOULD NOT accept reservations or connection initiations over
already-relayed connections (to prevent relay chaining).

### Relay Connection Flow

When peer B wants to connect to peer A through relay R:

```
Peer B                       Relay R                       Peer A
  │                              │                              │
  │  ── stream: .../hop ────────►│                              │
  │                              │                              │
  │  ── HopMessage {             │                              │
  │       type: CONNECT,         │                              │
  │       peer: { id: QmA }     │                              │
  │     } ─────────────────────►│                              │
  │                              │                              │
  │                              │  ── stream: .../stop ──────►│
  │                              │                              │
  │                              │  ── StopMessage {            │
  │                              │       type: CONNECT,         │
  │                              │       peer: { id: QmB },    │
  │                              │       limit: Limit {         │
  │                              │         duration: 120,       │
  │                              │         data: 131072         │
  │                              │       }                      │
  │                              │     } ─────────────────────►│
  │                              │                              │
  │                              │  ◄── StopMessage {           │
  │                              │        type: STATUS,         │
  │                              │        status: OK            │
  │                              │      } ────────────────────│
  │                              │                              │
  │  ◄── HopMessage {            │                              │
  │        type: STATUS,         │                              │
  │        status: OK,           │                              │
  │        limit: Limit {        │                              │
  │          duration: 120,      │                              │
  │          data: 131072        │                              │
  │        }                     │                              │
  │      } ────────────────────│                              │
  │                              │                              │
  │  ◄═══════ relayed connection (bridged streams) ══════════►│
  │  (hop stream)                │              (stop stream)   │
  │                              │                              │
  │  B and A upgrade the relayed connection with a              │
  │  security protocol and multiplexer, just as they            │
  │  would upgrade a direct TCP connection.                     │
```

## Hole Punching

Hole punching is the process of establishing a direct connection between two
peers, one or both of which are behind NATs, by exploiting the NAT mapping
behavior. The libp2p hole punching process has two phases.

### Two-Phase Process Overview

```
 Phase I: Discovery & Relay          Phase II: Direct Connection
 ┌────────────────────────┐          ┌────────────────────────────┐
 │                        │          │                            │
 │  1. AutoNAT: detect    │          │  4. DCUtR: coordinate      │
 │     NAT status         │          │     hole punch over        │
 │                        │          │     relay connection       │
 │  2. Reserve relay slot │          │                            │
 │     (Circuit Relay v2) │   ────►  │  5. Simultaneous connect:  │
 │                        │          │     both peers dial each   │
 │  3. Establish relayed  │          │     other at the same time │
 │     connection         │          │                            │
 │                        │          │  6. Upgrade to direct      │
 │                        │          │     connection             │
 └────────────────────────┘          └────────────────────────────┘
```

### Phase I: NAT Detection and Relay Connection

1. **NAT detection:** Peer A uses AutoNAT (v1 or v2) to determine that it is
   behind a NAT and not publicly reachable.

2. **Relay reservation:** A finds a publicly reachable relay peer R and makes a
   reservation using Circuit Relay v2. A now has a relay address:
   `/p2p/QmR/p2p-circuit/p2p/QmA`.

3. **Address advertisement:** A advertises its relay address through a discovery
   mechanism (DHT, rendezvous, etc.).

4. **Relay connection:** When peer B wants to connect to A, it discovers A's
   relay address and establishes a relayed connection through R.

### Phase II: DCUtR Coordination and Simultaneous Connect

5. **Connection observation:** Upon receiving the relayed connection from B, peer
   A examines B's addresses from the Identify protocol. If B has public
   addresses, A first attempts a unilateral direct connection. If that fails (or
   B is also behind a NAT), A initiates the DCUtR protocol.

6. **DCUtR exchange:** A and B exchange `Connect` messages containing their
   observed external addresses and a `Sync` message to synchronize timing.

7. **Simultaneous open:** Both peers dial each other at the predicted external
   addresses simultaneously. For TCP, this results in a TCP Simultaneous Open.
   For QUIC, A dials B while B sends UDP packets to punch the NAT.

8. **Upgrade:** If a direct connection is established, both peers migrate to it
   and close the relay connection after a grace period.

## DCUtR Protocol

Direct Connection Upgrade through Relay (DCUtR) is the coordination protocol
that enables hole punching. It runs over an existing relayed connection and
synchronizes both peers to attempt simultaneous direct connections.

### Protocol ID

```
/libp2p/dcutr
```

### Protobuf Definition

```protobuf
syntax = "proto2";

package holepunch.pb;

message HolePunch {
    enum Type {
        CONNECT = 100;
        SYNC    = 300;
    }

    required Type  type     = 1;
    repeated bytes ObsAddrs = 2;
}
```

`ObsAddrs` is a list of multiaddrs encoded in the binary multiaddr
representation.

All RPC messages are prefixed with the message length in bytes, encoded as an
unsigned variable-length integer per the
[multiformats unsigned-varint spec](https://github.com/multiformats/unsigned-varint).
Implementations SHOULD refuse encoded RPC messages (length prefix excluded)
larger than 4 KiB.

### Message Flow

The protocol uses three messages: two `CONNECT` messages and one `SYNC` message.

1. **B sends Connect:** The inbound peer (B, the one that received the relayed
   connection) opens a DCUtR stream and sends a `CONNECT` message containing its
   observed/predicted addresses. B starts a timer to measure the relay RTT.

2. **A responds with Connect:** A sends back a `CONNECT` message with its own
   observed/predicted addresses.

3. **B sends Sync:** Upon receiving A's `CONNECT`, B calculates the relay RTT
   and sends a `SYNC` message. B starts a timer for half the RTT.

4. **Simultaneous dial:** Both peers attempt direct connections:
   - **A** dials B's addresses immediately upon receiving `SYNC`.
   - **B** dials A's addresses when the half-RTT timer expires.

The half-RTT timer compensates for the relay latency: B's `SYNC` takes
approximately RTT/2 to reach A through the relay. By the time A receives `SYNC`
and starts dialing, approximately RTT/2 has passed. If B also starts dialing
after RTT/2, both peers begin their dial attempts at approximately the same
wall-clock time.

### Transport-Specific Behavior

**TCP:**
- Both peers dial each other simultaneously, resulting in a TCP Simultaneous
  Open. For all protocols layered on top, A is the client and B is the server.

**QUIC:**
- Upon receiving `SYNC`, A immediately dials B.
- Upon RTT/2 timer expiry, B sends UDP packets filled with random bytes to A's
  address at random intervals between 10-200ms. This punches a hole in B's NAT
  for A's QUIC connection to traverse.
- A is the QUIC client, B is the QUIC server.

### Retry Behavior

The inbound peer (B) SHOULD retry twice (for a total of 3 attempts) before
considering the upgrade failed. Each retry performs a fresh `CONNECT`/`SYNC`
exchange to get a new RTT measurement, preventing a flawed measurement on the
first attempt from distorting subsequent retries.

### DCUtR Sequence Diagram

```
Peer A (initiator)                    Relay                    Peer B (inbound)
     │                                  │                           │
     │  ◄════ relayed connection (via Circuit Relay v2) ═══════════►│
     │                                  │                           │
     │                                  │   B examines A's addrs    │
     │                                  │   from Identify.          │
     │                                  │   A is behind NAT too.    │
     │                                  │   B initiates DCUtR.      │
     │                                  │                           │
     │  ◄── stream: /libp2p/dcutr (over relayed conn) ────────────│
     │                                  │                           │
     │  ◄── HolePunch {                 │              t0: B sends  │
     │        type: CONNECT,            │              Connect,     │
     │        ObsAddrs: [B_addr1, ...]  │              starts RTT   │
     │      } ─────────────────────────────────────────────────── │
     │                                  │                           │
     │  ── HolePunch {                  │                           │
     │       type: CONNECT,             │                           │
     │       ObsAddrs: [A_addr1, ...]   │                           │
     │     } ─────────────────────────────────────────────────────►│
     │                                  │              t1: B gets   │
     │                                  │              A's Connect. │
     │                                  │              RTT=t1-t0.   │
     │                                  │                           │
     │  ◄── HolePunch {                 │              B sends Sync.│
     │        type: SYNC                │              B starts     │
     │      } ─────────────────────────────────── timer: RTT/2.    │
     │                                  │                           │
     │  A receives SYNC.                │                           │
     │  A dials B_addr1                 │                           │
     │  immediately.                    │              (RTT/2 later)│
     │                                  │              B dials      │
     │                                  │              A_addr1.     │
     │                                  │                           │
     │  ═══════════ direct connection established ═════════════════►│
     │                                  │                           │
     │  Migrate streams to direct conn. │                           │
     │  Close relay connection after    │                           │
     │  grace period.                   │                           │
```

## End-to-End Hole Punching Walkthrough

The following walks through the complete sequence from a node discovering it is
behind a NAT all the way to establishing a direct connection with another NATed
peer.

### Complete Sequence

```
Step 1: NAT Detection (AutoNAT)
────────────────────────────────────────────────────────────────────────

  Peer A                         Peers X, Y, Z (public)
    │                                    │
    │  ── /libp2p/autonat/1.0.0 ───────►│
    │  ── Dial { addrs: [A's addrs] } ─►│
    │  ◄── DialResponse { E_DIAL_ERROR } │
    │                                    │
    │  (repeated with X, Y, Z)           │
    │                                    │
    │  Result: A is behind NAT.          │


Step 2: Relay Reservation (Circuit Relay v2)
────────────────────────────────────────────────────────────────────────

  Peer A                              Relay R (public)
    │                                       │
    │  ── /libp2p/circuit/relay/0.2.0/hop ─►│
    │  ── RESERVE ─────────────────────────►│
    │  ◄── STATUS: OK, Reservation {...} ───│
    │                                       │
    │  A's relay addr:                      │
    │  /ip4/.../tcp/4001/p2p/QmR/p2p-circuit/p2p/QmA
    │                                       │
    │  A advertises relay addr via DHT.     │


Step 3: Relay Connection
────────────────────────────────────────────────────────────────────────

  Peer B           Relay R                          Peer A
    │                 │                                │
    │  B discovers A's relay addr via DHT.             │
    │                 │                                │
    │  ── CONNECT ──►│                                │
    │                 │  ── CONNECT ──────────────────►│
    │                 │  ◄── STATUS: OK ──────────────│
    │  ◄── STATUS: OK │                                │
    │                 │                                │
    │  ◄═══════ relayed connection ═══════════════════►│
    │                 │                                │
    │  B and A perform security handshake and          │
    │  multiplexer negotiation over relayed conn.      │


Step 4: DCUtR Hole Punch Coordination
────────────────────────────────────────────────────────────────────────

  Peer B (inbound)                      Peer A (initiator)
    │                                       │
    │  (over relayed connection)            │
    │                                       │
    │  ── /libp2p/dcutr ──────────────────►│
    │  ── CONNECT { ObsAddrs: [B1, B2] } ─►│
    │  ◄── CONNECT { ObsAddrs: [A1, A2] } ─│
    │  ── SYNC ────────────────────────────►│
    │                                       │
    │  Start timer: RTT/2     A dials B1, B2│
    │  Timer fires: dial A1, A2    immediately│
    │                                       │


Step 5: Direct Connection
────────────────────────────────────────────────────────────────────────

  Peer B                                Peer A
    │                                       │
    │  TCP Simultaneous Open succeeds       │
    │  (or QUIC connection established)     │
    │                                       │
    │  ◄═══════ direct connection ═════════►│
    │                                       │
    │  Security handshake + muxer on        │
    │  direct connection.                   │
    │                                       │
    │  Migrate to direct connection.        │
    │  Close relay connection.              │
```

### ASCII Overview: Complete Architecture

```
┌──────────────────────────────────────────────────────────────────┐
│                     NAT Traversal Stack                          │
├──────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────┐    ┌─────────────────┐    ┌──────────────────┐ │
│  │  AutoNAT    │    │ Circuit Relay v2 │    │     DCUtR        │ │
│  │             │    │                 │    │                  │ │
│  │ "Am I       │───►│ "Route through  │───►│ "Upgrade to      │ │
│  │  reachable?"│    │  a relay peer"  │    │  direct conn"    │ │
│  │             │    │                 │    │                  │ │
│  │ /libp2p/    │    │ .../hop         │    │ /libp2p/dcutr    │ │
│  │ autonat/    │    │ .../stop        │    │                  │ │
│  │ 1.0.0       │    │                 │    │ CONNECT/SYNC     │ │
│  │             │    │ RESERVE/CONNECT │    │ msgs for         │ │
│  │ Dial/       │    │ msgs + Limits   │    │ synchronized     │ │
│  │ DialResponse│    │                 │    │ hole punch       │ │
│  └─────────────┘    └─────────────────┘    └──────────────────┘ │
│                                                                  │
│  Phase I: Detection ──────── Phase I: Relay ──── Phase II: Punch │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

## Haskell Implementation Notes

### Key Types and Architecture

A Haskell implementation of the NAT traversal stack would involve the following
key types and components:

- **AutoNAT service:** A background service that periodically probes peers to
  determine reachability status. The core state can be modeled as:
  - `NATStatus`: An enumeration of `Public`, `Private`, or `Unknown`.
  - A threshold counter tracking successful/failed dial-back reports.

- **Circuit Relay client:** Manages relay reservations and relayed connections:
  - `Reservation`: Tracks expiry time, relay addresses, and voucher.
  - `RelayedConnection`: A connection wrapper that bridges the hop/stop streams.
  - Reservation refresh logic using a timer or background thread.

- **DCUtR coordinator:** Implements the hole punch coordination:
  - RTT measurement between `CONNECT` send and receive.
  - Timer-based synchronization for simultaneous dial.
  - Retry logic (up to 3 attempts).

### Relevant Packages

- **Protobuf encoding:** `proto-lens` or `protobuf` for encoding/decoding the
  protocol messages.
- **Networking:** `network` package for TCP socket operations. Port reuse
  (`SO_REUSEADDR` / `SO_REUSEPORT`) is critical for TCP hole punching.
- **Concurrency:** `async` for managing simultaneous dial attempts and background
  services (reservation refresh, AutoNAT probing).
- **Multiaddr:** A multiaddr library for parsing and constructing circuit relay
  addresses.
- **Signed envelopes:** Implementation of the signed envelope format for
  reservation vouchers (see
  [RFC 0002](https://github.com/libp2p/specs/blob/master/RFC/0002-signed-envelopes.md)).

### Implementation Order

A recommended implementation order:

1. **AutoNAT v1** -- simplest, provides the foundation for NAT detection.
2. **Circuit Relay v2 client** -- reservation and relayed connections (client
   side only; implementing relay server functionality is optional).
3. **DCUtR** -- hole punch coordination, depends on both AutoNAT and relay.
4. **AutoNAT v2** -- per-address reachability testing with nonce verification.

## Spec References

- AutoNAT v1: https://github.com/libp2p/specs/blob/master/autonat/autonat-v1.md
- AutoNAT v2: https://github.com/libp2p/specs/blob/master/autonat/autonat-v2.md
- Circuit Relay v2: https://github.com/libp2p/specs/blob/master/relay/circuit-v2.md
- DCUtR: https://github.com/libp2p/specs/blob/master/relay/DCUtR.md
- Hole Punching overview: https://github.com/libp2p/specs/blob/master/connections/hole-punching.md
- Signed Envelopes (RFC 0002): https://github.com/libp2p/specs/blob/master/RFC/0002-signed-envelopes.md
- Ford & Srisuresh, "Peer-to-Peer Communication Across NATs": https://pdos.csail.mit.edu/papers/p2pnat.pdf
- ICE (RFC 5245): https://tools.ietf.org/html/rfc5245
