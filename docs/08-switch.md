# Chapter 8: Switch / Swarm

The Switch (called Swarm in Go implementations) is the central coordinator of
the libp2p networking stack. It owns all transports, manages connections to
peers, drives the connection upgrade pipeline, dispatches incoming streams to
protocol handlers, and enforces resource limits. Every outbound dial and every
inbound accept flows through the Switch. This chapter specifies its
responsibilities, internal state, and the algorithms that govern connection
establishment, reuse, and teardown.

## Switch as Central Coordinator

The Switch sits between the transport layer below and the application protocols
above. It is the single entry point for all network operations.

```
┌─────────────────────────────────────────────────────────────┐
│                   Application Protocols                      │
│          (Identify, Ping, Kademlia, GossipSub, ...)          │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│                         Switch                               │
│                                                              │
│  ┌──────────┐ ┌──────────┐ ┌───────────┐ ┌──────────────┐  │
│  │ Transport│ │Connection│ │  Stream    │ │  Protocol    │  │
│  │ Manager  │ │  Pool    │ │  Manager   │ │  Registry    │  │
│  └──────────┘ └──────────┘ └───────────┘ └──────────────┘  │
│  ┌──────────┐ ┌──────────┐ ┌───────────┐                   │
│  │  Dialer  │ │ Listener │ │  Resource  │                   │
│  │          │ │ Manager  │ │  Manager   │                   │
│  └──────────┘ └──────────┘ └───────────┘                   │
│                                                              │
├─────────────────────────────────────────────────────────────┤
│              Transports (TCP, QUIC, WebSocket, ...)           │
└─────────────────────────────────────────────────────────────┘
```

### Responsibilities

| Responsibility | Description |
|----------------|-------------|
| Transport management | Register transports, select the correct transport for a given multiaddr |
| Dialing | Resolve addresses, dial peers, handle parallel attempts, manage backoff |
| Listening | Bind transports to listen addresses, accept inbound connections |
| Connection upgrading | Drive the upgrade pipeline: security negotiation then muxer negotiation |
| Connection pool | Track active connections per peer, reuse existing connections |
| Stream management | Open new streams on existing connections, accept inbound streams |
| Protocol dispatch | Route inbound streams to registered protocol handlers via multistream-select |
| Resource management | Enforce limits on connections, streams, file descriptors, and memory |
| Connection gating | Allow or deny connections based on policy (IP, Peer ID, etc.) |
| Event notification | Emit events for connection open/close, stream open/close |

### Naming: Switch vs Swarm

The terms "Switch" and "Swarm" refer to the same component. The libp2p
specification uses "Switch." Go's implementation calls it "Swarm." Rust calls
it "Swarm." JavaScript historically used "Switch" then moved to "Swarm." This
chapter uses "Switch" to align with the specification, but the terms are
interchangeable.

## Connection Upgrading Pipeline

When a raw transport connection is established (e.g., a TCP socket), it must be
upgraded to a secure, multiplexed connection before application protocols can
use it. The upgrade pipeline transforms a raw byte stream into a fully capable
libp2p connection.

### Full Upgrade Sequence

```
 Raw Transport Connection (e.g., TCP socket)
         │
         ▼
 ┌───────────────────────────────────────┐
 │  Step 1: multistream-select           │
 │  Negotiate security protocol          │
 │  Dialer proposes: /noise              │
 │  Listener responds: /noise (or na)    │
 └───────────────────────────────────────┘
         │
         ▼
 ┌───────────────────────────────────────┐
 │  Step 2: Security Handshake           │
 │  Perform Noise XX (3 messages)        │
 │  or TLS 1.3 handshake                 │
 │  Result: encrypted, authenticated     │
 │          channel + remote Peer ID     │
 └───────────────────────────────────────┘
         │
         ▼
 ┌───────────────────────────────────────┐
 │  Step 3: multistream-select           │
 │  Negotiate muxer protocol             │
 │  Dialer proposes: /yamux/1.0.0        │
 │  Listener responds: /yamux/1.0.0      │
 │  (runs over the encrypted channel)    │
 └───────────────────────────────────────┘
         │
         ▼
 ┌───────────────────────────────────────┐
 │  Step 4: Muxer Initialization         │
 │  Initialize Yamux session             │
 │  Connection is now multiplexed        │
 │  Streams can be opened/accepted       │
 └───────────────────────────────────────┘
         │
         ▼
 Upgraded Connection (secure + multiplexed)
```

### Step-by-Step Detail

**Step 1: Security Protocol Negotiation (Chapter 7)**

On the raw transport byte stream, both sides run multistream-select. The dialer
proposes one or more security protocols. The listener selects one it supports.

```
Dialer                              Listener
  │                                    │
  │── "/multistream/1.0.0\n" ────────►│
  │◄── "/multistream/1.0.0\n" ────────│
  │                                    │
  │── "/noise\n" ────────────────────►│
  │◄── "/noise\n" ────────────────────│  (agreed)
  │                                    │
```

If the listener does not support the proposed protocol, it responds with `na`.
The dialer may propose alternatives. If no agreement is reached, the connection
is closed.

Supported security protocols:

| Protocol ID | Description |
|-------------|-------------|
| `/noise` | Noise XX (recommended for TCP) |
| `/tls/1.0.0` | TLS 1.3 |

**Step 2: Security Handshake (Chapter 5)**

Once a security protocol is agreed, the handshake proceeds over the raw
transport stream. For Noise XX, this is a 3-message handshake (1.5 RTT) that
produces an encrypted, authenticated channel. For TLS 1.3, this is a standard
TLS handshake (1 RTT).

After the security handshake:
- Both sides know the remote peer's identity (Peer ID)
- All subsequent data is encrypted and authenticated
- The dialer can verify the remote Peer ID matches expectations

**Step 3: Muxer Protocol Negotiation (Chapter 7)**

Over the now-encrypted channel, both sides run multistream-select again. The
dialer proposes a stream multiplexer. The listener selects one it supports.

```
Dialer                              Listener
  │                                    │
  │── "/multistream/1.0.0\n" ────────►│   (encrypted)
  │◄── "/multistream/1.0.0\n" ────────│   (encrypted)
  │                                    │
  │── "/yamux/1.0.0\n" ──────────────►│   (encrypted)
  │◄── "/yamux/1.0.0\n" ──────────────│   (encrypted, agreed)
  │                                    │
```

Supported muxer protocols:

| Protocol ID | Description |
|-------------|-------------|
| `/yamux/1.0.0` | Yamux (recommended) |
| `/mplex/6.7.0` | mplex (deprecated) |

**Step 4: Muxer Initialization (Chapter 6)**

After muxer agreement, the Yamux session is initialized. The dialer takes the
client role (odd stream IDs), the listener takes the server role (even stream
IDs). The connection is now fully upgraded and ready for application streams.

### Early Muxer Negotiation (Noise Extensions Optimization)

The standard upgrade pipeline requires two rounds of multistream-select: one
for security and one for the muxer. The Noise handshake payload includes an
optional `extensions` field (see Chapter 5) that can carry muxer preferences,
eliminating the second multistream-select round-trip.

```
Standard Pipeline (3 round-trips for negotiation):
  multistream-select (security)   1 RTT
  Noise XX handshake              1.5 RTT
  multistream-select (muxer)      1 RTT
  ─────────────────────────────
  Total:                          3.5 RTT

With Early Muxer Negotiation (2 round-trips):
  multistream-select (security)   1 RTT
  Noise XX handshake              1.5 RTT  (muxer piggybacked in payload)
  ─────────────────────────────
  Total:                          2.5 RTT
```

**How it works:**

1. During the Noise handshake, each peer includes its supported muxers in the
   `NoiseExtensions.stream_muxers` field of the handshake payload (messages 2
   and 3).
2. The initiator sends its supported muxers in message 3.
3. The responder sends its supported muxers in message 2.
4. After the handshake completes, both sides select the muxer using the
   following rule: pick the first muxer from the **initiator's** list that
   appears in the **responder's** list.
5. If a common muxer is found, the muxer is initialized immediately. No
   multistream-select negotiation for the muxer is needed.
6. If no common muxer is found (or if either side did not send extensions),
   fall back to the standard multistream-select muxer negotiation.

**Important:** The initiator MUST NOT include extensions in Noise message 1
(it is unencrypted). Extensions are only sent in messages 2 and 3, which are
encrypted.

### Upgrade for Non-Upgrading Transports

QUIC, WebRTC, and WebTransport have built-in encryption and multiplexing. For
these transports, the Switch skips the upgrade pipeline entirely. The transport
itself provides the secure, multiplexed connection. Protocol negotiation
(multistream-select) still occurs per-stream for application protocols.

| Transport | Security Upgrade | Muxer Upgrade | Per-Stream Negotiation |
|-----------|-----------------|---------------|----------------------|
| TCP | Yes (Noise/TLS) | Yes (Yamux) | Yes |
| WebSocket | Yes (Noise/TLS) | Yes (Yamux) | Yes |
| QUIC v1 | No (built-in TLS 1.3) | No (built-in) | Yes |
| WebRTC | No (built-in DTLS) | No (built-in SCTP) | Yes |
| WebTransport | No (built-in TLS 1.3) | No (built-in) | Yes |

## Dialing

The Switch provides a `Dial` operation that, given a Peer ID and optionally a
set of addresses, establishes an upgraded connection to the remote peer.

### Dial Flow

```
         Dial(PeerID, [Multiaddr])
                  │
                  ▼
    ┌─────────────────────────────┐
    │  1. Check connection pool   │──── Existing connection? ──► Return it
    │     for existing connection │
    └─────────────────────────────┘
                  │ (no existing connection)
                  ▼
    ┌─────────────────────────────┐
    │  2. Resolve addresses       │
    │     - Use provided addrs    │
    │     - Query peer store      │
    │     - Query DHT (optional)  │
    └─────────────────────────────┘
                  │
                  ▼
    ┌─────────────────────────────┐
    │  3. Check dial backoff      │──── Recently failed? ──► Return error
    │     for the peer            │
    └─────────────────────────────┘
                  │ (no backoff active)
                  ▼
    ┌─────────────────────────────┐
    │  4. Connection gating       │──── Policy denies? ──► Return error
    │     (optional)              │
    └─────────────────────────────┘
                  │ (allowed)
                  ▼
    ┌─────────────────────────────┐
    │  5. Resource check          │──── Over limits? ──► Return error
    │     (connection limits)     │
    └─────────────────────────────┘
                  │ (within limits)
                  ▼
    ┌─────────────────────────────┐
    │  6. Select transport for    │
    │     each address            │
    │     (match multiaddr to     │
    │      registered transport)  │
    └─────────────────────────────┘
                  │
                  ▼
    ┌─────────────────────────────┐
    │  7. Parallel dial attempts  │
    │     (ranked, staggered)     │
    └─────────────────────────────┘
                  │
                  ▼
    ┌─────────────────────────────┐
    │  8. First success:          │
    │     Upgrade connection      │
    │     Cancel remaining dials  │
    └─────────────────────────────┘
                  │
                  ▼
    ┌─────────────────────────────┐
    │  9. Add to connection pool  │
    │     Emit Connected event    │
    └─────────────────────────────┘
                  │
                  ▼
          Return Connection
```

### Parallel Dial Attempts (Happy Eyeballs Style)

When a peer has multiple addresses, the Switch dials them in parallel using a
staggered approach inspired by the Happy Eyeballs algorithm (RFC 8305). This
avoids waiting for slow addresses while still preferring certain address types.

**Address ranking:**

1. QUIC addresses are preferred over TCP (lower latency, no upgrade overhead)
2. IPv6 addresses are preferred over IPv4
3. Direct addresses are preferred over relayed addresses
4. Recently successful addresses are preferred

**Staggered dialing:**

Rather than dialing all addresses simultaneously (which wastes resources) or
sequentially (which is slow), the Switch uses staggered delays:

```
Time   Action
─────  ──────────────────────────────────────────
 0ms   Dial address 1 (highest ranked)
250ms  If no success yet, dial address 2
500ms  If no success yet, dial address 3
...    Continue with 250ms delay between attempts
```

The 250ms delay comes from RFC 8305. It provides enough time for fast addresses
to succeed while not waiting too long before trying alternatives.

**On first success:**
- Cancel all pending dial attempts
- Upgrade the successful connection
- Return the upgraded connection

**On all failures:**
- Activate dial backoff for the peer
- Return an error

### Dial Backoff

When all dial attempts to a peer fail, the Switch records a backoff entry to
avoid hammering unreachable peers.

| Parameter | Typical Value | Description |
|-----------|---------------|-------------|
| Backoff duration | 5 seconds | Time before retrying a failed peer |
| Backoff multiplier | 2x | Exponential increase on repeated failures |
| Maximum backoff | 5 minutes | Upper bound on backoff duration |
| Backoff reset | On successful connection | Clears backoff state |

```
Attempt 1 fails  →  backoff 5s
Attempt 2 fails  →  backoff 10s
Attempt 3 fails  →  backoff 20s
Attempt 4 fails  →  backoff 40s
...
Attempt N fails  →  backoff min(5s * 2^(N-1), 300s)
```

Any dial attempt during the backoff period returns immediately with an error
without actually attempting the connection. The backoff is per-peer, not
per-address.

### Dial Deduplication

If multiple goroutines (or threads) attempt to dial the same peer
simultaneously, the Switch deduplicates these into a single dial operation. All
callers receive the same connection (or error) when the dial completes.

```
Thread A: Dial(PeerX)  ──┐
                          ├──►  Single dial operation  ──►  Connection
Thread B: Dial(PeerX)  ──┘                                    │
                                                              ├──► Thread A
                                                              └──► Thread B
```

## Listening

The Switch provides a `Listen` operation that binds one or more transports to
local addresses and accepts inbound connections.

### Listen Flow

```
         Listen([Multiaddr])
                  │
                  ▼
    ┌─────────────────────────────┐
    │  1. Select transport for    │
    │     each listen address     │
    └─────────────────────────────┘
                  │
                  ▼
    ┌─────────────────────────────┐
    │  2. Bind transport to       │
    │     listen address          │
    │     (e.g., TCP bind+listen) │
    └─────────────────────────────┘
                  │
                  ▼
    ┌─────────────────────────────┐
    │  3. Accept loop (per        │
    │     transport listener)     │
    └─────────────────────────────┘
                  │
                  ▼ (on each accepted connection)
    ┌─────────────────────────────┐
    │  4. Connection gating       │──── Policy denies? ──► Close raw conn
    │     (check IP/PeerID)       │
    └─────────────────────────────┘
                  │ (allowed)
                  ▼
    ┌─────────────────────────────┐
    │  5. Resource check          │──── Over limits? ──► Close raw conn
    └─────────────────────────────┘
                  │ (within limits)
                  ▼
    ┌─────────────────────────────┐
    │  6. Upgrade connection      │
    │     (security + muxer)      │
    │     as responder             │
    └─────────────────────────────┘
                  │
                  ▼
    ┌─────────────────────────────┐
    │  7. Verify remote Peer ID   │──── Already connected? ──► Policy decision
    │     (known after security   │     (allow multiple or reject)
    │      handshake)             │
    └─────────────────────────────┘
                  │
                  ▼
    ┌─────────────────────────────┐
    │  8. Add to connection pool  │
    │     Emit Connected event    │
    │     Start stream accept     │
    │     loop for this conn      │
    └─────────────────────────────┘
```

### Inbound Connection Handling

For each accepted connection, the Switch spawns a concurrent task that:

1. **Upgrades the connection** as the responder (the dialing side is the
   initiator; the listening side is the responder in multistream-select and
   Noise/TLS).
2. **Runs the stream accept loop**: continuously accepts new inbound streams
   from the muxer and dispatches each to the appropriate protocol handler.

```
Stream Accept Loop (per connection):

    loop {
        stream ← accept_stream(muxer_session)
        spawn {
            protocol ← multistream_select_respond(stream, registered_protocols)
            handler  ← lookup_handler(protocol)
            handler(stream)
        }
    }
```

Each inbound stream is handled in its own concurrent task. The
multistream-select negotiation on the stream determines which protocol handler
receives it.

### Inbound vs Outbound Roles

| Aspect | Dialer (Outbound) | Listener (Inbound) |
|--------|-------------------|---------------------|
| multistream-select role | Proposer | Responder |
| Noise/TLS role | Initiator | Responder |
| Yamux role | Client (odd stream IDs) | Server (even stream IDs) |
| Who opens first stream | Typically the dialer | Either side can |

## Connection Reuse

Stream multiplexing (Chapter 6) enables multiple independent streams over a
single connection. The Switch exploits this to avoid redundant connections.

### Reuse Policy

When the application requests a new stream to a peer:

1. **Check the connection pool** for an existing connection to that peer.
2. If a healthy connection exists, open a new stream on it. No new connection
   is needed.
3. If no connection exists (or all existing connections are closing), dial a new
   connection.

```
Application: OpenStream(PeerID, "/ipfs/kad/1.0.0")
         │
         ▼
    ┌────────────────────────┐
    │  Connection pool lookup │
    │  for PeerID             │
    └────────────────────────┘
         │                │
    (found)          (not found)
         │                │
         ▼                ▼
    Open stream      Dial peer
    on existing      ──► Upgrade
    connection       ──► Open stream
```

### When to Open New Connections vs Reuse

In general, a single multiplexed connection per peer is sufficient. However,
there are cases where multiple connections to the same peer are justified:

| Scenario | Action |
|----------|--------|
| No existing connection | Dial new |
| Existing healthy connection | Reuse (open stream) |
| Existing connection is closing | Dial new |
| Simultaneous dial (both peers dial each other) | Keep one, close one (see below) |
| Transport diversity (e.g., TCP + QUIC) | Implementation-specific |

### Simultaneous Open

When two peers dial each other at the same time, both will establish a
connection, resulting in two connections between the same pair. The Switch
resolves this by keeping one and closing the other using a deterministic tie-
breaking rule:

**Rule:** Compare the two peers' Peer IDs as byte strings. The peer with the
**larger** Peer ID keeps its initiated (outbound) connection; the peer with
the **smaller** Peer ID closes its initiated connection and uses the inbound
connection instead.

This ensures both sides agree on which connection to keep without additional
coordination.

## Connection Lifecycle

Each connection managed by the Switch progresses through a defined set of
states.

### Connection States

```
                  Dial / Accept
                       │
                       ▼
               ┌──────────────┐
               │  Connecting  │    Raw transport established,
               │              │    upgrade in progress
               └──────┬───────┘
                      │ (upgrade succeeds)
                      ▼
               ┌──────────────┐
               │    Open      │    Fully upgraded, streams
               │              │    can be opened/accepted
               └──────┬───────┘
                      │ (shutdown initiated)
                      ▼
               ┌──────────────┐
               │   Closing    │    Go Away sent/received,
               │              │    no new streams, existing
               │              │    streams draining
               └──────┬───────┘
                      │ (all streams closed)
                      ▼
               ┌──────────────┐
               │   Closed     │    Transport connection
               │              │    closed, resources freed
               └──────────────┘
```

| State | Description | Allowed Operations |
|-------|-------------|-------------------|
| Connecting | Raw connection established, upgrade in progress | None (wait for upgrade) |
| Open | Secure, multiplexed connection ready | Open streams, accept streams |
| Closing | Graceful shutdown in progress | Existing streams continue, no new streams |
| Closed | Connection terminated | None (removed from pool) |

### Connection Events

The Switch emits events that application code can subscribe to:

| Event | When |
|-------|------|
| `Connected(PeerID, ConnInfo)` | Connection fully upgraded and added to pool |
| `Disconnected(PeerID, ConnInfo)` | Connection closed and removed from pool |
| `StreamOpened(PeerID, StreamInfo)` | New stream opened (inbound or outbound) |
| `StreamClosed(PeerID, StreamInfo)` | Stream closed |

`ConnInfo` includes: direction (inbound/outbound), remote address, local
address, security protocol, muxer protocol. `StreamInfo` includes: stream ID,
protocol ID, direction.

### Graceful Shutdown

Yamux supports graceful shutdown through the Go Away frame (see Chapter 6).
When the Switch decides to close a connection:

1. **Send Go Away** (error code 0x00 = normal termination) on the Yamux
   session.
2. **Stop accepting new streams** on this connection.
3. **Wait for existing streams to complete** (with a timeout).
4. **Close the transport connection** (TCP socket, etc.).

The remote side, upon receiving Go Away:
1. Stops opening new streams on this connection.
2. Allows existing streams to finish.
3. May open a new connection if it still needs to communicate.

```
Peer A                                  Peer B
  │                                       │
  │── GoAway [Normal, StreamID=0] ──────►│
  │                                       │
  │   (existing streams continue)         │
  │◄──────── Data [StreamID=3] ──────────│
  │── Data [StreamID=3, FIN] ───────────►│
  │◄── Data [StreamID=3, FIN] ──────────│
  │                                       │
  │   (all streams closed)                │
  │── TCP FIN ──────────────────────────►│
  │◄── TCP FIN ──────────────────────────│
```

### Abrupt Termination

If a connection must be closed immediately (e.g., protocol error, resource
exhaustion), the Switch sends a Go Away with an appropriate error code and
closes the transport connection without waiting for streams to drain.

| Go Away Error Code | Value | Meaning |
|--------------------|-------|---------|
| Normal | 0x00 | Graceful shutdown |
| Protocol Error | 0x01 | Muxer protocol violation |
| Internal Error | 0x02 | Unrecoverable internal error |

## Resource Management

Without resource limits, a libp2p node is vulnerable to resource exhaustion
attacks. An adversary could open thousands of connections or streams, consuming
all available memory and file descriptors. The Switch enforces configurable
limits at multiple scopes.

### Resource Scopes

Resource limits are applied hierarchically:

```
┌─────────────────────────────────────────────┐
│  System Scope                                │
│  (global limits for the entire node)         │
│                                              │
│  ┌────────────────────────────────────────┐  │
│  │  Service Scope                         │  │
│  │  (limits per protocol/service)         │  │
│  │                                        │  │
│  │  ┌─────────────────────────────────┐   │  │
│  │  │  Peer Scope                     │   │  │
│  │  │  (limits per remote peer)       │   │  │
│  │  │                                 │   │  │
│  │  │  ┌──────────────────────────┐   │   │  │
│  │  │  │  Connection Scope       │   │   │  │
│  │  │  │  (limits per connection) │   │   │  │
│  │  │  │                         │   │   │  │
│  │  │  │  ┌───────────────────┐  │   │   │  │
│  │  │  │  │  Stream Scope    │  │   │   │  │
│  │  │  │  │  (per stream)    │  │   │   │  │
│  │  │  │  └───────────────────┘  │   │   │  │
│  │  │  └──────────────────────────┘   │   │  │
│  │  └─────────────────────────────────┘   │  │
│  └────────────────────────────────────────┘  │
└─────────────────────────────────────────────┘
```

Every resource reservation must be approved at every enclosing scope. For
example, opening a new stream requires available capacity at the stream's scope,
the connection scope, the peer scope, the service scope, and the system scope.

### Tracked Resources

| Resource | Description |
|----------|-------------|
| Connections (inbound) | Number of inbound connections |
| Connections (outbound) | Number of outbound connections |
| Streams (inbound) | Number of inbound streams |
| Streams (outbound) | Number of outbound streams |
| File descriptors | OS file descriptor usage (TCP sockets) |
| Memory | Bytes of memory reserved for buffers |

### Typical Default Limits

| Scope | Resource | Typical Limit |
|-------|----------|---------------|
| System | Total connections | 128 - 256 |
| System | Total streams | 1024 - 4096 |
| System | Memory | Proportional to system RAM |
| System | File descriptors | 256 - 512 |
| Peer | Connections (inbound) | 4 |
| Peer | Connections (outbound) | 4 |
| Peer | Streams (inbound) | 256 |
| Peer | Streams (outbound) | 256 |
| Connection | Streams (inbound) | 256 |
| Connection | Streams (outbound) | 256 |
| Connection | Memory | 1 MiB - 4 MiB |

These limits are implementation-specific and should be tunable. go-libp2p
auto-scales limits proportional to available system memory.

### Connection Gating

Connection gating provides policy-based admission control. The Switch consults
a connection gater at multiple points in the connection lifecycle:

| Gate Point | Input | Decision |
|------------|-------|----------|
| Before dialing | Remote multiaddr | Allow / Deny dial |
| After accepting | Remote IP address | Allow / Deny accept |
| After security handshake | Remote Peer ID | Allow / Deny upgrade |
| After muxer upgrade | Fully identified connection | Allow / Deny |

Gating is separate from resource management. A connection may be within
resource limits but denied by policy (e.g., a blocklisted Peer ID).

### Connection Pruning

When the connection pool approaches its limit, the Switch must decide which
connections to prune (close). Pruning strategies consider:

| Factor | Preference |
|--------|------------|
| Connection age | Older connections pruned first (LRU) |
| Stream activity | Idle connections (no active streams) pruned first |
| Direction | Inbound connections pruned before outbound |
| Peer importance | Connections to peers with active protocols kept longer |
| Redundancy | If multiple connections to same peer, close extras |

The pruning algorithm runs when a new inbound connection would exceed the
system connection limit. It selects the least valuable connection and closes it
gracefully (Go Away) before accepting the new one.

## Protocol Handlers

Application protocols (Identify, Ping, Kademlia, GossipSub, custom protocols)
register themselves with the Switch. When an inbound stream arrives, the Switch
uses multistream-select to determine which protocol the remote side wants, then
dispatches the stream to the registered handler.

### Registering Protocol Handlers

Each handler is registered with a protocol ID and a stream handler function:

```
Switch.SetStreamHandler("/ipfs/id/1.0.0",    identifyHandler)
Switch.SetStreamHandler("/ipfs/ping/1.0.0",  pingHandler)
Switch.SetStreamHandler("/ipfs/kad/1.0.0",   kadHandler)
Switch.SetStreamHandler("/meshsub/1.1.0",    gossipsubHandler)
```

The protocol ID is a string that uniquely identifies the protocol. Protocol IDs
follow the convention `/<name>/<version>`.

### Stream Handler Dispatch

When an inbound stream is accepted from the muxer:

```
Inbound Stream
      │
      ▼
multistream-select (responder)
      │
      │  Remote peer proposes: "/ipfs/kad/1.0.0"
      │  Check registered handlers
      │
      ├── Found? ──► Respond with "/ipfs/kad/1.0.0"
      │               Dispatch stream to kadHandler
      │
      └── Not found? ──► Respond with "na"
                         Remote may propose another protocol
                         If no match: close stream
```

### Protocol Matching

Protocol matching supports two modes:

**Exact match:** The proposed protocol ID must exactly match a registered
handler.

**Semantic version match (optional):** Some implementations support matching
based on semantic versioning. For example, a handler registered for
`/ipfs/kad/1.0.0` could accept `/ipfs/kad/1.1.0` if the major version matches.
This is implementation-specific and not required by the spec.

### Outbound Streams

To open an outbound stream to a remote peer for a specific protocol:

1. Obtain a connection to the peer (reuse existing or dial new).
2. Open a new muxer stream on the connection.
3. Run multistream-select as the proposer, proposing the desired protocol.
4. If the remote agrees, the stream is ready for application data.
5. If the remote responds with `na`, the stream is closed with an error.

```
OpenStream(PeerID, "/ipfs/kad/1.0.0")
         │
         ▼
    Get/Dial connection to PeerID
         │
         ▼
    Open new muxer stream
         │
         ▼
    multistream-select (proposer)
    Propose "/ipfs/kad/1.0.0"
         │
         ├── Agreed ──► Return stream for application use
         │
         └── "na" ──► Close stream, return error
```

## Haskell Implementation Notes

### Architecture Sketch

The Switch can be modeled as a record of operations backed by shared concurrent
state:

```haskell
data Switch = Switch
  { swConfig        :: SwitchConfig
  , swTransports    :: TVar (Map TransportKey Transport)
  , swConnPool      :: TVar (Map PeerID [Connection])
  , swListeners     :: TVar [Listener]
  , swProtocols     :: TVar (Map ProtocolID StreamHandler)
  , swDialBackoffs  :: TVar (Map PeerID BackoffState)
  , swResourceMgr   :: ResourceManager
  , swConnGater     :: ConnectionGater
  , swEvents        :: TChan SwitchEvent
  }

data SwitchConfig = SwitchConfig
  { scListenAddrs     :: [Multiaddr]
  , scSecurityProtos  :: [SecurityProtocol]   -- e.g., [Noise, TLS13]
  , scMuxerProtos     :: [MuxerProtocol]      -- e.g., [Yamux, Mplex]
  , scDialTimeout     :: NominalDiffTime      -- e.g., 10 seconds
  , scDialBackoff     :: BackoffConfig
  , scResourceLimits  :: ResourceLimits
  }

data Connection = Connection
  { connPeerID     :: PeerID
  , connDirection  :: Direction        -- Inbound | Outbound
  , connLocalAddr  :: Multiaddr
  , connRemoteAddr :: Multiaddr
  , connSecurity   :: ProtocolID       -- e.g., "/noise"
  , connMuxer      :: ProtocolID       -- e.g., "/yamux/1.0.0"
  , connSession    :: MuxerSession     -- open/accept streams
  , connState      :: TVar ConnState   -- Connecting | Open | Closing | Closed
  }

data Direction = Inbound | Outbound
  deriving (Eq, Show)

data ConnState = Connecting | Open | Closing | Closed
  deriving (Eq, Show)

type StreamHandler = Stream -> IO ()

data SwitchEvent
  = Connected    PeerID ConnInfo
  | Disconnected PeerID ConnInfo
  | StreamOpened  PeerID StreamInfo
  | StreamClosed  PeerID StreamInfo
```

### Key Operations

```haskell
-- Dial a peer, reusing existing connections when possible
dial :: Switch -> PeerID -> [Multiaddr] -> IO Connection

-- Open a new stream to a peer for a specific protocol
openStream :: Switch -> PeerID -> ProtocolID -> IO Stream

-- Register a protocol handler
setStreamHandler :: Switch -> ProtocolID -> StreamHandler -> IO ()

-- Start listening on configured addresses
listen :: Switch -> IO ()

-- Close the switch and all connections
close :: Switch -> IO ()
```

### Use of STM for Concurrent State

The Switch manages highly concurrent state: multiple connections, streams,
dial attempts, and protocol handlers accessed from many threads simultaneously.
STM (Software Transactional Memory) is the natural fit in Haskell.

```haskell
-- Atomic connection pool lookup and insertion
dialOrReuse :: Switch -> PeerID -> [Multiaddr] -> IO Connection
dialOrReuse sw pid addrs = do
    -- Check pool atomically
    existing <- atomically $ do
        pool <- readTVar (swConnPool sw)
        case Map.lookup pid pool of
            Just (c:_) -> do
                st <- readTVar (connState c)
                if st == Open then return (Just c) else return Nothing
            _ -> return Nothing
    case existing of
        Just c  -> return c
        Nothing -> dialNew sw pid addrs

-- Atomic resource reservation
reserveConnection :: ResourceManager -> PeerID -> Direction -> STM (Either ResourceError ())
```

Key STM patterns:
- `TVar (Map PeerID [Connection])` for the connection pool
- `TVar (Map ProtocolID StreamHandler)` for the protocol registry
- `TVar (Map PeerID BackoffState)` for dial backoffs
- `TChan SwitchEvent` for event broadcasting
- Compose multiple reads/writes atomically with `atomically`

### Resource Management with Bracket Patterns

Connection and stream lifecycles are managed with bracket patterns to ensure
cleanup on exceptions:

```haskell
withConnection :: Switch -> PeerID -> (Connection -> IO a) -> IO a
withConnection sw pid action =
    bracket
        (dial sw pid [])
        (\conn -> atomically $ modifyTVar (connState conn) (const Closing))
        action

withStream :: Connection -> ProtocolID -> (Stream -> IO a) -> IO a
withStream conn proto action =
    bracket
        (openMuxerStream (connSession conn))
        closeStream
        (\stream -> do
            negotiateProtocol stream proto
            action stream)
```

Resource cleanup is critical: every opened connection must be closed, every
opened stream must be closed, every reserved resource must be released. The
bracket pattern guarantees this even in the presence of asynchronous exceptions.

### Accept Loop with Async

The listener accept loop spawns a new `async` task for each inbound connection:

```haskell
acceptLoop :: Switch -> Listener -> IO ()
acceptLoop sw listener = forever $ do
    rawConn <- accept listener
    void $ async $ handleInbound sw rawConn

handleInbound :: Switch -> RawConnection -> IO ()
handleInbound sw rawConn = do
    -- Gate check, resource check, upgrade, add to pool
    result <- try $ upgradeInbound sw rawConn
    case result of
        Left (e :: SomeException) -> closeRaw rawConn
        Right conn -> do
            addToPool sw conn
            streamAcceptLoop sw conn

streamAcceptLoop :: Switch -> Connection -> IO ()
streamAcceptLoop sw conn = forever $ do
    stream <- acceptStream (connSession conn)
    void $ async $ dispatchStream sw conn stream
```

### Concurrency Summary

| Component | Concurrency Primitive | Rationale |
|-----------|----------------------|-----------|
| Connection pool | `TVar (Map ...)` | Atomic read/modify from many threads |
| Protocol registry | `TVar (Map ...)` | Handlers may be added/removed at runtime |
| Dial backoffs | `TVar (Map ...)` | Updated from dial threads, read from any |
| Connection state | `TVar ConnState` | Transitions observed by multiple threads |
| Events | `TChan SwitchEvent` | Broadcast to multiple subscribers |
| Accept loop | `async` per connection | Each connection handled independently |
| Stream dispatch | `async` per stream | Each stream handled independently |
| Parallel dialing | `race` / `async` | First success wins |
| Resource cleanup | `bracket` / `finally` | Exception safety |

## Spec References

- Connections spec: https://github.com/libp2p/specs/blob/master/connections/README.md
- Switch concept: https://docs.libp2p.io/concepts/multiplex/switch/
- go-libp2p swarm: https://github.com/libp2p/go-libp2p/tree/master/p2p/net/swarm
- go-libp2p resource manager: https://github.com/libp2p/go-libp2p/tree/master/p2p/host/resource-manager
- Noise extensions (early muxer): https://github.com/libp2p/specs/tree/master/noise
- Happy Eyeballs: https://www.rfc-editor.org/rfc/rfc8305
