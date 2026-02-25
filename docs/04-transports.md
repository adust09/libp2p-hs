# Chapter 4: Transports

A transport in libp2p provides the ability to establish connections (dial) and
accept connections (listen) over a network protocol. This chapter covers the
transport abstraction and the key transport implementations.

## Transport Interface

Every transport implements two core operations:

```
Dial   : Multiaddr → IO Connection
Listen : Multiaddr → IO Listener
```

A `Listener` yields incoming connections. A `Connection` provides a reliable,
bidirectional byte stream (before upgrade) or a multiplexed stream provider
(after upgrade).

### Which Transports Need Upgrading?

| Transport | Needs Security Upgrade | Needs Muxer Upgrade |
|-----------|----------------------|-------------------- |
| TCP | Yes (Noise/TLS) | Yes (Yamux) |
| WebSocket | Yes (Noise/TLS) | Yes (Yamux) |
| QUIC v1 | No (built-in TLS 1.3) | No (built-in) |
| WebRTC | No (built-in DTLS) | No (built-in SCTP) |
| WebTransport | No (built-in TLS 1.3) | No (built-in) |

TCP and WebSocket connections are raw byte streams that must go through the full
connection upgrade pipeline (multistream-select → security → multistream-select
→ muxer). QUIC, WebRTC, and WebTransport have encryption and multiplexing
built into the transport itself.

## TCP Transport

TCP is the most widely supported transport and the recommended starting point
for a new implementation.

### Multiaddr Format
```
/ip4/<addr>/tcp/<port>
/ip6/<addr>/tcp/<port>
/dns4/<hostname>/tcp/<port>
/dns6/<hostname>/tcp/<port>
```

### Connection Lifecycle

**Dialing:**
1. Resolve the multiaddr to an IP address and port
2. Open a TCP socket connection (standard `connect()`)
3. Upgrade the connection:
   a. multistream-select → negotiate security (e.g., `/noise`)
   b. Perform security handshake (e.g., Noise XX)
   c. multistream-select → negotiate muxer (e.g., `/yamux/1.0.0`)
   d. Initialize stream multiplexer
4. Connection is now ready for application streams

**Listening:**
1. Bind to IP:port and listen for connections
2. On accept, perform the upgrade process (same as dialing, but as responder)
3. After upgrade, yield the connection to the Switch

### TCP-Specific Considerations

- **Keepalives**: Enable TCP keepalives to detect dead connections
- **Nodelay**: Disable Nagle's algorithm (`TCP_NODELAY`) for low latency
- **Reuse port**: Consider `SO_REUSEPORT` for hole punching scenarios
- **Connection timeout**: Implement dial timeout (typical: 5-10 seconds)

## QUIC Transport (v1)

QUIC provides encryption (TLS 1.3) and multiplexing natively. It runs over UDP
and offers significant advantages: 1-RTT connection establishment, no head-of-
line blocking across streams, built-in connection migration.

### Multiaddr Format
```
/ip4/<addr>/udp/<port>/quic-v1
/ip6/<addr>/udp/<port>/quic-v1
```

### Key Properties

- **No upgrade needed**: TLS 1.3 and stream multiplexing are part of the QUIC
  protocol itself. No multistream-select negotiation for security or muxer.
- **1-RTT handshake**: Security and transport are established simultaneously.
- **Peer authentication**: The libp2p peer's identity key is embedded in a
  self-signed TLS certificate. The remote peer's Peer ID is verified from this
  certificate during the TLS handshake.
- **Streams**: QUIC streams map directly to libp2p streams. Each stream is
  bidirectional and independently flow-controlled.
- **Protocol negotiation**: multistream-select is still used per-stream for
  application protocol negotiation.

### QUIC Connection Flow
```
Client                                      Server
  │                                            │
  │─────── QUIC Initial (TLS ClientHello) ────►│
  │                                            │
  │◄────── QUIC Handshake (TLS ServerHello) ───│
  │        (includes server cert with          │
  │         libp2p identity key)               │
  │                                            │
  │─────── QUIC Handshake Complete ───────────►│
  │        (TLS Finished + client cert)        │
  │                                            │
  │  Connection established (1-RTT)            │
  │                                            │
  │─── Open QUIC stream ──────────────────────►│
  │─── multistream-select for protocol ───────►│
  │◄── protocol data exchange ────────────────►│
```

### ALPN

libp2p over QUIC uses the ALPN (Application-Layer Protocol Negotiation)
extension in TLS to identify itself. The ALPN protocol ID is `libp2p`.

## WebSocket Transport

WebSocket provides a bidirectional byte stream over HTTP, useful for
browser-based peers.

### Multiaddr Format
```
/ip4/<addr>/tcp/<port>/ws          (unencrypted WebSocket)
/ip4/<addr>/tcp/<port>/wss         (TLS-encrypted WebSocket)
/dns4/<hostname>/tcp/<port>/wss    (typical production setup)
```

### Properties

- Raw WebSocket connections need security and muxer upgrades (same as TCP)
- WSS (WebSocket Secure) provides TLS at the transport level, but libp2p still
  requires its own security negotiation (Noise/TLS) on top
- Primarily used for browser interoperability

## WebRTC Transport

WebRTC enables browser-to-browser and browser-to-server connections using
DTLS for security and SCTP for multiplexing.

### Multiaddr Format
```
/ip4/<addr>/udp/<port>/webrtc-direct
```

### Properties

- Built-in security (DTLS) and multiplexing (SCTP data channels)
- No upgrade needed
- Supports NAT traversal through ICE (Interactive Connectivity Establishment)
- Two variants: `webrtc-direct` (signaling over HTTP) and `webrtc` (signaling
  over a relay)

## WebTransport

WebTransport is based on HTTP/3 (QUIC) and provides a modern alternative for
browser connectivity.

### Multiaddr Format
```
/ip4/<addr>/udp/<port>/quic-v1/webtransport/certhash/<hash>
```

### Properties

- Built on QUIC — inherits all QUIC benefits
- Certificate hashes in the multiaddr allow browsers to connect to
  self-signed servers
- No upgrade needed (security and muxing from QUIC)

## Implementation Priority for libp2p-hs

For the Haskell implementation, the recommended order:

1. **TCP** — Start here. Most widely supported, easiest to debug (use Wireshark).
   Requires implementing the full upgrade pipeline.
2. **QUIC** — Second priority. Significant performance benefits. Use an existing
   Haskell QUIC library (e.g., `quic` package from Kazu Yamamoto).
3. **WebSocket** — If browser interop is needed.
4. **WebRTC/WebTransport** — Lower priority.

## Spec References

- Transport interface: https://github.com/libp2p/specs/blob/master/connections/README.md
- QUIC transport: https://github.com/libp2p/specs/tree/master/quic
- WebRTC: https://github.com/libp2p/specs/tree/master/webrtc
- WebTransport: https://github.com/libp2p/specs/tree/master/webtransport
