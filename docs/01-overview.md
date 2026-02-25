# Chapter 1: Overview

## What is libp2p?

libp2p is a modular networking stack for peer-to-peer applications. Originally
extracted from IPFS, it provides a framework of composable protocols that handle
peer discovery, transport, security, multiplexing, and application-level routing
without relying on centralized infrastructure.

Unlike traditional client-server networking where a server has a fixed address
and clients connect to it, libp2p peers are equal participants that can both
initiate and accept connections. Each peer is identified by a cryptographic key
pair rather than an IP address, enabling identity to persist across network
changes.

## Design Principles

### 1. Transport Agnosticism

libp2p abstracts the underlying transport mechanism. A peer can communicate over
TCP, QUIC, WebSocket, WebRTC, or WebTransport — all using the same application
code. Transports are selected at runtime based on the addresses of the remote
peer.

### 2. Encryption by Default

Every libp2p connection is encrypted and authenticated. There is no unencrypted
mode. The security protocol (Noise or TLS 1.3) is negotiated during connection
establishment, and the remote peer's identity is cryptographically verified.

### 3. Peer Identity over Network Identity

Peers are identified by their Peer ID (a hash of their public key), not by
their IP address or port. This means a peer's identity is stable even as it
moves between networks, changes IP addresses, or listens on different ports.

### 4. Protocol Multiplexing

Multiple application protocols can share a single connection through stream
multiplexing. Each protocol negotiation happens at the stream level, allowing
a single TCP connection to carry DHT queries, pubsub messages, and file
transfers simultaneously.

### 5. Modular and Composable

Each layer of the stack is a replaceable module:
- Transports: TCP, QUIC, WebSocket, WebRTC, WebTransport
- Security: Noise, TLS 1.3
- Multiplexing: Yamux, mplex
- Discovery: mDNS, DHT, Rendezvous
- Routing: Kademlia DHT
- Messaging: GossipSub

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Application Layer                        │
│        (GossipSub, DHT, Identify, Ping, Custom, ...)        │
├─────────────────────────────────────────────────────────────┤
│                     Protocol Negotiation                     │
│                   (multistream-select)                        │
├─────────────────────────────────────────────────────────────┤
│                    Stream Multiplexing                        │
│                   (Yamux / mplex)                             │
├─────────────────────────────────────────────────────────────┤
│                     Protocol Negotiation                     │
│                   (multistream-select)                        │
├─────────────────────────────────────────────────────────────┤
│                     Secure Channel                           │
│                   (Noise / TLS 1.3)                          │
├─────────────────────────────────────────────────────────────┤
│                     Protocol Negotiation                     │
│                   (multistream-select)                        │
├─────────────────────────────────────────────────────────────┤
│                       Transport                              │
│              (TCP / QUIC / WebSocket / WebRTC)                │
├─────────────────────────────────────────────────────────────┤
│                       Network                                │
│                    (IP / UDP / etc.)                          │
└─────────────────────────────────────────────────────────────┘
```

## Layer Stack: Connection Upgrade Pipeline

When a TCP connection is established, it goes through a series of upgrades:

```
Raw TCP connection
    │
    ▼
multistream-select  ──►  Negotiate security protocol
    │
    ▼
Secure Channel (Noise XX handshake)
    │
    ▼
multistream-select  ──►  Negotiate muxer protocol
    │
    ▼
Stream Multiplexer (Yamux)
    │
    ▼
Individual streams opened
    │
    ▼
multistream-select  ──►  Negotiate application protocol per stream
    │
    ▼
Application data exchange
```

QUIC is different — it has built-in encryption (TLS 1.3) and multiplexing, so
no negotiation for security or muxer is needed. Streams are opened directly.

## Key Abstractions

### Connection

A connection represents a secure, multiplexed link between two peers. It may
wrap a TCP socket, a QUIC connection, or any other transport. Once established,
it provides the ability to open and accept streams.

### Stream

A stream is a bidirectional byte channel within a connection. Streams are
lightweight (a single connection may carry thousands) and are the unit at which
protocol negotiation occurs. Each stream carries exactly one protocol's data.

### Switch / Swarm

The Switch (also called Swarm in some implementations) is the central component.
It manages:
- Transport listeners (accepting incoming connections)
- Dialing out to remote peers
- Connection upgrading (security + muxing)
- Connection reuse (sharing a connection across multiple protocol interactions)
- Protocol routing (dispatching incoming streams to handlers)

### Peer Store

The Peer Store maintains metadata about known peers:
- Peer ID → public key mapping
- Peer ID → known multiaddrs
- Peer ID → supported protocols
- Connection status and metadata

## Comparison with Traditional Networking

| Aspect | Client-Server | libp2p |
|--------|--------------|--------|
| Identity | IP address + port | Cryptographic Peer ID |
| Discovery | DNS / hardcoded | DHT, mDNS, Rendezvous |
| NAT | Port forwarding | Hole punching, Circuit Relay |
| Security | Optional (TLS) | Mandatory (Noise/TLS) |
| Multiplexing | HTTP/2 or custom | Yamux (built-in) |
| Addressing | `host:port` | Multiaddr (composable) |
| Topology | Star / hub-spoke | Mesh / arbitrary |

## Reference Implementations

| Language | Repository | Maturity |
|----------|------------|----------|
| Go | [go-libp2p](https://github.com/libp2p/go-libp2p) | Most mature, reference impl |
| Rust | [rust-libp2p](https://github.com/libp2p/rust-libp2p) | Production-ready, strong types |
| JavaScript | [js-libp2p](https://github.com/libp2p/js-libp2p) | Browser-compatible |
| Nim | [nim-libp2p](https://github.com/vacp2p/nim-libp2p) | Smaller, readable |
| **Haskell** | **libp2p-hs** (this project) | **In development** |

The Go and Rust implementations are the primary references for understanding
behavior not fully specified in the written specs. When the spec is ambiguous,
these implementations define the de facto standard.

## Spec References

- libp2p specs repository: https://github.com/libp2p/specs
- Introduction: https://docs.libp2p.io/concepts/introduction/
- Transports: https://docs.libp2p.io/concepts/transports/overview/
- Protocols: https://docs.libp2p.io/concepts/fundamentals/protocols/
