# libp2p Implementation Textbook

## Project: libp2p-hs — A Haskell Implementation of libp2p

This textbook provides implementation-level documentation for the libp2p networking
stack. It is designed as the authoritative reference for building a Haskell
implementation of libp2p from scratch, covering wire formats, protobuf definitions,
handshake sequences, byte-level structures, and architectural decisions.

Every chapter includes enough detail to write conformant code without needing to
reverse-engineer existing implementations.

---

## Reading Order

### Foundation (Read First)
1. **[Chapter 1: Overview](01-overview.md)** — Architecture, design principles, layer stack
2. **[Chapter 2: Peer Identity](02-peer-identity.md)** — Key types, Peer ID derivation, protobuf encoding
3. **[Chapter 3: Addressing](03-addressing.md)** — Multiaddr format, protocol codes, encoding

### Core Protocols (Read Second)
4. **[Chapter 7: Protocol Negotiation](07-protocols.md)** — multistream-select, Identify, Ping
5. **[Chapter 5: Secure Channels](05-secure-channels.md)** — Noise XX handshake, TLS 1.3
6. **[Chapter 6: Stream Multiplexing](06-multiplexing.md)** — Yamux frames, mplex wire format

### Transport & Connection Layer
7. **[Chapter 4: Transports](04-transports.md)** — TCP, QUIC, WebSocket, WebRTC
8. **[Chapter 8: Switch / Swarm](08-switch.md)** — Connection upgrading, lifecycle, resource management
9. **[Chapter 12: Connection Flow](12-connection-flow.md)** — End-to-end walkthrough with diagrams

### Higher-Level Protocols
10. **[Chapter 9: Distributed Hash Table](09-dht.md)** — Kademlia, routing, RPCs
11. **[Chapter 10: NAT Traversal](10-nat-traversal.md)** — AutoNAT, Circuit Relay v2, Hole Punching
12. **[Chapter 11: Publish/Subscribe](11-pubsub.md)** — GossipSub, message propagation

---

## Recommended Haskell Implementation Order

After reading this textbook, implement in this order:

| Step | Component | Key Chapter |
|------|-----------|-------------|
| 1 | Multiaddr parsing/encoding | Ch 3 |
| 2 | Peer ID generation (Ed25519 first) | Ch 2 |
| 3 | multistream-select negotiation | Ch 7 |
| 4 | Noise XX handshake | Ch 5 |
| 5 | Yamux multiplexer | Ch 6 |
| 6 | TCP transport + Switch | Ch 4, 8 |
| 7 | Identify protocol | Ch 7 |
| 8 | Ping protocol | Ch 7 |
| 9 | Kademlia DHT | Ch 9 |
| 10 | Circuit Relay + NAT traversal | Ch 10 |
| 11 | GossipSub | Ch 11 |

---

## Specification References

| Spec | URL |
|------|-----|
| libp2p specs (all) | https://github.com/libp2p/specs |
| Peer IDs | https://github.com/libp2p/specs/blob/master/peer-ids/peer-ids.md |
| Addressing | https://github.com/multiformats/multiaddr |
| Connections | https://github.com/libp2p/specs/blob/master/connections/README.md |
| Noise | https://github.com/libp2p/specs/tree/master/noise |
| Yamux | https://github.com/hashicorp/yamux/blob/master/spec.md |
| mplex | https://github.com/libp2p/specs/tree/master/mplex |
| Identify | https://github.com/libp2p/specs/blob/master/identify/README.md |
| Kademlia DHT | https://github.com/libp2p/specs/blob/master/kad-dht/README.md |
| Circuit Relay v2 | https://github.com/libp2p/specs/blob/master/relay/circuit-v2.md |
| DCUtR | https://github.com/libp2p/specs/blob/master/relay/DCUtR.md |
| AutoNAT | https://github.com/libp2p/specs/blob/master/autonat/README.md |
| GossipSub v1.1 | https://github.com/libp2p/specs/blob/master/pubsub/gossipsub/gossipsub-v1.1.md |
| Hole Punching | https://github.com/libp2p/specs/blob/master/relay/hole-punching.md |

---

## Reference Implementations

| Language | Repository | Notes |
|----------|------------|-------|
| Go | https://github.com/libp2p/go-libp2p | Most mature, reference implementation |
| Rust | https://github.com/libp2p/rust-libp2p | Strong type system, good reference for Haskell |
| JavaScript | https://github.com/libp2p/js-libp2p | Browser-compatible |
| Nim | https://github.com/vacp2p/nim-libp2p | Smaller, easier to read |

---

## Glossary

| Term | Definition |
|------|------------|
| **Peer** | A participant in the libp2p network, identified by a Peer ID |
| **Peer ID** | A cryptographic hash of a peer's public key, used as a unique identifier |
| **Multiaddr** | A self-describing network address format supporting protocol composition |
| **Multihash** | A self-describing hash format: `<hash-function-code><digest-size><digest>` |
| **Multicodec** | A self-describing codec identifier used in multiaddr, CID, etc. |
| **CID** | Content Identifier — a self-describing content-addressed identifier |
| **Transport** | The underlying network protocol (TCP, QUIC, WebSocket, etc.) |
| **Secure Channel** | An encrypted communication layer (Noise, TLS 1.3) |
| **Muxer** | Stream multiplexer allowing multiple logical streams over one connection |
| **Stream** | A bidirectional byte channel within a multiplexed connection |
| **Switch / Swarm** | The central component coordinating transports, security, and muxing |
| **Protocol ID** | A string identifier for a libp2p protocol (e.g., `/noise`, `/yamux/1.0.0`) |
| **multistream-select** | Protocol negotiation mechanism used before security and muxer upgrades |
| **Varint** | Variable-length integer encoding (unsigned LEB128) |
| **Protobuf** | Protocol Buffers — Google's binary serialization format, used extensively in libp2p |
| **DHT** | Distributed Hash Table — decentralized key-value store |
| **Kademlia** | The DHT algorithm used by libp2p, based on XOR distance metric |
| **GossipSub** | libp2p's pub/sub protocol using gossip-based message propagation |
| **NAT** | Network Address Translation — a barrier to direct peer-to-peer connectivity |
| **Circuit Relay** | A protocol allowing peers to communicate through an intermediary relay node |
| **Hole Punching** | Technique to establish direct connections through NATs |
| **DCUtR** | Direct Connection Upgrade through Relay — coordinates hole punching |
| **AutoNAT** | Protocol for detecting whether a peer is behind a NAT |
| **Prologue** | Context data mixed into a Noise handshake for channel binding |
| **Upgrader** | Component that transforms a raw connection into a secure, multiplexed one |
