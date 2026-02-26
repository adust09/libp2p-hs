# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

libp2p-hs is a Haskell implementation of the [libp2p](https://libp2p.io/) networking stack. The project is in early stage — the `docs/` directory contains a 12-chapter implementation-level textbook (7,270 lines) that serves as the authoritative reference for all implementation work.

## Current State

Phases 0–4a + Noise XX handshake are implemented: scaffolding, varint, multihash, multiaddr, peer identity (Ed25519), multistream-select, Yamux frame encoding, Noise framing/payload/key-signing, and full Noise XX handshake (via cacophony) + post-handshake encrypted transport. 117 tests pass across all modules. The project uses a single Cabal library (not internal libraries, due to linker issues with shared `hs-source-dirs`).

## Documentation Reference

The textbook in `docs/` covers wire formats, protobuf definitions, handshake sequences, and byte-level structures. Always consult the relevant chapter before implementing a component:

| Component | Chapter | Key Content |
|-----------|---------|-------------|
| Multiaddr | `docs/03-addressing.md` | Binary format, protocol codes, varint encoding |
| Peer ID | `docs/02-peer-identity.md` | Ed25519/RSA/Secp256k1 key encoding, Peer ID derivation algorithm |
| multistream-select | `docs/07-protocols.md` | Wire format (varint-prefixed UTF-8 + newline), negotiation flow |
| Noise handshake | `docs/05-secure-channels.md` | XX pattern, handshake payload protobuf, framing |
| Yamux | `docs/06-multiplexing.md` | 12-byte frame header, flow control, stream lifecycle |
| Switch/Swarm | `docs/08-switch.md` | Connection upgrading pipeline, resource management |
| Identify/Ping | `docs/07-protocols.md` | Protobuf definitions, wire format |
| Kademlia DHT | `docs/09-dht.md` | XOR distance, k-buckets, RPC protobuf, iterative lookup |
| NAT traversal | `docs/10-nat-traversal.md` | AutoNAT, Circuit Relay v2, DCUtR protobuf definitions |
| GossipSub | `docs/11-pubsub.md` | Mesh management, peer scoring (P1-P7), heartbeat |
| Connection flow | `docs/12-connection-flow.md` | End-to-end TCP/QUIC walkthrough with byte-level detail |

## Planned Implementation Order

Follow this sequence (defined in `docs/INDEX.md`):

1. Multiaddr parsing/encoding
2. Peer ID generation (Ed25519 first)
3. multistream-select negotiation
4. Noise XX handshake
5. Yamux multiplexer
6. TCP transport + Switch
7. Identify protocol
8. Ping protocol
9. Kademlia DHT
10. Circuit Relay + NAT traversal
11. GossipSub

## Architecture

libp2p is a layered protocol stack. Each layer is modular and composable:

```
Application (GossipSub, DHT, Identify, Ping)
         ↓
   multistream-select (protocol negotiation)
         ↓
   Yamux / mplex (stream multiplexing)
         ↓
   multistream-select (muxer negotiation)
         ↓
   Noise / TLS 1.3 (encryption + authentication)
         ↓
   multistream-select (security negotiation)
         ↓
   TCP / QUIC / WebSocket (transport)
```

The **Switch** (ch.08) is the central coordinator that manages this pipeline, connection pooling, and protocol handler dispatch.

## Key Haskell Libraries

- **Crypto**: `crypton` (Ed25519) + `memory` (ByteArray conversion)
- **Noise protocol**: `cacophony` (Noise XX state machine, X25519 DH, ChaChaPoly1305 AEAD) — requires GHC 9.10.x (base < 4.22)
- **Base58**: `ppad-base58` (Bitcoin alphabet encode/decode)
- **Protobuf**: Manual encoding (libp2p protobufs are small, avoids `proto-lens` dependency)
- **Networking**: `network`, `iproute` (IPv4/IPv6 address handling)
- **Concurrency**: `stm`, `async`
- **Binary parsing**: `binary`, `bytestring`
- **Binary helpers**: `Core.Binary` module for shared big-endian encoding (uses `bytestring` Builder + `binary` Get)
- **ASN.1**: `asn1-encoding` (for RSA/ECDSA key formats, not yet used)

### Architecture note

Single library pattern in `.cabal` — internal libraries caused linker issues with shared `hs-source-dirs` on GHC 9.14.1.

## Upstream Specs

Primary reference: https://github.com/libp2p/specs

Reference implementations for cross-checking: [go-libp2p](https://github.com/libp2p/go-libp2p) (most mature), [rust-libp2p](https://github.com/libp2p/rust-libp2p) (good type system reference).

## Branch Conventions

- `main` — protected, no direct commits
- `docs/*` — documentation branches
- `feat/*` — feature implementation branches
- `fix/*` — bug fix branches

Create branches matching the GitHub Issue tag. Use worktrees for parallel work.
