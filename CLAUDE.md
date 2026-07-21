# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

libp2p-hs is a Haskell implementation of the [libp2p](https://libp2p.io/) networking stack. A 12-chapter implementation-level textbook (7,270 lines) serves as the authoritative reference for all implementation work; it lives in the `research-notes` repository at `~/research-notes/Projects/libp2p-hs/`.

## Current State

All implementation phases (0–10d) are complete: the full libp2p stack from transport through application protocols, including TCP, Noise XX, Yamux, Switch, Identify, Ping, Kademlia DHT, AutoNAT, Circuit Relay v2, DCUtR, GossipSub, end-to-end integration tests, and public API facade. 547 tests pass across all modules. The project uses a single Cabal library (not internal libraries, due to linker issues with shared `hs-source-dirs`).

### Ongoing: interoperability testing

Following an invitation from a libp2p maintainer, the project is joining the [`libp2p/unified-testing`](https://github.com/libp2p/unified-testing) cross-implementation effort. Local interop harness lives in `interop/` (hs ↔ go transport: tcp+noise+yamux) with CI in `.github/workflows/interop.yml`. Upstream test-app submissions are tracked as GitHub issues, in priority order:

- **#129** transport interop test app → `libp2p/unified-testing` (adapt existing `interop/` to the upstream contract; ref nim PR #77)
- **#130** perf monitoring test app (new binary; ref nim PR #93)
- **#131** hole punching / DCUtR interop test app (ref nim PR #78)
- **#132** P2P spreadsheet example over GossipSub (optional showcase)

## Documentation Reference

The textbook (in `~/research-notes/Projects/libp2p-hs/`) covers wire formats, protobuf definitions, handshake sequences, and byte-level structures. Always consult the relevant chapter before implementing a component:

| Component | Chapter | Key Content |
|-----------|---------|-------------|
| Multiaddr | `03-addressing.md` | Binary format, protocol codes, varint encoding |
| Peer ID | `02-peer-identity.md` | Ed25519/RSA/Secp256k1 key encoding, Peer ID derivation algorithm |
| multistream-select | `07-protocols.md` | Wire format (varint-prefixed UTF-8 + newline), negotiation flow |
| Noise handshake | `05-secure-channels.md` | XX pattern, handshake payload protobuf, framing |
| Yamux | `06-multiplexing.md` | 12-byte frame header, flow control, stream lifecycle |
| Switch/Swarm | `08-switch.md` | Connection upgrading pipeline, resource management |
| Identify/Ping | `07-protocols.md` | Protobuf definitions, wire format |
| Kademlia DHT | `09-dht.md` | XOR distance, k-buckets, RPC protobuf, iterative lookup |
| NAT traversal | `10-nat-traversal.md` | AutoNAT, Circuit Relay v2, DCUtR protobuf definitions |
| GossipSub | `11-pubsub.md` | Mesh management, peer scoring (P1-P7), heartbeat |
| Connection flow | `12-connection-flow.md` | End-to-end TCP/QUIC walkthrough with byte-level detail |

## Planned Implementation Order

Follow this sequence (defined in `INDEX.md`):

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

### Avoid Stacked PRs

When working on sequential sub-phases, **merge each PR into main before creating the next branch**. Do not branch from a feature branch — this creates stacked PRs where deleting a base branch auto-closes downstream PRs. Always branch from the latest `main`.
