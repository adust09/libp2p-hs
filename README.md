# libp2p-hs

A Haskell implementation of the [libp2p](https://libp2p.io/) modular peer-to-peer networking stack.

## Quickstart

```haskell
import Network.LibP2P
import Network.LibP2P.Crypto.Key (publicKey)

main :: IO ()
main = do
  -- Generate identity
  Right kp <- generateKeyPair
  let pid = fromPublicKey (publicKey kp)

  -- Create and configure switch
  sw <- newSwitch pid kp
  tcp <- newTCPTransport
  addTransport sw tcp
  registerIdentifyHandlers sw
  registerPingHandler sw

  -- Start listening
  addrs <- switchListen sw defaultConnectionGater
    [Multiaddr [IP4 0x7f000001, TCP 0]]
  putStrLn $ "Listening on: " ++ show addrs

  -- Dial a remote peer
  -- result <- dial sw remotePeerId [remoteAddr]

  -- Clean shutdown
  switchClose sw
```

## Architecture

```
Application (GossipSub, DHT, Identify, Ping)
         |
   multistream-select (protocol negotiation)
         |
   Yamux (stream multiplexing)
         |
   multistream-select (muxer negotiation)
         |
   Noise XX (encryption + authentication)
         |
   multistream-select (security negotiation)
         |
   TCP (transport)
```

The **Switch** is the central coordinator managing the full upgrade pipeline,
connection pool, protocol handler dispatch, and resource limits.

## Component Status

| Component | Module | Status |
|-----------|--------|--------|
| Varint / Multihash | `Core.Varint`, `Core.Multihash` | Complete |
| Multiaddr | `Multiaddr.*` | Complete |
| Peer Identity (Ed25519) | `Crypto.*` | Complete |
| multistream-select | `MultistreamSelect.*` | Complete |
| Noise XX handshake | `Security.Noise.*` | Complete |
| Yamux multiplexer | `Mux.Yamux.*` | Complete |
| TCP transport | `Transport.TCP` | Complete |
| Switch (core + dial + listen) | `Switch.*` | Complete |
| Resource manager | `Switch.ResourceManager` | Complete |
| Identify protocol | `Protocol.Identify.*` | Complete |
| Ping protocol | `Protocol.Ping.*` | Complete |
| Kademlia DHT | `DHT.*` | Complete |
| AutoNAT | `NAT.AutoNAT.*` | Complete |
| Circuit Relay v2 | `NAT.Relay.*` | Complete |
| DCUtR | `NAT.DCUtR.*` | Complete |
| GossipSub | `Protocol.GossipSub.*` | Complete |
| Public API | `Network.LibP2P` | Complete |

## Building

Requires **GHC 9.10.x** (the `cacophony` dependency requires `base < 4.22`).

```bash
# Build
cabal build

# Run tests
cabal test

# Build documentation
cabal haddock
```

## Tests

547 tests covering all components: unit tests, property tests, and end-to-end
integration tests over real TCP connections.

```bash
# Run all tests
cabal test --test-show-details=streaming

# Run specific test module
cabal test --test-option="--match=Integration"
```

## Documentation

The `docs/` directory contains a 12-chapter implementation-level textbook
covering wire formats, protobuf definitions, handshake sequences, and
byte-level structures for every component.

## Specification

Based on the [libp2p specification](https://github.com/libp2p/specs).
Reference implementations: [go-libp2p](https://github.com/libp2p/go-libp2p),
[rust-libp2p](https://github.com/libp2p/rust-libp2p).

## License

MIT
