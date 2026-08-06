# libp2p-hs

A Haskell implementation of the [libp2p](https://libp2p.io/) modular peer-to-peer networking stack.

## Motivation

Implementation diversity is a core resilience property of decentralized networks:
a bug in one client must not be able to take down the whole network. Yet in the
Ethereum ecosystem — whose consensus layer is built on libp2p — there are
virtually no client implementations written in a pure functional language.
libp2p-hs exists to widen that diversity: a complete, spec-conformant libp2p
stack in Haskell, with the type-level guarantees and STM-based concurrency that
purely functional programming brings to protocol implementation.

## Upstream Collaboration

The project is part of the [libp2p/unified-testing](https://github.com/libp2p/unified-testing)
cross-implementation interop effort, joined at the invitation of a libp2p
maintainer. The codebase receives code review from libp2p maintainers, and
official integration into the unified testing suite is planned within 2027.
Local interop evidence (hs ↔ go over tcp+noise+yamux) lives in `interop/RESULTS.md`.

## Quickstart

```haskell
import LibP2P
import LibP2P.Crypto.Key (publicKey)

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

**[API Reference](https://adust09.github.io/libp2p-hs/)** — Generated Haddock documentation.

## Specification

Based on the [libp2p specification](https://github.com/libp2p/specs).
Reference implementations: [go-libp2p](https://github.com/libp2p/go-libp2p),
[rust-libp2p](https://github.com/libp2p/rust-libp2p).

## License

MIT
