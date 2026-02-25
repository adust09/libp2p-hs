# Chapter 3: Addressing

libp2p uses **multiaddr** (multiaddress), a self-describing network address
format that supports composing multiple protocol layers into a single address.
This chapter covers the binary encoding, protocol codes, and practical usage
patterns needed for implementation.

## What is Multiaddr?

A multiaddr is a binary-encoded, composable network address. Unlike traditional
`host:port` pairs, a multiaddr describes the entire protocol stack needed to
reach a peer.

Example (human-readable form):
```
/ip4/198.51.100.0/tcp/4001/p2p/QmYyQSo1c1Ym7orWxLYvCrM2EmxFTANf8wXmmE7DWjhx5N
```

This says: "Connect via IPv4 address 198.51.100.0, TCP port 4001, to peer with
the given Peer ID."

## Binary Format

A multiaddr is encoded as a concatenation of protocol segments:

```
<multiaddr> = (<protocol-segment>)+
<protocol-segment> = <protocol-code> <address-bytes>
<protocol-code> = unsigned varint
<address-bytes> = depends on protocol (fixed-length or varint-length-prefixed)
```

### Varint Encoding

Protocol codes and length prefixes use **unsigned varint** (LEB128) encoding,
as defined by the [multiformats unsigned-varint spec](https://github.com/multiformats/unsigned-varint).

```
Value < 128:    1 byte   (value as-is, high bit = 0)
Value < 16384:  2 bytes  (7 bits per byte, high bit = continuation flag)
...and so on
```

Examples:
| Value | Varint Bytes |
|-------|-------------|
| 4 (ip4) | `0x04` |
| 6 (tcp) | `0x06` |
| 421 (p2p) | `0xa5 0x03` |
| 273 (udp) | `0x91 0x02` |
| 460 (quic-v1) | `0xcc 0x03` |

## Protocol Table

Key protocols and their multiaddr encoding rules:

| Protocol | Code | Code (varint) | Address Format |
|----------|------|---------------|----------------|
| ip4 | 4 | `04` | 4 bytes (big-endian IPv4) |
| tcp | 6 | `06` | 2 bytes (big-endian port) |
| udp | 273 | `91 02` | 2 bytes (big-endian port) |
| ip6 | 41 | `29` | 16 bytes (big-endian IPv6) |
| dns | 53 | `35` | varint-length-prefixed UTF-8 |
| dns4 | 54 | `36` | varint-length-prefixed UTF-8 |
| dns6 | 55 | `37` | varint-length-prefixed UTF-8 |
| dnsaddr | 56 | `38` | varint-length-prefixed UTF-8 |
| p2p | 421 | `a5 03` | varint-length-prefixed multihash |
| quic-v1 | 460 | `cc 03` | 0 bytes (no address) |
| ws | 477 | `dd 03` | 0 bytes (no address) |
| wss | 478 | `de 03` | 0 bytes (no address) |
| webtransport | 465 | `d1 03` | 0 bytes (no address) |
| p2p-circuit | 290 | `a2 02` | 0 bytes (no address) |
| noise | 454 | `c6 03` | 0 bytes (no address) |
| yamux | 467 | `d3 03` | 0 bytes (no address) |

The full registry is at: https://github.com/multiformats/multicodec/blob/master/table.csv

## Address Encoding Examples

### Example 1: `/ip4/127.0.0.1/tcp/4001`

```
Protocol: ip4 (code 4)
  Code:    04
  Address: 7f 00 00 01          (127.0.0.1 as 4 bytes)

Protocol: tcp (code 6)
  Code:    06
  Address: 0f a1                (4001 as big-endian uint16)

Binary:  04 7f000001 06 0fa1
         (9 bytes total)
```

### Example 2: `/ip4/198.51.100.0/udp/9090/quic-v1`

```
Protocol: ip4 (code 4)
  Code:    04
  Address: c6 33 64 00          (198.51.100.0)

Protocol: udp (code 273)
  Code:    91 02
  Address: 23 82                (9090 as big-endian uint16)

Protocol: quic-v1 (code 460)
  Code:    cc 03
  Address: (none — zero-length address)

Binary:  04 c6336400 9102 2382 cc03
         (11 bytes total)
```

### Example 3: `/ip4/127.0.0.1/tcp/4001/p2p/12D3KooW...`

The `/p2p` component contains a Peer ID as a varint-length-prefixed multihash:

```
Protocol: ip4 → 04 7f000001
Protocol: tcp → 06 0fa1
Protocol: p2p (code 421)
  Code:    a5 03
  Address: <varint length of multihash><multihash bytes>
```

For an Ed25519 peer, the multihash is 38 bytes (identity multihash), so:
```
  Length:  26                    (38 as varint)
  Data:    00 24 0801 1220 <32-byte Ed25519 public key>
```

### Example 4: Circuit Relay Address

```
/ip4/198.51.100.0/tcp/4001/p2p/<relay-peer-id>/p2p-circuit/p2p/<target-peer-id>
```

This means: connect to the relay peer, establish a circuit relay, then reach
the target peer through the relay. The `/p2p-circuit` component has no address
bytes — it acts as a marker separating the relay address from the target.

## Encapsulation and Decapsulation

Multiaddrs compose through **encapsulation**. A transport address can be
encapsulated with additional protocol layers:

```haskell
-- Conceptual Haskell representation
data Multiaddr = Multiaddr [Component]
data Component = Component ProtocolCode ByteString

-- Encapsulation = appending components
encapsulate :: Multiaddr -> Multiaddr -> Multiaddr
encapsulate (Multiaddr a) (Multiaddr b) = Multiaddr (a ++ b)

-- Decapsulation = removing the last occurrence of a protocol and everything after it
decapsulate :: Multiaddr -> ProtocolCode -> Multiaddr
```

## String Parsing Rules

The human-readable string format follows these rules:

1. Each component starts with `/`
2. Protocol name follows (lowercase, e.g., `ip4`, `tcp`, `p2p`)
3. If the protocol has an address, it follows after another `/`
4. Components are concatenated

```
/ip4/127.0.0.1/tcp/4001
 ─┬─ ─────┬─── ─┬─ ──┬─
  │       │      │    │
  proto   addr   proto addr
```

Parsing algorithm:
1. Split on `/` (ignoring leading empty string)
2. Read protocol name → look up code and address format
3. If protocol has an address, read the next token and parse it
4. Repeat until end of string
5. Encode each component to binary

## Address Patterns in libp2p

### TCP Transport
```
/ip4/<ipv4-addr>/tcp/<port>
/ip6/<ipv6-addr>/tcp/<port>
/dns4/<hostname>/tcp/<port>
/dns6/<hostname>/tcp/<port>
```

### QUIC Transport (v1)
```
/ip4/<ipv4-addr>/udp/<port>/quic-v1
/ip6/<ipv6-addr>/udp/<port>/quic-v1
```

### WebSocket
```
/ip4/<ipv4-addr>/tcp/<port>/ws
/dns4/<hostname>/tcp/<port>/wss
```

### WebTransport
```
/ip4/<ipv4-addr>/udp/<port>/quic-v1/webtransport
```

### Circuit Relay
```
/p2p/<relay-id>/p2p-circuit/p2p/<target-id>
/<relay-transport-addr>/p2p/<relay-id>/p2p-circuit/p2p/<target-id>
```

### With Peer ID
Any transport address can be suffixed with `/p2p/<peer-id>`:
```
/ip4/198.51.100.0/tcp/4001/p2p/<peer-id>
/ip4/198.51.100.0/udp/4001/quic-v1/p2p/<peer-id>
```

## `dnsaddr` Resolution

The `dnsaddr` protocol uses DNS TXT records to resolve multiaddrs:

```
/dnsaddr/bootstrap.libp2p.io
```

Resolution:
1. Query TXT records for `_dnsaddr.bootstrap.libp2p.io`
2. Each TXT record contains `dnsaddr=<multiaddr>`
3. Replace the `/dnsaddr/...` component with the resolved multiaddr

This enables bootstrap nodes to change their addresses without updating
hardcoded configurations.

## Haskell Implementation Notes

For the Haskell implementation, consider:

- **Data representation**: A multiaddr as a newtype over `ByteString` with
  smart constructors for each protocol, or as a list of typed components.
- **Varint**: Implement unsigned varint encoding/decoding (LEB128) as a
  foundational utility — it's used throughout libp2p.
- **Protocol registry**: A map from protocol code to parsing/formatting
  functions.
- **Existing libraries**: Check Hackage for `multiformats` or `multiaddr`
  packages, though a custom implementation may be necessary.

Key type sketch:
```haskell
newtype Multiaddr = Multiaddr ByteString
  deriving (Eq, Ord)

data Protocol
  = IP4 Word32
  | IP6 (Word64, Word64)
  | TCP Word16
  | UDP Word16
  | P2P PeerId
  | QuicV1
  | WS
  | WSS
  | P2PCircuit
  | DNS Text
  | DNS4 Text
  | DNS6 Text
  | DNSAddr Text
  deriving (Eq, Show)

parseMultiaddr :: ByteString -> Either String [Protocol]
encodeMultiaddr :: [Protocol] -> ByteString
toText :: [Protocol] -> Text
fromText :: Text -> Either String [Protocol]
```

## Spec References

- Multiaddr spec: https://github.com/multiformats/multiaddr
- Protocol table: https://github.com/multiformats/multicodec/blob/master/table.csv
- Unsigned varint: https://github.com/multiformats/unsigned-varint
