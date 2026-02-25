# Chapter 9: Distributed Hash Table (Kademlia)

The libp2p Distributed Hash Table (DHT) is the primary mechanism for peer
routing and content discovery in decentralized libp2p networks. It is based on
the Kademlia algorithm, augmented with ideas from S/Kademlia, Coral, and the
BitTorrent DHT. This chapter covers the wire protocol, routing table mechanics,
lookup algorithms, and record management in full implementation detail.

## Kademlia Fundamentals

### XOR Distance Metric

Kademlia defines "distance" between two keys using the bitwise XOR operation.
This is not a physical distance -- it is a mathematical metric over a 256-bit
keyspace.

Given two keys `a` and `b`, the distance is:

```
distance(a, b) = XOR(sha256(a), sha256(b))
```

Both keys are first hashed with SHA-256 to produce 256-bit values, then XORed
together. The result is interpreted as an unsigned 256-bit integer.

**Worked example:**

```
Peer A ID (SHA-256):  0010 1100 ...  (256 bits)
Peer B ID (SHA-256):  0010 0101 ...  (256 bits)
                      ─────────
XOR distance:         0000 1001 ...  (256 bits)

Peer A ID (SHA-256):  0010 1100 ...  (256 bits)
Peer C ID (SHA-256):  1101 0011 ...  (256 bits)
                      ─────────
XOR distance:         1111 1111 ...  (256 bits)

distance(A, B) < distance(A, C)
because 0000 1001... < 1111 1111...
```

Peer B is "closer" to Peer A than Peer C in XOR space. Note that this has
nothing to do with network topology or geographic location.

### Properties of XOR Distance

The XOR metric satisfies the properties required for a proper distance function:

1. **Identity**: `d(x, x) = 0` -- a key has zero distance to itself
2. **Symmetry**: `d(x, y) = d(y, x)` -- if A is close to B, then B is close to A.
   This is a key advantage over asymmetric metrics: every lookup for a target
   converges from all directions simultaneously.
3. **Triangle inequality**: `d(x, z) <= d(x, y) + d(y, z)` -- the direct
   distance is never longer than going through an intermediate point.
4. **Non-negativity**: `d(x, y) >= 0` for all x, y.
5. **Unidirectionality**: For any point `x` and distance `delta > 0`, there is
   exactly one point `y` such that `d(x, y) = delta`. This means that lookups
   for the same key from different starting points converge along the same path.

### 256-bit Keyspace

All keys in the libp2p DHT are 256-bit SHA-256 hashes. Peer IDs are hashed
with SHA-256 to produce their position in the keyspace:

```
key = SHA-256(peer_id_bytes)
```

This means the keyspace has 2^256 possible positions. The SHA-256 hash ensures
a uniform distribution of peers across the keyspace, preventing clustering.

## Routing Table

### k-Buckets

The routing table is organized into **k-buckets**, where each bucket holds up to
`k` peers at a particular distance range from the local node.

**Replication parameter `k`:** The recommended value is **20** in the libp2p
DHT. This governs both the bucket size and the replication factor for stored
records.

### Bucket Structure

An implementation must try to maintain `k` peers with shared key prefix of
length `L`, for every `L` in `[0..255]`, in its routing table.

There are 256 buckets, one for each bit of distance:

```
Bucket 0:   Peers with distance in [2^255, 2^256)   — differ in bit 0 (MSB)
Bucket 1:   Peers with distance in [2^254, 2^255)   — share bit 0, differ in bit 1
Bucket 2:   Peers with distance in [2^253, 2^254)   — share bits 0-1, differ in bit 2
  ...
Bucket 254: Peers with distance in [2^1, 2^2)       — share bits 0-253, differ in bit 254
Bucket 255: Peers with distance in [2^0, 2^1)       — share bits 0-254, differ in bit 255
```

Equivalently, bucket `i` contains peers whose key shares a common prefix of
length `i` with the local node's key and differs at bit `i`.

```
Local node key:  0010 1100 0111 ...

Bucket 0:  1... .... .... ...   (first bit differs)
Bucket 1:  01.. .... .... ...   (first bit matches, second differs)
Bucket 2:  001. .... .... ...   (first 2 bits match, third differs)
Bucket 3:  0011 .... .... ...   (first 3 bits match, fourth differs)
Bucket 4:  0010 0... .... ...   (first 4 bits match, fifth differs)
  ...

Each bucket holds up to k=20 peers.
```

The bucket for distant peers (bucket 0) covers half the keyspace and is
easily filled. Buckets for nearby peers (high index) cover exponentially smaller
portions of the keyspace and may remain partially empty.

### Bucket Refresh and Maintenance

The routing table must be kept fresh to reflect the current state of the
network. The bootstrap process (described below) refreshes buckets periodically.

On every bootstrap run (default: every 10 minutes):
1. For every non-empty k-bucket, generate a random peer ID that would fall
   into that bucket.
2. Perform a lookup for that random ID via the `FIND_NODE` RPC.
3. Peers encountered during the lookup are inserted into the routing table.

This ensures that all buckets remain populated with live peers.

### Least-Recently-Seen Eviction

When a new peer is discovered and the appropriate k-bucket is full, the
implementation must decide whether to keep or evict an existing entry.

The original Kademlia paper specifies pinging the least-recently-seen peer:
- If it responds, it is moved to the tail (most recently seen) and the new peer
  is discarded. Long-lived peers are statistically more likely to remain online.
- If it does not respond, it is evicted and the new peer takes its place.

Note: The libp2p DHT implementation historically deviates from this by evicting
the least-recently-seen peer without pinging, which causes higher bucket churn.
Implementations SHOULD follow the standard Kademlia eviction policy (ping first)
for better routing table stability.

### Routing Table Entry

Each entry in a k-bucket contains:

| Field | Description |
|-------|-------------|
| Peer ID | The peer's identity |
| Addresses | Known multiaddrs for the peer |
| Last seen | Timestamp of last successful communication |
| Connection type | Current connection status |

## Protocol ID

```
/ipfs/kad/1.0.0
```

This is the protocol identifier used with multistream-select when opening a
Kademlia DHT stream. Nodes operating in **server mode** advertise this protocol
via the Identify protocol. Nodes operating in **client mode** do not advertise
it.

## RPC Messages

### Wire Format

All RPC messages are sent over a dedicated stream:

1. Open a new stream using multistream-select with `/ipfs/kad/1.0.0`.
2. Send the request message.
3. Read the response message.
4. Close the stream.

On any error, the stream is reset.

Each message is prefixed with its length in bytes, encoded as an unsigned
variable-length integer (unsigned varint, as defined by the multiformats
unsigned-varint spec).

```
┌──────────────────────┬──────────────────────────────┐
│  message_length      │       protobuf_message       │
│  (uvarint)           │     (variable length)        │
└──────────────────────┴──────────────────────────────┘
```

### Protobuf Definitions

The following protobuf definitions are taken verbatim from the spec:

```protobuf
syntax = "proto2";

// Record represents a dht record that contains a value
// for a key value pair
message Record {
    // The key that references this record
    bytes key = 1;

    // The actual value this record is storing
    bytes value = 2;

    // Note: These fields were removed from the Record message
    //
    // Hash of the authors public key
    // optional string author = 3;
    // A PKI signature for the key+value+author
    // optional bytes signature = 4;

    // Time the record was received, set by receiver
    // Formatted according to https://datatracker.ietf.org/doc/html/rfc3339
    string timeReceived = 5;
};

message Message {
    enum MessageType {
        PUT_VALUE     = 0;
        GET_VALUE     = 1;
        ADD_PROVIDER  = 2;
        GET_PROVIDERS = 3;
        FIND_NODE     = 4;
        PING          = 5;
    }

    enum ConnectionType {
        // sender does not have a connection to peer, and no extra information
        // (default)
        NOT_CONNECTED = 0;

        // sender has a live connection to peer
        CONNECTED     = 1;

        // sender recently connected to peer
        CAN_CONNECT   = 2;

        // sender recently tried to connect to peer repeatedly but failed to
        // connect ("try" here is loose, but this should signal "made strong
        // effort, failed")
        CANNOT_CONNECT = 3;
    }

    message Peer {
        // ID of a given peer.
        bytes id = 1;

        // multiaddrs for a given peer
        repeated bytes addrs = 2;

        // used to signal the sender's connection capabilities to the peer
        ConnectionType connection = 3;
    }

    // defines what type of message it is.
    MessageType type = 1;

    // defines what coral cluster level this query/response belongs to.
    // in case we want to implement coral's cluster rings in the future.
    int32 clusterLevelRaw = 10;  // NOT USED

    // Used to specify the key associated with this message.
    // PUT_VALUE, GET_VALUE, ADD_PROVIDER, GET_PROVIDERS
    bytes key = 2;

    // Used to return a value
    // PUT_VALUE, GET_VALUE
    Record record = 3;

    // Used to return peers closer to a key in a query
    // GET_VALUE, GET_PROVIDERS, FIND_NODE
    repeated Peer closerPeers = 8;

    // Used to return Providers
    // GET_VALUE, ADD_PROVIDER, GET_PROVIDERS
    repeated Peer providerPeers = 9;
}
```

### FIND_NODE

Used for peer routing -- finding the closest peers to a given key.

**Request:**
| Field | Value |
|-------|-------|
| `type` | `FIND_NODE` (4) |
| `key` | Binary Peer ID of the node to find |

**Response:**
| Field | Value |
|-------|-------|
| `type` | `FIND_NODE` (4) |
| `closerPeers` | Up to `k` closest `Peer` entries known by the responder |

Each `Peer` in `closerPeers` contains the peer's ID, known multiaddrs, and
connection type.

**Wire example (conceptual):**

```
Request:
  type: FIND_NODE
  key:  0x12200a1b2c3d...  (peer ID bytes, 34 bytes for Ed25519)

Response:
  type: FIND_NODE
  closerPeers: [
    { id: 0x1220..., addrs: [/ip4/1.2.3.4/tcp/4001], connection: CONNECTED },
    { id: 0x1220..., addrs: [/ip4/5.6.7.8/tcp/4001], connection: CAN_CONNECT },
    ...  (up to k=20 peers)
  ]
```

### PUT_VALUE / GET_VALUE

Used for storing and retrieving arbitrary records in the DHT.

**PUT_VALUE request:**
| Field | Value |
|-------|-------|
| `type` | `PUT_VALUE` (0) |
| `key` | Key for the record |
| `record` | `Record` protobuf with `key` and `value` |

**PUT_VALUE response:**
The target node validates the record. If valid, it stores the record and echoes
the request back as the response.

**GET_VALUE request:**
| Field | Value |
|-------|-------|
| `type` | `GET_VALUE` (1) |
| `key` | Key to look up |

**GET_VALUE response:**
| Field | Value |
|-------|-------|
| `type` | `GET_VALUE` (1) |
| `record` | The `Record` for the key (if found in the datastore) |
| `closerPeers` | Up to `k` closest peers to the key |

The response may contain both a record and closer peers. The caller uses the
closer peers to continue the iterative lookup if needed.

### ADD_PROVIDER / GET_PROVIDERS

Used for content routing -- advertising and discovering content providers.

**ADD_PROVIDER request:**
| Field | Value |
|-------|-------|
| `type` | `ADD_PROVIDER` (2) |
| `key` | Multihash of the content (not a CID -- see note below) |
| `providerPeers` | `Peer` entries for the provider (must match sender's Peer ID) |

The target node verifies that the `providerPeers` entries match the sender's
Peer ID. If they do, the provider record is stored.

**GET_PROVIDERS request:**
| Field | Value |
|-------|-------|
| `type` | `GET_PROVIDERS` (3) |
| `key` | Multihash of the content |

**GET_PROVIDERS response:**
| Field | Value |
|-------|-------|
| `type` | `GET_PROVIDERS` (3) |
| `providerPeers` | Known provider `Peer` entries for this key |
| `closerPeers` | Up to `k` closest peers to the key |

**Why multihash, not CID?** Provider records use multihashes as keys because
the same content (same multihash) may appear under different CIDs (CIDv0 vs
CIDv1, different codecs like dag-pb vs raw). The multihash is the minimal
common denominator that all parties agree on.

### PING (Deprecated)

The `PING` message type (5) is deprecated and replaced by the dedicated ping
protocol (`/ipfs/ping/1.0.0`). Implementations may handle incoming PING
requests for backwards compatibility but MUST NOT actively send them.

## Iterative Lookup Algorithm

The core of Kademlia is the iterative lookup -- a process that converges
towards the `k` closest peers to a target key by querying progressively closer
peers.

### Alpha Concurrency Parameter

The concurrency of lookups is limited by parameter `alpha` (written as `a`
below). The libp2p spec sets the default value to **10**, meaning up to 10
in-flight requests at any time. (Note: The original Kademlia paper uses
`alpha = 3`.)

### Step-by-Step Algorithm (FIND_NODE)

**Goal:** Find the `k` closest peers to a target key `Key`.

**State:**
- `Pq`: Set of peers already queried
- `Pn`: Candidate peers sorted by ascending XOR distance from `Key`

**Initialization:**
Seed `Pn` with the `k` closest peers to `Key` from the local routing table.

**Loop:**

```
1. TERMINATION CHECK
   If we have queried and received responses from the k closest
   peers we have seen, return those k peers as the result.
   Also terminate early if Pn is empty and no requests are in flight.

2. SEND QUERIES
   Pick up to alpha peers from the front of Pn (closest first).
   Send FIND_NODE(Key) to each.
   Move them from Pn to Pq (mark as queried).

3. PROCESS RESPONSES
   For each response:
   - On success: add the returned closerPeers to Pn
     (excluding those already in Pq).
     Re-sort Pn by distance to Key.
   - On error/timeout: discard and continue.

4. Go to step 1.
```

### ASCII Diagram: Iterative Lookup

```
                         Target Key: K
                              |
    Local Node                |
        |                     |
        |  1. Seed Pn with k closest from routing table
        |                     |
        v                     |
   ┌─────────┐                |
   │ Pn list │ (sorted by distance to K)
   │ [A,B,C] │                |
   └────┬────┘                |
        |                     |
        | 2. Query alpha peers in parallel
        |                     |
   ┌────┴────────────────┐    |
   v         v            v   |
 ┌───┐    ┌───┐       ┌───┐  |
 │ A │    │ B │       │ C │  |
 └─┬─┘    └─┬─┘       └─┬─┘  |
   |        |            |    |
   | FIND_NODE(K)        |    |
   |        |            |    |
   v        v            v    |
  [D,E]   [E,F,G]     [F,H]  | (closer peers returned)
   |        |            |    |
   | 3. Merge into Pn, re-sort by distance
   |                     |
   v                     |
   ┌─────────┐           |
   │ Pn list │ (updated, re-sorted)
   │ [D,E,F, │           |
   │  G,H]   │           |
   └────┬────┘           |
        |                |
        | 4. Query next alpha peers...
        |                |
   ┌────┴────────────┐   |
   v        v         v  |
 ┌───┐   ┌───┐    ┌───┐ |
 │ D │   │ E │    │ F │ |
 └─┬─┘   └─┬─┘    └─┬─┘ |
   |       |         |   |
   v       v         v   |
 [I,J]  [I,K]     [J,L]  | (even closer peers)
   |       |         |   |
   | 5. Merge, re-sort   |
   |                     |
   v                     |
   ┌─────────┐           |
   │ Pn list │           |
   │ [I,J,K, │           |
   │  L, ...]│ ──► Converging toward K
   └─────────┘
        |
        | 6. Terminate when k closest have all responded
        |
        v
   Result: k closest peers to K
```

### Termination Condition

The lookup terminates when the initiator has queried and received responses
from the `k` closest peers it has seen. At that point, no un-queried peer in
`Pn` is closer than the `k`-th closest responding peer, so further queries
cannot improve the result.

The lookup also terminates early if:
- `Pn` is empty (all known peers have been queried)
- The total number of known peers is less than `k`

### Value Lookup (GET_VALUE)

The value lookup follows the same iterative pattern but uses `GET_VALUE`
instead of `FIND_NODE`, and introduces additional state:

- `best`: The best value found so far
- `Pb`: Peers that returned the best value
- `Po`: Peers that returned outdated values
- `cnt`: Number of values collected

When a value is received, it is compared against the current best using a
`Validator.Select()` function. Peers with outdated values are added to `Po`.
When the lookup completes, outdated peers (and close peers with no value) are
corrected via `PUT_VALUE(Key, best)` -- this ensures eventual convergence to the
best record.

## Peer Routing vs Content Routing

### Finding Peers by ID (Peer Routing)

To find a specific peer by its Peer ID:

1. Compute `Key = PeerID` (the raw Peer ID bytes).
2. Run the iterative FIND_NODE lookup for `Key`.
3. The lookup converges toward the target peer.
4. If the target peer is online and reachable, it will appear in the
   `closerPeers` of some response (or respond to the query directly).
5. Extract the peer's multiaddrs from the `Peer` record.

### Finding Content Providers (Content Routing)

To find providers for a piece of content:

1. Compute `Key = multihash(content)`.
2. Run the iterative lookup using `GET_PROVIDERS` instead of `FIND_NODE`.
3. Peers along the way return both `providerPeers` (known providers) and
   `closerPeers` (for continuing the lookup).
4. Collect provider records until sufficient providers are found or the
   lookup terminates.

To advertise content:

1. Compute `Key = multihash(content)`.
2. Run `FIND_NODE` to find the `k` closest peers to `Key`.
3. Send `ADD_PROVIDER` to each of those `k` peers with your own `PeerInfo`.

### Record Types and Validation

The DHT supports different record types, each with its own validation logic.
The validator interface (in Go-like pseudocode from the spec):

```go
// Validator is an interface that should be implemented by record
// validators.
type Validator interface {
    // Validate validates the given record, returning an error if it's
    // invalid (e.g., expired, signed by the wrong key, etc.).
    Validate(key string, value []byte) error

    // Select selects the best record from the set of records (e.g., the
    // newest).
    //
    // Decisions made by select should be stable.
    Select(key string, values [][]byte) (int, error)
}
```

**`Validate()`** is called:
1. When validating values retrieved in a `GET_VALUE` query.
2. When validating values received in a `PUT_VALUE` query before storing.

**`Select()`** resolves conflicts when multiple values are found for the same
key. It returns the index of the best value. Common strategies: highest
sequence number, most recent timestamp, or cryptographic verification.

## Server Mode vs Client Mode

### Server Mode

A node in server mode:
- **Advertises** `/ipfs/kad/1.0.0` via the Identify protocol
- **Accepts** incoming Kademlia streams (responds to RPCs)
- **Stores** records and provider information for other peers
- **Is added** to other nodes' routing tables

Server mode is appropriate for:
- Publicly routable nodes (e.g., servers in a datacenter)
- Nodes with stable availability, good bandwidth, and sufficient resources

### Client Mode

A node in client mode:
- **Does NOT advertise** `/ipfs/kad/1.0.0` via Identify
- **Does NOT accept** incoming Kademlia streams
- **Initiates** queries (FIND_NODE, GET_VALUE, etc.) but does not serve them
- **Is NOT added** to other nodes' routing tables

Client mode is appropriate for:
- Nodes behind NAT or firewall
- Nodes with intermittent availability
- Nodes with limited resources (CPU, RAM, bandwidth)

### Routing Table Implications

Both client and server nodes maintain a routing table. However, both only add
**server-mode** peers to their routing tables. This is critical: if client
nodes were added to routing tables, they would be unreachable for incoming
queries, degrading DHT performance.

```
                  Routing Table Insertion Rules
                  ─────────────────────────────

  Discovered Peer Mode    Added to Routing Table?
  ──────────────────────  ───────────────────────
  Server                  Yes
  Client                  No
```

## Record Format

### Record Protobuf

The `Record` message stores key-value pairs in the DHT:

```protobuf
message Record {
    bytes key = 1;           // The key referencing this record
    bytes value = 2;         // The actual value stored
    string timeReceived = 5; // RFC 3339 timestamp, set by the receiver
};
```

Note: Fields 3 (`author`) and 4 (`signature`) were removed from the Record
message. Modern libp2p uses signed envelopes for authenticated records instead.

### Signed Records (Routing Records)

For records that require authentication (e.g., peer routing records), libp2p
uses **signed envelopes**. A signed envelope wraps a payload with a signature
from the originating peer's identity key:

```
┌─────────────────────────────────────────────────────┐
│ Signed Envelope                                     │
├──────────────────┬──────────────────────────────────┤
│ public_key       │ Signer's public key (protobuf)   │
│ payload_type     │ Multicodec identifying the type  │
│ payload          │ The record bytes                  │
│ signature        │ Signature over domain + type +    │
│                  │ payload using identity key        │
└──────────────────┴──────────────────────────────────┘
```

The signature covers:
```
sign_data = domain_string || payload_type || payload
```

This prevents replay attacks across different contexts (the domain string acts
as a namespace).

### Validator Interface (Haskell Perspective)

```haskell
-- | Validator for DHT records
data Validator = Validator
  { validate :: ByteString -> ByteString -> Either ValidationError ()
    -- ^ validate key value: check if a record is valid
  , select   :: ByteString -> [ByteString] -> Either ValidationError Int
    -- ^ select key values: choose the best record, return its index
  }
```

## Bootstrap Process

### Overview

The bootstrap process populates and refreshes the routing table. It runs:
1. Once at startup
2. Periodically thereafter (default: every **10 minutes**)

Each run is subject to a `QueryTimeout` (default: **10 seconds**), which upon
firing aborts the run.

### Initial Bootstrap

At startup, the node must know at least one bootstrap peer to join the network.
Bootstrap nodes are well-known, stable peers whose multiaddrs are hardcoded
or configured.

For the IPFS network, the default bootstrap peers are maintained by Protocol
Labs and are publicly listed.

### Self-Lookup

On every bootstrap run:

1. **Self-lookup**: Perform a `FIND_NODE` lookup for the local node's own Peer
   ID. This populates the buckets closest to the local node, which are the
   hardest to fill naturally.

2. **Random lookups**: For every non-empty k-bucket, generate a random Peer ID
   that would fall into that bucket and perform a `FIND_NODE` lookup for it.
   This refreshes all buckets with live peers.

```
Bootstrap Process
─────────────────

1. Connect to bootstrap peers
       │
       v
2. FIND_NODE(self)
       │  Populates nearby buckets
       │  Discovers peers close to us
       v
3. For each non-empty bucket i:
       │  Generate random ID with prefix length i
       │  FIND_NODE(random_id)
       │  Populates bucket i with fresh peers
       v
4. Routing table is now populated
       │
       v
5. Wait 10 minutes, go to step 2
```

### Peers Encountered During Bootstrap

All peers encountered during any lookup are candidates for insertion into the
routing table (subject to the bucket capacity and eviction rules). This means
that even lookups targeting a specific key indirectly improve routing table
health by discovering new peers.

## Security Considerations

### Sybil Attacks

An attacker creates many fake identities (Sybil nodes) to gain
disproportionate control over portions of the keyspace.

**Impact:** The attacker can:
- Intercept or drop lookups for targeted keys
- Return false provider records
- Eclipse honest nodes from the network

**Countermeasures:**
- The SHA-256 hash function makes it computationally expensive to generate
  Peer IDs close to a specific key (2^128 expected work for a 50% collision
  in any given bucket).
- Replication factor `k = 20` means an attacker must control many nodes near
  a key to fully compromise it.
- Disjoint lookup paths (from S/Kademlia) make it harder to intercept all
  paths to a key.

### Eclipse Attacks

An attacker surrounds a target node with malicious peers, isolating it from
the honest network.

**Impact:** The eclipsed node:
- Only sees attacker-controlled peers in its routing table
- Cannot reach honest peers for lookups
- Receives attacker-controlled records and provider information

**Countermeasures:**
- Prefer long-lived peers in the routing table (standard Kademlia eviction
  policy). Established honest peers are not evicted in favor of new attacker
  nodes.
- Verify record signatures where applicable.
- Use multiple bootstrap nodes from independent operators.
- Periodically refresh the routing table from diverse sources.

### Content Poisoning

An attacker stores invalid or malicious records in the DHT.

**Countermeasures:**
- Record validation via the `Validator` interface.
- Signed records (envelopes) for authenticated data.
- The `Select()` function ensures that honest nodes can "outcompete" invalid
  records by preferring records with higher sequence numbers or valid
  signatures.

### Routing Table Pollution

Malicious peers advertise themselves aggressively to fill honest nodes'
routing tables.

**Countermeasures:**
- Only add server-mode peers to the routing table (client/server distinction).
- Bucket size limit of `k = 20` with eviction of unresponsive peers.
- Prefer peers that have been responsive over time.

## Haskell Implementation Notes

### Key Data Types

```haskell
-- | 256-bit key in the DHT keyspace
newtype DHTKey = DHTKey ByteString
  -- ^ Always exactly 32 bytes (SHA-256 output)
  deriving (Eq, Ord, Show)

-- | Compute the DHT key for a Peer ID
peerIdToKey :: PeerId -> DHTKey
peerIdToKey pid = DHTKey (SHA256.hash (peerIdToBytes pid))

-- | XOR distance between two DHT keys
xorDistance :: DHTKey -> DHTKey -> DHTKey
xorDistance (DHTKey a) (DHTKey b) =
    DHTKey (BS.pack (BS.zipWith xor a b))

-- | Common prefix length (determines bucket index)
commonPrefixLength :: DHTKey -> DHTKey -> Int
commonPrefixLength (DHTKey a) (DHTKey b) =
    countLeadingZeros (BS.pack (BS.zipWith xor a b))
  where
    countLeadingZeros bs =
      let bits = concatMap byteToBits (BS.unpack bs)
      in  length (takeWhile (== False) bits)

-- | A single entry in a k-bucket
data BucketEntry = BucketEntry
  { entryPeerId    :: !PeerId
  , entryAddrs     :: ![Multiaddr]
  , entryLastSeen  :: !UTCTime
  , entryConnType  :: !ConnectionType
  } deriving (Show)

-- | A k-bucket holding up to k peers
data KBucket = KBucket
  { bucketEntries  :: !(Seq BucketEntry)  -- ordered by last-seen time
  , bucketCapacity :: !Int                 -- k = 20
  } deriving (Show)

-- | The full routing table: 256 k-buckets
data RoutingTable = RoutingTable
  { rtSelfKey  :: !DHTKey
  , rtBuckets  :: !(Vector KBucket)  -- indexed 0..255
  , rtK        :: !Int               -- replication parameter (20)
  } deriving (Show)
```

### XOR Distance Operations

For comparing distances, you can work with `ByteString` directly:

```haskell
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bits (xor)

-- | Compare two XOR distances.
-- Returns LT if distance a is less than distance b.
compareDistance :: DHTKey -> DHTKey -> DHTKey -> Ordering
compareDistance target a b =
    compare (xorDistance target a) (xorDistance target b)
  -- Ord instance on DHTKey (which wraps ByteString) gives
  -- lexicographic comparison, which is correct for big-endian
  -- unsigned 256-bit integers.

-- | Sort peers by distance to a target key
sortByDistance :: DHTKey -> [PeerId] -> [PeerId]
sortByDistance target =
    sortBy (\a b -> compareDistance target
                      (peerIdToKey a)
                      (peerIdToKey b))
```

Note: Lexicographic `ByteString` comparison is equivalent to big-endian
unsigned integer comparison, which is exactly what we need for XOR distances.

### Concurrent Lookup with async

The iterative lookup naturally maps to Haskell's `async` library:

```haskell
import Control.Concurrent.Async (mapConcurrently, race, withAsync)
import Control.Concurrent.STM

-- | Perform an iterative FIND_NODE lookup
iterativeFindNode
  :: DHTNode
  -> DHTKey           -- ^ target key
  -> IO [PeerInfo]    -- ^ k closest peers
iterativeFindNode node targetKey = do
    -- Seed candidates from local routing table
    let seeds = closestPeers (routingTable node) targetKey (rtK (routingTable node))

    -- Mutable state via STM
    candidatesVar <- newTVarIO (sortByDistance targetKey seeds)
    queriedVar    <- newTVarIO Set.empty
    resultsVar    <- newTVarIO []

    let alpha = 10  -- concurrency parameter

    let loop = do
          -- Pick next batch of candidates
          batch <- atomically $ do
            candidates <- readTVar candidatesVar
            queried    <- readTVar queriedVar
            let unqueried = filter (`Set.notMember` queried) candidates
                batch     = take alpha unqueried
            mapM_ (\p -> modifyTVar' queriedVar (Set.insert p)) batch
            return batch

          if null batch
            then return ()  -- no more candidates
            else do
              -- Query all batch peers concurrently
              responses <- mapConcurrently (sendFindNode node targetKey) batch

              -- Merge results
              atomically $ do
                queried <- readTVar queriedVar
                let newPeers = concatMap closerPeersFromResponse responses
                    filtered = filter (`Set.notMember` queried) newPeers
                modifyTVar' candidatesVar $ \cs ->
                  sortByDistance targetKey (cs ++ filtered)

              -- Check termination: have we queried the k closest?
              done <- atomically $ do
                candidates <- readTVar candidatesVar
                queried    <- readTVar queriedVar
                let kClosest = take (rtK (routingTable node)) candidates
                return (all (`Set.member` queried) kClosest)

              unless done loop

    loop

    -- Return k closest peers from candidates
    atomically $ do
      candidates <- readTVar candidatesVar
      return (take (rtK (routingTable node)) candidates)
```

### DHT Message Encoding

Use the `proto-lens` or `proto3-wire` library for protobuf encoding/decoding:

```haskell
-- | DHT Message type tags
data MessageType
  = PutValue       -- 0
  | GetValue       -- 1
  | AddProvider    -- 2
  | GetProviders   -- 3
  | FindNode       -- 4
  | Ping           -- 5 (deprecated)
  deriving (Show, Eq, Enum)

-- | A DHT RPC message
data DHTMessage = DHTMessage
  { msgType         :: !MessageType
  , msgKey          :: !ByteString
  , msgRecord       :: !(Maybe Record)
  , msgCloserPeers  :: ![PeerInfo]
  , msgProviderPeers :: ![PeerInfo]
  } deriving (Show)

-- | Encode a DHTMessage to wire format (uvarint-prefixed protobuf)
encodeDHTMessage :: DHTMessage -> ByteString
encodeDHTMessage msg =
    let payload = encodeProtobuf msg  -- protobuf encoding
        len     = encodeUvarint (BS.length payload)
    in  len <> payload

-- | Decode a DHTMessage from wire format
decodeDHTMessage :: ByteString -> Either String DHTMessage
decodeDHTMessage bs = do
    (len, rest) <- decodeUvarint bs
    let (payload, _) = BS.splitAt (fromIntegral len) rest
    decodeProtobuf payload
```

### Provider Record Management

```haskell
-- | Provider record with expiration
data ProviderRecord = ProviderRecord
  { prPeerId    :: !PeerId
  , prAddrs     :: ![Multiaddr]
  , prTimestamp  :: !UTCTime
  } deriving (Show)

-- | Provider store: maps content multihash to providers
type ProviderStore = Map ByteString [ProviderRecord]

-- | Expiration interval for provider records (IPFS: 48 hours)
providerRecordExpiration :: NominalDiffTime
providerRecordExpiration = 48 * 3600

-- | Republish interval for provider records (IPFS: 22 hours)
providerRecordRepublish :: NominalDiffTime
providerRecordRepublish = 22 * 3600

-- | Remove expired provider records
cleanExpiredProviders :: UTCTime -> ProviderStore -> ProviderStore
cleanExpiredProviders now =
    Map.map (filter isValid)
  where
    isValid pr = diffUTCTime now (prTimestamp pr) < providerRecordExpiration
```

## Spec References

- Kademlia DHT spec: https://github.com/libp2p/specs/blob/master/kad-dht/README.md
- Kademlia paper: Maymounkov, P., & Mazieres, D. (2002). *Kademlia: A Peer-to-Peer Information System Based on the XOR Metric.* https://doi.org/10.1007/3-540-45748-8_5
- S/Kademlia paper: Baumgart, I., & Mies, S. (2007). *S/Kademlia: A Practicable Approach Towards Secure Key-Based Routing.* https://doi.org/10.1109/ICPADS.2007.4447808
- Coral paper: Freedman, M. J., & Mazieres, D. (2003). *Sloppy Hashing and Self-Organizing Clusters.* https://www.cs.princeton.edu/~mfreed/docs/coral-iptps03.pdf
- BitTorrent DHT (BEP-5): http://bittorrent.org/beps/bep_0005.html
- Unsigned varint spec: https://github.com/multiformats/unsigned-varint
- Provider record measurements: https://github.com/protocol/network-measurements/blob/master/results/rfm17-provider-record-liveness.md
- Identify protocol: https://github.com/libp2p/specs/blob/master/identify/README.md
