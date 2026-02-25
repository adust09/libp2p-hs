# Chapter 11: Publish/Subscribe (GossipSub)

libp2p provides a topic-based publish/subscribe system built on GossipSub, a
gossip-based mesh routing protocol. This chapter covers the PubSub interface,
GossipSub v1.0 and the v1.1 security extensions, including peer scoring, flood
publishing, and all control message semantics.

## PubSub Interface

### Protocol ID

```
/meshsub/1.1.0
```

GossipSub v1.1 peers advertise this protocol string. The protocol is backwards
compatible with v1.0 (`/meshsub/1.0.0`). Peers discover each other's protocol
support via multistream-select negotiation on the pubsub stream.

### Topic-Based Model

PubSub uses a topic-based publish/subscribe model:

- **Topics** are identified by opaque string IDs (e.g., `"blocks"`, `"tx"`)
- **Publishers** send messages to a topic
- **Subscribers** register interest in topics and receive messages published to them
- A peer can be both publisher and subscriber for any number of topics
- A peer can publish to topics it is not subscribed to (via fan-out)

### Message Format (Protobuf)

The `Message` protobuf defines the content exchanged over PubSub:

```protobuf
syntax = "proto2";

message Message {
    optional string from = 1;
    optional bytes data = 2;
    optional bytes seqno = 3;
    required string topic = 4;
    optional bytes signature = 5;
    optional bytes key = 6;
}
```

| Field | Type | Description |
|-------|------|-------------|
| `from` | string | Original author's Peer ID (not the forwarder) |
| `data` | bytes | Opaque application payload |
| `seqno` | bytes | 64-bit big-endian uint, unique per (from, topic) pair |
| `topic` | string | Topic ID this message is published to |
| `signature` | bytes | Signature over the marshalled message (excluding this field) |
| `key` | bytes | Author's public key (when not inlineable in `from`) |

The presence of `from`, `seqno`, `signature`, and `key` depends on the
**signature policy** configured for the topic:

| Policy | `from` | `seqno` | `signature` | `key` |
|--------|--------|---------|-------------|-------|
| `StrictSign` | Present | Present | Present | Present (if needed) |
| `StrictNoSign` | Absent | Absent | Absent | Absent |

For `StrictSign`, the signature is computed over the marshalled protobuf
(excluding the `signature` field) prefixed with the literal string
`"libp2p-pubsub:"`.

Message size SHOULD be limited to 1 MiB. Messages exceeding this limit are
rejected.

### RPC Message Wrapper

All PubSub communication is wrapped in an `RPC` protobuf:

```protobuf
syntax = "proto2";

message RPC {
    repeated SubOpts subscriptions = 1;
    repeated Message publish = 2;
    optional ControlMessage control = 3;

    message SubOpts {
        optional bool subscribe = 1;
        optional string topicid = 2;
    }
}
```

A single `RPC` message can contain:

- **Subscription changes**: subscribe/unsubscribe announcements
- **Published messages**: zero or more `Message` payloads
- **Control messages**: GossipSub-specific mesh management (GRAFT, PRUNE, IHAVE, IWANT)

Control messages are piggybacked on regular RPC messages whenever possible to
reduce message rate.

### Control Message Protobuf

```protobuf
syntax = "proto2";

message ControlMessage {
    repeated ControlIHave ihave = 1;
    repeated ControlIWant iwant = 2;
    repeated ControlGraft graft = 3;
    repeated ControlPrune prune = 4;
}

message ControlIHave {
    optional string topicID = 1;
    repeated bytes messageIDs = 2;
}

message ControlIWant {
    repeated bytes messageIDs = 1;
}

message ControlGraft {
    optional string topicID = 1;
}

message ControlPrune {
    optional string topicID = 1;
    repeated PeerInfo peers = 2;       // v1.1 Peer Exchange
    optional uint64 backoff = 3;       // v1.1 backoff time (seconds)
}

message PeerInfo {
    optional bytes peerID = 1;
    optional bytes signedPeerRecord = 2;
}
```

## GossipSub Design

### Two Overlay Networks

GossipSub constructs two overlapping overlay networks for each topic:

```
┌─────────────────────────────────────────────────────────────┐
│                    Topic "blocks"                            │
│                                                             │
│   Full-Message Mesh (eager push)                            │
│   ┌───┐     ┌───┐     ┌───┐                                │
│   │ A │─────│ B │─────│ C │    Degree D=6                   │
│   └───┘     └───┘     └───┘    (Dlo=4, Dhi=12)             │
│     │  \      │      /  │                                   │
│     │   \     │     /   │      Full messages flow along     │
│   ┌───┐  \  ┌───┐ /  ┌───┐    mesh links immediately       │
│   │ D │   ──│ E │──  │ F │                                  │
│   └───┘     └───┘     └───┘                                 │
│                                                             │
│   Metadata-Only Gossip (lazy push)                          │
│   ┌───┐ ╌╌╌ ┌───┐ ╌╌╌ ┌───┐                                │
│   │ A │     │ G │     │ H │    IHAVE messages sent to       │
│   └───┘     └───┘     └───┘    random non-mesh peers        │
│     ╎         ╎         ╎      every heartbeat              │
│   ┌───┐     ┌───┐     ┌───┐                                 │
│   │ I │     │ J │     │ K │    Peers request full messages   │
│   └───┘     └───┘     └───┘    with IWANT on demand         │
│                                                             │
│   ───  mesh link (full messages)                            │
│   ╌╌╌  gossip link (metadata only)                          │
└─────────────────────────────────────────────────────────────┘
```

1. **Full-message mesh**: A sparse, bidirectional overlay where complete messages
   are eagerly forwarded. Each peer maintains `D` (target 6) mesh links per topic.

2. **Metadata-only gossip**: A supplementary overlay where peers periodically
   announce message IDs (via IHAVE) to random non-mesh peers. This allows
   recovery from mesh failures and accelerates propagation.

### Mesh Peering Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `D` | 6 | Desired mesh degree (outbound links per topic) |
| `D_lo` | 4 | Lower bound; below this, GRAFT new peers |
| `D_hi` | 12 | Upper bound; above this, PRUNE excess peers |
| `D_lazy` | 6 | Number of peers for gossip emission (or `gossip_factor`) |
| `D_score` | 4-5 | Peers retained by score during oversubscription prune |
| `D_out` | 2 | Minimum outbound connections in mesh |

**Mesh links are bidirectional**: if peer A has peer B in its mesh, then peer B
also has peer A in its mesh. This is maintained via GRAFT/PRUNE exchanges.

### Gossip Peering

Gossip targets peers who are in the topic but NOT in the mesh. During each
heartbeat, the router selects peers for gossip emission:

- **v1.0**: Select `D_lazy` random non-mesh peers
- **v1.1 (adaptive)**: Select `gossip_factor` (default 0.25 = 25%) of eligible
  peers, with a minimum of `D_lazy`

With the default gossip factor of 0.25 and 3 gossip rounds per message (from
`mcache_gossip`), each peer has approximately a 57.8% chance of receiving gossip
about any given message: `1 - (3/4)^3 = 0.578`.

## Grafting and Pruning

### GRAFT Message

A `GRAFT` message requests that the remote peer add the sender to its mesh for
the specified topic.

```
Peer A                              Peer B
  │                                    │
  │  ── GRAFT(topicID="blocks") ───►   │
  │     "Add me to your mesh"          │
  │                                    │
  │  A adds B to mesh["blocks"]        │  B adds A to mesh["blocks"]
  │  (bidirectional link formed)       │  (if subscribed to topic)
  │                                    │
```

**Processing rules:**
- If the receiver is subscribed to the topic: accept and add sender to mesh
- If the receiver is NOT subscribed: respond with `PRUNE`
- If the sender is an explicit peer: log error and respond with `PRUNE` (v1.1)
- If the sender has a negative score (v1.1): respond with `PRUNE`
- If the sender is within a backoff period (v1.1): respond with `PRUNE` and
  apply a behavioral penalty via P7
- If the topic is unknown (v1.1): silently ignore (do NOT respond with PRUNE,
  to prevent spam amplification)

### PRUNE Message

A `PRUNE` message requests that the remote peer remove the sender from its mesh.

```
Peer A                              Peer B
  │                                    │
  │  ── PRUNE(topicID="blocks",   ──►  │
  │         peers=[C,D,E],             │
  │         backoff=60) ──────────     │
  │                                    │
  │  A removes B from mesh["blocks"]   │  B removes A from mesh["blocks"]
  │                                    │  B may connect to C, D, E
  │                                    │  B starts backoff timer (60s)
  │                                    │
```

**v1.1 extensions:**
- `peers`: Optional list of `PeerInfo` for peer exchange (PX). Pruned peer can
  connect to these to reform its mesh. Only peers with non-negative scores are
  exchanged.
- `backoff`: Duration in seconds before the pruned peer may re-GRAFT. Default
  is 60 seconds. The pruned peer must wait the full backoff plus slack for the
  next heartbeat before attempting to re-GRAFT.

### Backoff Timers

```
PRUNE received at t=0, backoff=60s
  │
  ├── t=0..60s: backoff period, GRAFT attempts are rejected
  │             Early GRAFT → immediate PRUNE + P₇ penalty
  │
  ├── t=60s: backoff expires
  │
  └── t=61s (next heartbeat): safe to re-GRAFT
```

| Backoff Type | Default Duration | When Used |
|-------------|------------------|-----------|
| `PruneBackoff` | 60 seconds | Normal mesh prune |
| `UnsubscribeBackoff` | 10 seconds | When unsubscribing from a topic |

Both the pruning and pruned peers maintain backoff state. A peer that attempts
to GRAFT during the backoff period will have the GRAFT rejected and receive a
behavioral penalty (P7).

## Message Propagation

### Full Message Forwarding

Within the mesh, messages are forwarded eagerly (full push):

```
Publisher                Mesh Peers              Further Mesh Peers
   │                                                   │
   │  publish("blocks", data)                          │
   │                                                   │
   ├──► Peer A ──────► Peer C ──────► Peer E           │
   ├──► Peer B ──────► Peer D ──────► Peer F           │
   │         └────────► Peer G                         │
   │                                                   │
```

When a peer receives a valid, unseen message:

1. Add message ID to the `seen` cache
2. Store message in the `mcache` (message cache)
3. Forward to all peers in `mesh[topic]` except the sender
4. Forward to all `peers.floodsub[topic]` (backwards compatibility)
5. Deliver to the local application

### Message Deduplication (Seen Cache)

The `seen` cache is a timed least-recently-used cache of message IDs:

| Parameter | Default | Description |
|-----------|---------|-------------|
| `seen_ttl` | 2 minutes | Expiry time for seen message IDs |

**Message ID calculation** (default, origin-stamped):
```
message_id = from || seqno
```

Alternatively, applications can provide a custom `message_id_fn`:
```
message_id = hash(message.data)    // content-addressed messaging
```

All peers in a topic MUST use identical message ID calculation logic.

Before forwarding or delivering a message, the router checks:
1. Is the message ID in the `seen` cache? If yes, drop (duplicate).
2. Was this message published by us? If yes, drop (no self-forwarding).
3. Is the message valid? If no, drop and penalize sender.

### Message Cache (mcache)

The message cache stores recent messages segmented into history windows:

| Parameter | Default | Description |
|-----------|---------|-------------|
| `mcache_len` | 5 | Total history windows retained |
| `mcache_gossip` | 3 | Windows examined for gossip emission |

```
Window 0 (current)  │ Window 1  │ Window 2  │ Window 3  │ Window 4
  newest messages    │           │           │           │  oldest
  ◄── gossip range (mcache_gossip=3) ──►    │           │
  ◄── full cache (mcache_len=5) ────────────────────────►
```

Operations:
- `mcache.put(m)`: Store message in current window
- `mcache.get(id)`: Retrieve message by ID (for IWANT responses)
- `mcache.get_gossip_ids(topic)`: Get IDs from recent `mcache_gossip` windows
- `mcache.shift()`: Rotate windows during heartbeat, discard oldest

### Message Validation

Messages undergo a three-stage validation:

1. **Structural validation**: Correct protobuf format, required fields present,
   size within limits
2. **Signature validation** (if `StrictSign`): Verify `signature` against `key`
   and `from`
3. **Application validation**: Custom validator function registered per topic

**v1.1 Extended Validators** return one of three results:

| Result | Action | Score Impact |
|--------|--------|-------------|
| `Accept` | Deliver and forward | P2 credit (first delivery) |
| `Reject` | Drop, do not forward | P4 penalty (invalid message) |
| `Ignore` | Drop, do not forward | No penalty |

The `Ignore` result is useful when a peer cannot determine validity (e.g.,
during blockchain sync) without triggering a penalty.

## Gossip Protocol (IHAVE/IWANT)

### IHAVE: Announce Message IDs

The `IHAVE` message announces message IDs that the sender has recently seen.
It is emitted during the heartbeat to random non-mesh peers:

```
Peer A (has messages m1, m2, m3)         Peer B (non-mesh peer)
  │                                          │
  │  ── IHAVE(topic="blocks",           ──►  │
  │          messageIDs=[m1, m2, m3])        │
  │                                          │
  │  "I have these messages, want any?"      │
  │                                          │
```

**Emission rules (v1.1):**
- Emitted every heartbeat (default 1 second)
- Targets: `gossip_factor` (0.25) of eligible non-mesh peers, minimum `D_lazy`
  peers
- Only peers with score above `GossipThreshold` receive gossip
- Message IDs are drawn from the last `mcache_gossip` (3) windows

**Spam protection (v1.1):**
- IHAVE messages per heartbeat are capped per peer
- Total advertised message IDs per heartbeat are capped
- Additional IHAVE messages beyond the cap are silently ignored

### IWANT: Request Full Messages

The `IWANT` message requests the full content of messages by their IDs:

```
Peer B (wants m2)                     Peer A (has m2 in mcache)
  │                                          │
  │  ── IWANT(messageIDs=[m2])          ──►  │
  │                                          │
  │  ◄── Message(m2)                    ───  │
  │                                          │
```

**Processing rules:**
- On receiving IHAVE, check `seen` cache. Request only unseen messages via IWANT.
- On receiving IWANT, look up in `mcache`. Forward if present.
- **v1.1**: IWANT responses are limited per peer to prevent resource drain
- **v1.1**: In-flight IWANT requests are probabilistically tracked. If a random
  tracked message is not received within a timeout, the advertising peer gets
  a P7 behavioral penalty.

### Lazy Push/Pull Pattern

```
                    Eager Push (mesh)
Publisher ──────────────────────────────► Mesh Peer
    │
    │               Lazy Push (gossip)
    │  ┌──────────────────────────────────────────────┐
    │  │  1. IHAVE(ids)                               │
    │  │  ──────────────────►  Non-Mesh Peer          │
    │  │                                              │
    │  │  2. IWANT(ids)                               │
    │  │  ◄──────────────────  Non-Mesh Peer          │
    │  │                                              │
    │  │  3. Full Message                             │
    │  │  ──────────────────►  Non-Mesh Peer          │
    │  └──────────────────────────────────────────────┘
```

The gossip mechanism serves as a secondary propagation channel:
- Recovers from mesh link failures
- Detects missing messages
- Enables epidemic-style protocols (e.g., episub) to build broadcast trees

## Fan-out for Non-Subscribers

A peer can publish messages to topics it has NOT subscribed to using the
**fan-out** mechanism:

```
Publisher (not subscribed to "tx")
  │
  │  First publish to "tx":
  │    1. Select D (6) peers from peers.gossipsub["tx"]
  │    2. Store them in fanout["tx"]
  │    3. Send message to all fanout peers
  │
  │  Subsequent publishes (within TTL):
  │    1. Use existing fanout["tx"] peers
  │    2. Send message to all fanout peers
  │
  │  After fanout_ttl (60s) with no publishes:
  │    1. Remove fanout["tx"] from state
  │
```

### Fan-out Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| Fan-out peers | `D` (6) | Number of fan-out peers per topic |
| `fanout_ttl` | 60 seconds | Time without publishing before fan-out state is discarded |

### Fan-out Maintenance (Heartbeat)

During each heartbeat:

```
for each topic in fanout:
    if time_since_last_published > fanout_ttl:
        remove topic from fanout
    else if |fanout[topic]| < D:
        select D - |fanout[topic]| peers from peers.gossipsub[topic]
        add selected peers to fanout[topic]
```

### Fan-out to Mesh Transition

When a peer subscribes to a topic (JOIN), it first checks the fan-out map:

1. If `fanout[topic]` exists: move those peers to `mesh[topic]`
2. If fewer than `D` peers: fill from `peers.gossipsub[topic]`
3. Send GRAFT to all new mesh members
4. Remove `fanout[topic]`

This avoids rebuilding mesh state from scratch when a publisher becomes a
subscriber.

**Note (v1.1)**: When `FloodPublish` is enabled (default `true`), the fan-out
mechanism is not used. Instead, published messages are sent to ALL connected
peers in the topic with a score above `PublishThreshold`.

## Per-Peer State

### Router State Overview

```
┌─────────────────────────────────────────────────────┐
│                   GossipSub Router                  │
│                                                     │
│  peers.gossipsub: Set PeerId                        │
│    All known gossipsub-capable peers                │
│                                                     │
│  peers.floodsub: Set PeerId                         │
│    All known floodsub-capable peers                 │
│                                                     │
│  mesh: Map Topic (Set PeerId)                       │
│    Mesh peers for each subscribed topic             │
│                                                     │
│  fanout: Map Topic (Set PeerId)                     │
│    Fan-out peers for unsubscribed topics            │
│                                                     │
│  mcache: MessageCache                               │
│    Recent messages in history windows               │
│                                                     │
│  seen: TimedLRUCache MessageId                      │
│    Recently seen message IDs (dedup)                │
│                                                     │
│  backoff: Map (PeerId, Topic) Timestamp             │
│    Backoff timers from PRUNE (v1.1)                 │
│                                                     │
│  scores: Map PeerId Score                           │
│    Peer scores (v1.1)                               │
│                                                     │
│  direct_peers: Set PeerId                           │
│    Explicit peering agreements (v1.1)               │
│                                                     │
│  fanout_last_pub: Map Topic Timestamp               │
│    Last publish time per fan-out topic              │
│                                                     │
│  peer_topics: Map PeerId (Set Topic)                │
│    Topics each peer is subscribed to                │
│                                                     │
│  outbound: Set PeerId                               │
│    Peers we initiated connections to                │
│                                                     │
└─────────────────────────────────────────────────────┘
```

### Per-Peer Tracked State

For each connected peer, the router tracks:

| State | Description |
|-------|-------------|
| Peer ID | Unique identifier |
| Protocol | `gossipsub` or `floodsub` |
| Topics | Set of topics the peer is subscribed to |
| Score | Computed peer score (v1.1) |
| Mesh membership | Which topics this peer is in our mesh for |
| Connection direction | Inbound or outbound (for `D_out` quota) |
| Backoff timers | Per-topic backoff expiration times |
| IP address | For IP colocation scoring (P6) |
| First message deliveries | Counter per topic (for P2) |
| Mesh message deliveries | Counter per topic (for P3) |
| Invalid messages | Counter per topic (for P4) |
| Mesh time | Time since GRAFT per topic (for P1) |
| Behavioral penalties | Counter for P7 |
| IHAVE budget | Remaining IHAVE messages this heartbeat |
| IWANT budget | Remaining IWANT responses this heartbeat |

## Peer Scoring (v1.1)

### Score Function

Each peer computes a local score for every other peer. Scores are **not shared**
between peers. The score determines how the router treats the peer across all
protocol operations.

```
Score(p) = TopicCap(
               Sum over topics t_i of:
                   t_i * (w1*P1 + w2*P2 + w3*P3 + w3b*P3b + w4*P4)
           )
           + w5*P5 + w6*P6 + w7*P7
```

### Score Components

| Parameter | Name | Weight | Description |
|-----------|------|--------|-------------|
| **P1** | Time in Mesh | Positive (small) | Rewards mesh longevity. Capped to prevent abuse. |
| **P2** | First Message Deliveries | Positive | Rewards peers who deliver messages first. |
| **P3** | Mesh Message Delivery Rate | Negative | Penalizes peers below expected delivery threshold. Value = deficit^2. |
| **P3b** | Mesh Message Delivery Failures | Negative | Sticky penalty accumulated on prune. Value = deficit^2 at prune time. |
| **P4** | Invalid Messages | Negative | Penalizes invalid message transmission. Value = counter^2. |
| **P5** | Application-Specific | Positive weight, arbitrary value | Application-defined score component. |
| **P6** | IP Colocation Factor | Negative | Penalizes multiple peers on same IP. Value = (count - threshold)^2. |
| **P7** | Behavioral Penalty | Negative | Explicit penalties for misbehavior (e.g., early re-GRAFT). Value = counter^2. |

### Topic Score Parameters

Each topic can be independently configured:

| Parameter | Description |
|-----------|-------------|
| `TopicWeight` | How much this topic contributes to overall score |
| `TimeInMeshWeight` | Weight for P1 (small positive) |
| `TimeInMeshQuantum` | Duration unit for P1 accrual |
| `TimeInMeshCap` | Maximum P1 value |
| `FirstMessageDeliveriesWeight` | Weight for P2 (positive) |
| `FirstMessageDeliveriesDecay` | Decay factor for P2 counter |
| `FirstMessageDeliveriesCap` | Maximum P2 counter value |
| `MeshMessageDeliveriesWeight` | Weight for P3 (negative) |
| `MeshMessageDeliveriesDecay` | Decay factor for P3 counter |
| `MeshMessageDeliveriesThreshold` | Expected delivery rate threshold |
| `MeshMessageDeliveriesCap` | Maximum P3 counter (>= threshold) |
| `MeshMessageDeliveriesActivation` | Grace period before P3 applies |
| `MeshMessageDeliveryWindow` | Near-first delivery window (1-5 ms) |
| `MeshFailurePenaltyWeight` | Weight for P3b (negative) |
| `MeshFailurePenaltyDecay` | Decay factor for P3b counter |
| `InvalidMessageDeliveriesWeight` | Weight for P4 (negative) |
| `InvalidMessageDeliveriesDecay` | Decay factor for P4 counter |

### Behavioral Penalties

The P7 behavioral penalty counter is explicitly incremented by the router for:

- Attempting to GRAFT during a backoff period
- Advertising bogus message IDs via IHAVE (tracked probabilistically)
- Other protocol violations

The penalty value is the **square** of the counter, mixed with a negative weight.
This provides quadratic escalation for repeated offenses.

### Score Thresholds

Thresholds control how the router treats peers based on their score:

```
Score axis:
  ◄───── negative ──────────── 0 ─────────── positive ─────►

  GraylistThreshold  PublishThreshold  GossipThreshold    0    AcceptPXThreshold
        │                  │                │             │           │
        ▼                  ▼                ▼             ▼           ▼
  ┌──────────┬──────────────┬───────────────┬─────────────┬──────────┐
  │  Ignore  │  No publish  │  No gossip    │  No mesh    │  Accept  │
  │  all RPC │  to peer     │  to/from peer │  (prune)    │  PX from │
  │          │              │               │             │  peer    │
  └──────────┴──────────────┴───────────────┴─────────────┴──────────┘
```

| Threshold | Constraint | Effect |
|-----------|-----------|--------|
| `0` (baseline) | -- | Peers below 0 are pruned from mesh. No PX emitted/accepted. |
| `GossipThreshold` | < 0 | No gossip emitted to or accepted from peer |
| `PublishThreshold` | <= `GossipThreshold` | No flood-published messages sent to peer |
| `GraylistThreshold` | < `PublishThreshold` | All RPCs from peer are ignored |
| `AcceptPXThreshold` | >= 0 | PX from PRUNE is only accepted from peers above this |
| `OpportunisticGraftThreshold` | >= 0 | Triggers opportunistic grafting when median mesh score falls below |

### Parameter Decay

All counters (P2, P3, P3b, P4, P7) decay periodically:

```
counter = counter * decay_factor
if counter < DecayToZero:
    counter = 0
```

| Parameter | Description |
|-----------|-------------|
| `DecayInterval` | How often decay is applied (e.g., 1 second) |
| `DecayToZero` | Threshold below which a counter is zeroed |
| `RetainScore` | How long to retain scores after peer disconnects |

Scores are retained after disconnection for `RetainScore` duration. This
prevents malicious peers from resetting their score by reconnecting.

## Topic Subscription Management

### JOIN: Subscribe to Topic

When the application subscribes to a topic:

```
JOIN(topic):
    1. Announce subscription to all known peers via RPC SubOpts
    2. Check fanout[topic]:
       - If exists: move fanout peers to mesh[topic]
       - Remove fanout[topic] and fanout_last_pub[topic]
    3. If |mesh[topic]| < D:
       - Select D - |mesh[topic]| peers from peers.gossipsub[topic]
         (excluding those already in mesh, with non-negative score)
       - Add to mesh[topic]
    4. For each new mesh peer:
       - Send GRAFT(topic)
```

### LEAVE: Unsubscribe from Topic

When the application unsubscribes from a topic:

```
LEAVE(topic):
    1. Announce unsubscription to all known peers via RPC SubOpts
    2. For each peer in mesh[topic]:
       - Send PRUNE(topic) with backoff=UnsubscribeBackoff (10s)
    3. Delete mesh[topic]
```

The `UnsubscribeBackoff` (10 seconds) is shorter than the normal `PruneBackoff`
(60 seconds) to allow faster resubscription.

### Mesh Construction Example

```
Peer X joins topic "blocks" (D=6, no prior fanout state):

Known gossipsub peers in "blocks": [A, B, C, D, E, F, G, H, I, J]

Step 1: Select 6 random peers → [A, C, D, F, H, J]
Step 2: mesh["blocks"] = {A, C, D, F, H, J}
Step 3: Send GRAFT("blocks") to each

        X
       /|\\ \  \
      A C D F H J     (6 mesh links formed)
```

## Heartbeat

### Overview

The heartbeat is a periodic maintenance procedure that keeps the mesh healthy:

| Parameter | Default |
|-----------|---------|
| `heartbeat_interval` | 1 second |

### Heartbeat Procedure

Each heartbeat performs three main tasks in order:

#### 1. Mesh Maintenance

```
for each topic in mesh:
    // Remove peers with negative scores (v1.1)
    for each peer in mesh[topic]:
        if score(peer) < 0:
            remove peer from mesh[topic]
            send PRUNE(topic) to peer

    // Undersubscribed: add peers
    if |mesh[topic]| < D_lo:
        select D - |mesh[topic]| peers from peers.gossipsub[topic]
            where peer not in mesh[topic]
            and score(peer) >= 0              // v1.1
            and backoff not active            // v1.1
        for each selected peer:
            add to mesh[topic]
            send GRAFT(topic)

    // Oversubscribed: remove peers
    if |mesh[topic]| > D_hi:
        keep best D_score peers by score      // v1.1
        keep D - D_score random peers
            (ensuring D_out outbound peers)   // v1.1
        for each removed peer:
            remove from mesh[topic]
            send PRUNE(topic) with PX

    // Outbound quota check (v1.1)
    if |mesh[topic]| >= D_lo:
        if outbound peers in mesh < D_out:
            select peers to fill D_out quota
            add to mesh[topic]
            send GRAFT(topic)
```

#### 2. Fanout Maintenance

```
for each topic in fanout:
    if time_since_last_published > fanout_ttl:
        remove topic from fanout
    else if |fanout[topic]| < D:
        fill from peers.gossipsub[topic]
```

#### 3. Gossip Emission

```
for each topic in mesh + fanout:
    mids = mcache.get_gossip_ids(topic)
    if mids is not empty:
        // v1.1: adaptive gossip
        eligible = peers in topic with score > GossipThreshold
        n = max(D_lazy, |eligible| * gossip_factor)
        select n peers from eligible, not in mesh[topic] or fanout[topic]
        send IHAVE(topic, mids) to selected peers

mcache.shift()     // rotate message cache windows
```

#### 4. Score Decay (v1.1)

```
for each peer:
    for each topic:
        decay P1 (mesh time), P2, P3, P3b, P4 counters
    decay P7 counter
```

#### 5. Opportunistic Grafting (v1.1, every ~60 heartbeats)

```
for each topic in mesh:
    median_score = median(scores of mesh[topic] peers)
    if median_score < OpportunisticGraftThreshold:
        select 2 peers with score > median_score
        add to mesh[topic]
        send GRAFT(topic)
```

## Flood Publishing

### Behavior

When `FloodPublish` is enabled (default `true` in v1.1), a peer's own messages
are published to **all** connected peers in the topic, not just mesh peers:

```
Normal mesh forwarding:              Flood publishing:

  Peer ──► mesh[topic]               Peer ──► ALL peers in topic
           (D peers)                           with score > PublishThreshold
```

**Key properties:**
- Applies to the peer's OWN messages only (not forwarded messages)
- Forwarded messages still use the mesh
- Applies regardless of whether the publisher is subscribed to the topic
- Replaces fan-out for non-subscribers when enabled

**Why flood publish?**
- Counters eclipse attacks: even if all mesh peers are malicious, the message
  reaches honest peers directly
- Reduces propagation latency: message is injected at multiple points
- Ensures newly published messages from honest nodes reach all connected
  honest peers

### Interaction with Fan-out

When flood publishing is enabled:
- The fan-out map is not used (messages go to all peers anyway)
- No gossip is emitted for topics where the peer is a pure publisher
  (not subscribed)

## Explicit Peering Agreements (v1.1)

Explicit peering agreements are configured out-of-band between node operators:

```
Peer A                                    Peer B
  │  (configured as explicit peers)          │
  │                                          │
  │  ── Always forward messages ────────►    │
  │  ◄── Always forward messages ────────    │
  │                                          │
  │  • Exist OUTSIDE the mesh                │
  │  • Not subject to scoring                │
  │  • Connections maintained at all times    │
  │  • Reconnect check every 5 minutes       │
  │  • GRAFT from explicit peer → error      │
  │                                          │
```

Explicit peers:
- Always receive forwarded messages (regardless of score)
- Always have their RPCs accepted
- Are NOT part of any mesh (exist outside the mesh overlay)
- Must not be GRAFTed; a GRAFT from an explicit peer is an error
- Connections are maintained permanently with periodic connectivity checks

## Haskell Implementation Notes

### Key Data Types

```haskell
-- | Topic identifier
type Topic = Text

-- | Unique message identifier
type MessageId = ByteString

-- | Peer score as a floating-point value
type Score = Double

-- | GossipSub message
data PubSubMessage = PubSubMessage
    { msgFrom      :: Maybe PeerId
    , msgData      :: ByteString
    , msgSeqNo     :: Maybe Word64
    , msgTopic     :: Topic
    , msgSignature :: Maybe ByteString
    , msgKey       :: Maybe ByteString
    }

-- | RPC wrapper
data RPC = RPC
    { rpcSubscriptions :: [SubOpts]
    , rpcPublish       :: [PubSubMessage]
    , rpcControl       :: Maybe ControlMessage
    }

data SubOpts = SubOpts
    { subOptSubscribe :: Bool
    , subOptTopicId   :: Topic
    }

-- | Control messages
data ControlMessage = ControlMessage
    { ctrlIHave :: [IHave]
    , ctrlIWant :: [IWant]
    , ctrlGraft :: [Graft]
    , ctrlPrune :: [Prune]
    }

data IHave = IHave Topic [MessageId]
data IWant = IWant [MessageId]
data Graft = Graft Topic
data Prune = Prune Topic [PeerInfo] (Maybe Word64)  -- peers, backoff

data PeerInfo = PeerInfo PeerId (Maybe ByteString)   -- signed peer record

-- | Per-topic peer state for scoring
data TopicPeerState = TopicPeerState
    { tpsMeshTime                :: NominalDiffTime
    , tpsFirstMessageDeliveries  :: Double
    , tpsMeshMessageDeliveries   :: Double
    , tpsMeshFailurePenalty      :: Double
    , tpsInvalidMessages         :: Double
    , tpsGraftTime               :: Maybe UTCTime
    , tpsInMesh                  :: Bool
    }

-- | Full peer state
data PeerState = PeerState
    { psTopics           :: Set Topic
    , psTopicState       :: Map Topic TopicPeerState
    , psBehaviorPenalty  :: Double
    , psIPAddress        :: Maybe IP
    , psIsOutbound       :: Bool
    , psConnectedSince   :: UTCTime
    , psScore            :: Score      -- cached, recomputed periodically
    }
```

### Router State with STM

The GossipSub router state should be managed with STM (Software Transactional
Memory) for safe concurrent access:

```haskell
data GossipSubRouter = GossipSubRouter
    { gsParams      :: GossipSubParams
    , gsMesh        :: TVar (Map Topic (Set PeerId))
    , gsFanout      :: TVar (Map Topic (Set PeerId))
    , gsFanoutPub   :: TVar (Map Topic UTCTime)
    , gsPeers       :: TVar (Map PeerId PeerState)
    , gsMcache      :: TVar MessageCache
    , gsSeen        :: TVar (Map MessageId UTCTime)
    , gsBackoff     :: TVar (Map (PeerId, Topic) UTCTime)
    , gsDirectPeers :: Set PeerId
    , gsScores      :: TVar (Map PeerId Score)
    }

data GossipSubParams = GossipSubParams
    { paramD                  :: Int           -- 6
    , paramDlo                :: Int           -- 4
    , paramDhi                :: Int           -- 12
    , paramDlazy              :: Int           -- 6
    , paramDscore             :: Int           -- 4
    , paramDout               :: Int           -- 2
    , paramHeartbeatInterval  :: NominalDiffTime  -- 1 second
    , paramFanoutTTL          :: NominalDiffTime  -- 60 seconds
    , paramMcacheLen          :: Int           -- 5
    , paramMcacheGossip       :: Int           -- 3
    , paramSeenTTL            :: NominalDiffTime  -- 120 seconds
    , paramPruneBackoff       :: NominalDiffTime  -- 60 seconds
    , paramUnsubBackoff       :: NominalDiffTime  -- 10 seconds
    , paramGossipFactor       :: Double        -- 0.25
    , paramFloodPublish       :: Bool          -- True
    }
```

### Timer-Based Heartbeat

The heartbeat should run as a background thread using `async`:

```haskell
runHeartbeat :: GossipSubRouter -> IO ()
runHeartbeat router = forever $ do
    threadDelay (toMicroseconds $ paramHeartbeatInterval $ gsParams router)
    atomically $ do
        meshMaintenance router
        fanoutMaintenance router
    -- Gossip emission may require IO for sending messages
    emitGossip router
    atomically $ do
        decayScores router
        shiftMcache router
    -- Opportunistic grafting (every ~60 heartbeats)
    checkOpportunisticGraft router
```

### Mesh Management with STM

```haskell
-- | Add a peer to the mesh for a topic (within STM)
graftPeer :: GossipSubRouter -> Topic -> PeerId -> STM ()
graftPeer router topic peer = do
    mesh <- readTVar (gsMesh router)
    let topicPeers = Map.findWithDefault Set.empty topic mesh
    writeTVar (gsMesh router) $
        Map.insert topic (Set.insert peer topicPeers) mesh

-- | Remove a peer from the mesh for a topic (within STM)
prunePeer :: GossipSubRouter -> Topic -> PeerId -> STM ()
prunePeer router topic peer = do
    mesh <- readTVar (gsMesh router)
    let topicPeers = Map.findWithDefault Set.empty topic mesh
    writeTVar (gsMesh router) $
        Map.insert topic (Set.delete peer topicPeers) mesh
```

### Recommended Haskell Libraries

| Purpose | Library | Notes |
|---------|---------|-------|
| Protobuf encoding | `proto-lens` or `proto3-wire` | For RPC/Message encoding |
| Concurrency | `stm`, `async` | Mesh state, heartbeat timer |
| Time | `time` | Backoff timers, score decay |
| Random selection | `random` | Peer selection for mesh/gossip |
| LRU Cache | `lrucache` | Seen cache implementation |
| IP addresses | `iproute` | For P6 IP colocation scoring |
| ByteString | `bytestring` | Message payloads and IDs |

## Spec References

- PubSub interface: https://github.com/libp2p/specs/blob/master/pubsub/README.md
- GossipSub v1.0: https://github.com/libp2p/specs/blob/master/pubsub/gossipsub/gossipsub-v1.0.md
- GossipSub v1.1: https://github.com/libp2p/specs/blob/master/pubsub/gossipsub/gossipsub-v1.1.md
- Go implementation: https://github.com/libp2p/go-libp2p-pubsub
- Rust implementation: https://github.com/libp2p/rust-libp2p/tree/master/protocols/gossipsub
