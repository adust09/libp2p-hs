# Chapter 6: Stream Multiplexing

Stream multiplexing allows multiple independent, bidirectional streams to share
a single connection. This chapter covers Yamux (recommended) and mplex
(deprecated).

## Yamux

Yamux ("Yet another Multiplexer") is the recommended stream multiplexer for
libp2p.

### Protocol ID

```
/yamux/1.0.0
```

### Frame Format

Every Yamux message has a fixed 12-byte header:

```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤
│    Version    │      Type     │           Flags             │
├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤
│                         Stream ID                           │
├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤
│                          Length                              │
└─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┘
```

| Field | Offset | Size | Encoding | Description |
|-------|--------|------|----------|-------------|
| Version | 0 | 1 byte | uint8 | Protocol version (always 0) |
| Type | 1 | 1 byte | uint8 | Frame type |
| Flags | 2 | 2 bytes | big-endian uint16 | Bitfield of flags |
| Stream ID | 4 | 4 bytes | big-endian uint32 | Stream identifier |
| Length | 8 | 4 bytes | big-endian uint32 | Type-dependent length/value |

Total header: **12 bytes**, always. No variable-length encoding.

### Frame Types

| Type | Value | Length Field Meaning |
|------|-------|---------------------|
| Data | 0x00 | Number of payload bytes following the header |
| Window Update | 0x01 | Window size increment (delta) in bytes |
| Ping | 0x02 | Opaque ping value (echoed back in response) |
| Go Away | 0x03 | Error code |

### Flags

| Flag | Value | Description |
|------|-------|-------------|
| SYN | 0x0001 | Open a new stream |
| ACK | 0x0002 | Acknowledge a new stream |
| FIN | 0x0004 | Half-close the stream (no more data from sender) |
| RST | 0x0008 | Reset the stream (abrupt termination) |

Flags apply to specific frame types:
- **SYN, ACK**: May be sent with Data, Window Update, or Ping frames
- **FIN, RST**: May be sent with Data or Window Update frames only

For example, a Data frame with SYN flag opens a new stream and sends data in
the same frame.

### Stream Lifecycle

```
Initiator                               Responder
    │                                       │
    │── Data/WndUpdate [SYN, StreamID=1] ──►│  Stream opened
    │                                       │
    │◄── Data/WndUpdate [ACK, StreamID=1] ──│  Stream acknowledged
    │                                       │
    │◄──────── Data [StreamID=1] ──────────►│  Bidirectional data
    │◄──────── Data [StreamID=1] ──────────►│
    │                                       │
    │── Data [FIN, StreamID=1] ────────────►│  Half-close (initiator done sending)
    │                                       │
    │◄── Data [FIN, StreamID=1] ────────────│  Half-close (responder done sending)
    │                                       │
    │         Stream fully closed            │
```

**Stream rejection:** The responder may reject a stream by replying with the
RST flag instead of ACK. The initiator should be prepared to handle this.

**Optimistic sending:** Because Yamux runs over a reliable transport, data can
be sent immediately after the SYN flag without waiting for the ACK. However,
the stream may be rejected (RST) after data has already been sent — senders
must handle this case.

### Stream ID Assignment

- Stream IDs are 32-bit unsigned integers
- The **client** (connection initiator) uses **odd** stream IDs: 1, 3, 5, ...
- The **server** (connection responder) uses **even** stream IDs: 2, 4, 6, ...
- Stream ID 0 is reserved for session-level messages (Ping, Go Away)

### Flow Control

Yamux uses a **credit-based** flow control mechanism at the **stream level only**
(there is no session-level flow control):

- **Initial window size**: 256 KiB (262144 bytes) per stream
- Each stream maintains a **send window** and **receive window**
- Only **Data frames** count toward the window size. Window Update, Ping, and
  Go Away frames are not tracked.
- When data is read from a stream, the receiver sends a **Window Update** frame
  to increase the sender's available window
- The Length field of a Window Update frame is the **delta** (increment), not
  the absolute window size
- Senders MUST NOT send more data than the current window allows
- If the window reaches 0, the sender blocks until a Window Update is received
- Both sides can send a Window Update with the SYN/ACK to indicate a larger
  initial window than the default 256 KiB

### Session-Level Messages

**Ping (Type = 0x02):**
- Stream ID = 0
- The Length field contains an opaque 32-bit value
- Responder echoes back a Ping frame with the same value
- Used for keepalive and latency measurement
- SYN flag on request, ACK flag on response

**Go Away (Type = 0x03):**
- Stream ID = 0
- Signals session termination
- Length field contains an error code:
  - 0x00: Normal termination
  - 0x01: Protocol error
  - 0x02: Internal error
- After sending Go Away, no new streams may be opened
- Existing streams are allowed to complete

### Data Frame

```
[12-byte header: Version=0, Type=0x00, Flags, StreamID, Length=N]
[N bytes of payload]
```

The Length field specifies how many bytes of payload follow the header.

### Wire Example: Opening a Stream and Sending Data

Opening stream 1 with 5 bytes of data ("hello"):

```hex
00       Version: 0
00       Type: Data (0x00)
00 01    Flags: SYN (0x0001)
00 00 00 01  StreamID: 1
00 00 00 05  Length: 5
68 65 6c 6c 6f  Payload: "hello"
```

Total: 12 + 5 = 17 bytes.

Acknowledging stream 1 with a window update:

```hex
00       Version: 0
01       Type: Window Update (0x01)
00 02    Flags: ACK (0x0002)
00 00 00 01  StreamID: 1
00 04 00 00  Length: 262144 (256 KiB window delta)
```

Total: 12 bytes (no payload for Window Update).

## mplex (Deprecated)

mplex is an older, simpler multiplexer. It is deprecated in favor of Yamux but
documented here for reference and interoperability.

### Protocol ID

```
/mplex/6.7.0
```

### Frame Format

```
┌──────────────────────┬──────────────────┬──────────────────────┐
│   header (varint)    │  length (varint) │   data (bytes)       │
└──────────────────────┴──────────────────┴──────────────────────┘
```

The header varint encodes both the stream ID and the message type:

```
header = (stream_id << 3) | flag
```

- **Lower 3 bits**: message type (flag)
- **Upper bits**: stream ID

### Message Types

| Flag | Name | Description |
|------|------|-------------|
| 0 | NewStream | Open a new stream (data = stream name as UTF-8) |
| 1 | MessageReceiver | Data from stream initiator to receiver |
| 2 | MessageInitiator | Data from stream receiver to initiator |
| 3 | CloseReceiver | Half-close from initiator's side |
| 4 | CloseInitiator | Half-close from receiver's side |
| 5 | ResetReceiver | Reset from initiator's side |
| 6 | ResetInitiator | Reset from receiver's side |

Note: The "Receiver"/"Initiator" in flag names refers to the **sender's role**,
not the recipient. Flag 1 (MessageReceiver) means "message sent by the stream
receiver" — this is confusing and is one reason mplex is deprecated.

### Key Differences from Yamux

| Feature | Yamux | mplex |
|---------|-------|-------|
| Header size | Fixed 12 bytes | Variable (varints) |
| Flow control | Yes (window-based) | **No** |
| Max frame payload | ~4 GiB (32-bit Length) | 1 MiB |
| Status | Recommended | Deprecated |
| Ping/keepalive | Built-in | No |
| Graceful shutdown | Go Away frame | No |

The lack of flow control in mplex means a fast sender can overwhelm a slow
receiver, which is a significant limitation for production use.

## Haskell Implementation Notes

- Yamux is the implementation priority. The 12-byte fixed header is easy to
  parse with `Data.Binary` or manual `ByteString` manipulation.
- Flow control state per stream: track send window and receive window as
  `IORef Int` or `TVar Int`.
- Use STM (`TVar`, `TQueue`) for managing concurrent stream state.
- Consider `async` for spawning per-stream read/write tasks.

Key data types sketch:

```haskell
data FrameType = Data | WindowUpdate | Ping | GoAway
  deriving (Eq, Show)

data Flags = Flags
  { flagSYN :: Bool
  , flagACK :: Bool
  , flagFIN :: Bool
  , flagRST :: Bool
  } deriving (Eq, Show)

data YamuxHeader = YamuxHeader
  { yhVersion  :: Word8       -- always 0
  , yhType     :: FrameType
  , yhFlags    :: Flags
  , yhStreamId :: Word32
  , yhLength   :: Word32
  } deriving (Eq, Show)

-- 12 bytes, big-endian, no variable-length encoding
encodeHeader :: YamuxHeader -> ByteString
decodeHeader :: ByteString -> Either String YamuxHeader
```

## Spec References

- Yamux: https://github.com/hashicorp/yamux/blob/master/spec.md
- mplex: https://github.com/libp2p/specs/tree/master/mplex
