-- | Shared types for Yamux session management.
--
-- Types follow the HashiCorp yamux spec.md and docs/06-multiplexing.md.
-- SessionRole determines stream ID parity, StreamState tracks the
-- stream lifecycle state machine, and YamuxSession/YamuxStream hold
-- per-session/per-stream mutable state via STM.
module Network.LibP2P.Mux.Yamux.Types
  ( SessionRole (..)
  , StreamState (..)
  , YamuxError (..)
  , YamuxStream (..)
  , YamuxSession (..)
  ) where

import Control.Concurrent.STM (TMVar, TQueue, TVar)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import Network.LibP2P.Mux.Yamux.Frame (GoAwayCode, YamuxHeader)

-- | SessionRole determines stream ID parity (spec.md §Stream Identification).
-- Client uses odd IDs (1, 3, 5, ...), Server uses even IDs (2, 4, 6, ...).
data SessionRole = RoleClient | RoleServer
  deriving (Show, Eq)

-- | Stream state machine (spec.md §Stream Open/Close/Reset).
-- States map to the spec's lifecycle:
--   SYN sent/received -> Established -> FIN sent/received -> Closed
data StreamState
  = StreamSYNSent -- ^ SYN sent, awaiting ACK (initiator)
  | StreamSYNReceived -- ^ SYN received, awaiting local ACK (responder)
  | StreamEstablished -- ^ Both SYN/ACK exchanged, data flows
  | StreamLocalClose -- ^ Local FIN sent (half-closed)
  | StreamRemoteClose -- ^ Remote FIN received (half-closed)
  | StreamClosed -- ^ Both FIN'd
  | StreamReset -- ^ RST sent or received
  deriving (Show, Eq)

-- | Errors map to spec-defined conditions.
data YamuxError
  = YamuxProtocolError !String -- ^ Spec violation (e.g., invalid version, unknown frame type)
  | YamuxStreamClosed -- ^ Write/read on closed stream
  | YamuxStreamReset -- ^ RST received
  | YamuxSessionShutdown -- ^ GoAway received or session closed
  | YamuxGoAway !GoAwayCode -- ^ Remote sent GoAway with specific code
  deriving (Show, Eq)

-- | Per-stream state (spec.md §Flow Control: per-stream windows only).
data YamuxStream = YamuxStream
  { ysStreamId :: !Word32
  , ysState :: !(TVar StreamState)
  , ysSendWindow :: !(TVar Word32) -- ^ Starts at 262144 (256 KiB)
  , ysRecvWindow :: !(TVar Word32) -- ^ Starts at 262144 (256 KiB)
  , ysRecvBuf :: !(TQueue ByteString) -- ^ Incoming data frames
  , ysSendNotify :: !(TMVar ()) -- ^ Wakeup blocked writers on WindowUpdate
  , ysSession :: !YamuxSession -- ^ Back-reference for frame sending
  }

-- | Session state.
data YamuxSession = YamuxSession
  { ysRole :: !SessionRole
  , ysNextStreamId :: !(TVar Word32) -- ^ Next ID to allocate
  , ysStreams :: !(TVar (Map.Map Word32 YamuxStream)) -- ^ Active streams
  , ysAcceptCh :: !(TQueue YamuxStream) -- ^ Inbound streams (max 256 pending)
  , ysSendCh :: !(TQueue (YamuxHeader, ByteString)) -- ^ Outbound frame queue
  , ysShutdown :: !(TVar Bool) -- ^ Local GoAway sent
  , ysRemoteGoAway :: !(TVar Bool) -- ^ Remote GoAway received
  , ysPings :: !(TVar (Map.Map Word32 (TMVar ()))) -- ^ Pending ping responses
  , ysNextPingId :: !(TVar Word32)
  , ysWrite :: !(ByteString -> IO ()) -- ^ Underlying transport write
  , ysRead :: !(Int -> IO ByteString) -- ^ Underlying transport read exact N bytes
  }
