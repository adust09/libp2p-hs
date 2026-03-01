-- | Yamux session management: create, openStream, acceptStream, ping, goaway.
--
-- Implements the session-level Yamux protocol per HashiCorp yamux spec.md.
-- The session manages a collection of multiplexed streams over a single
-- underlying transport connection.
--
-- Two background loops run per session:
--   recvLoop: reads 12-byte headers from transport, dispatches to streams
--   sendLoop: dequeues from ysSendCh, writes to transport
module Network.LibP2P.Mux.Yamux.Session
  ( newSession
  , closeSession
  , openStream
  , acceptStream
  , ping
  , sendGoAway
  , recvLoop
  , sendLoop
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import Network.LibP2P.Mux.Yamux.Frame
import Network.LibP2P.Mux.Yamux.Types

-- | Create a new Yamux session over a transport connection.
-- Client uses odd stream IDs starting at 1, server uses even starting at 2.
newSession :: SessionRole -> (ByteString -> IO ()) -> (Int -> IO ByteString) -> IO YamuxSession
newSession role writeFn readFn = do
  let startId = case role of
        RoleClient -> 1
        RoleServer -> 2
  nextId <- newTVarIO startId
  streams <- newTVarIO Map.empty
  acceptCh <- newTQueueIO
  sendCh <- newTQueueIO
  shutdown <- newTVarIO False
  remoteGoAway <- newTVarIO False
  pings <- newTVarIO Map.empty
  nextPingId <- newTVarIO 1
  pure
    YamuxSession
      { ysRole = role
      , ysNextStreamId = nextId
      , ysStreams = streams
      , ysAcceptCh = acceptCh
      , ysSendCh = sendCh
      , ysShutdown = shutdown
      , ysRemoteGoAway = remoteGoAway
      , ysPings = pings
      , ysNextPingId = nextPingId
      , ysWrite = writeFn
      , ysRead = readFn
      }

-- | Gracefully close the session by sending GoAway Normal.
closeSession :: YamuxSession -> IO ()
closeSession sess = sendGoAway sess GoAwayNormal

-- | Open a new outbound stream. Allocates the next stream ID and sends SYN.
-- Returns YamuxSessionShutdown if the session has sent or received GoAway.
openStream :: YamuxSession -> IO (Either YamuxError YamuxStream)
openStream sess = do
  -- Check shutdown state
  canOpen <- atomically $ do
    shut <- readTVar (ysShutdown sess)
    remote <- readTVar (ysRemoteGoAway sess)
    pure (not shut && not remote)
  if not canOpen
    then pure (Left YamuxSessionShutdown)
    else do
      -- Allocate stream ID (atomically increment by 2)
      sid <- atomically $ do
        nextId <- readTVar (ysNextStreamId sess)
        writeTVar (ysNextStreamId sess) (nextId + 2)
        pure nextId
      -- Create stream in SYNSent state
      stream <- newStream sess sid StreamSYNSent
      -- Register stream
      atomically $ modifyTVar' (ysStreams sess) (Map.insert sid stream)
      -- Send SYN frame (Data frame with SYN flag, no payload)
      let hdr =
            YamuxHeader
              { yhVersion = 0
              , yhType = FrameData
              , yhFlags = defaultFlags {flagSYN = True}
              , yhStreamId = sid
              , yhLength = 0
              }
      atomically $ writeTQueue (ysSendCh sess) (hdr, BS.empty)
      pure (Right stream)

-- | Accept an inbound stream. Blocks until a remote SYN arrives.
-- Returns YamuxSessionShutdown if the session is shut down.
acceptStream :: YamuxSession -> IO (Either YamuxError YamuxStream)
acceptStream sess = do
  stream <- atomically $ readTQueue (ysAcceptCh sess)
  -- Send ACK (WindowUpdate frame with ACK flag)
  let hdr =
        YamuxHeader
          { yhVersion = 0
          , yhType = FrameWindowUpdate
          , yhFlags = defaultFlags {flagACK = True}
          , yhStreamId = ysStreamId stream
          , yhLength = 0
          }
  atomically $ writeTQueue (ysSendCh sess) (hdr, BS.empty)
  -- Transition to Established
  atomically $ writeTVar (ysState stream) StreamEstablished
  pure (Right stream)

-- | Send a Ping and wait for the ACK response.
-- Ping uses StreamID 0 and the Length field carries an opaque value.
ping :: YamuxSession -> IO (Either YamuxError ())
ping sess = do
  (pingId, waiter) <- atomically $ do
    pid <- readTVar (ysNextPingId sess)
    writeTVar (ysNextPingId sess) (pid + 1)
    w <- newEmptyTMVar
    modifyTVar' (ysPings sess) (Map.insert pid w)
    pure (pid, w)
  -- Send Ping SYN frame
  let hdr =
        YamuxHeader
          { yhVersion = 0
          , yhType = FramePing
          , yhFlags = defaultFlags {flagSYN = True}
          , yhStreamId = 0
          , yhLength = pingId
          }
  atomically $ writeTQueue (ysSendCh sess) (hdr, BS.empty)
  -- Wait for ACK
  atomically $ takeTMVar waiter
  -- Cleanup
  atomically $ modifyTVar' (ysPings sess) (Map.delete pingId)
  pure (Right ())

-- | Send a GoAway frame with the specified error code.
-- Sets ysShutdown to True so no new streams can be opened.
sendGoAway :: YamuxSession -> GoAwayCode -> IO ()
sendGoAway sess code = do
  atomically $ writeTVar (ysShutdown sess) True
  let errCode = case code of
        GoAwayNormal -> 0x00
        GoAwayProtocol -> 0x01
        GoAwayInternal -> 0x02
  let hdr =
        YamuxHeader
          { yhVersion = 0
          , yhType = FrameGoAway
          , yhFlags = defaultFlags
          , yhStreamId = 0
          , yhLength = errCode
          }
  atomically $ writeTQueue (ysSendCh sess) (hdr, BS.empty)

-- | Receive loop: reads 12-byte headers from transport and dispatches frames.
-- This loop runs until the transport connection is closed or an error occurs.
recvLoop :: YamuxSession -> IO ()
recvLoop sess = go
  where
    go = do
      -- Read 12-byte header
      headerBytes <- ysRead sess headerSize
      case decodeHeader headerBytes of
        Left _err -> pure () -- Protocol error, stop
        Right hdr -> do
          -- Verify version
          if yhVersion hdr /= 0
            then pure () -- Protocol error
            else do
              dispatchFrame sess hdr
              go

-- | Dispatch a decoded frame to the appropriate handler.
dispatchFrame :: YamuxSession -> YamuxHeader -> IO ()
dispatchFrame sess hdr = case yhType hdr of
  FrameData -> handleDataFrame sess hdr
  FrameWindowUpdate -> handleWindowUpdate sess hdr
  FramePing -> handlePing sess hdr
  FrameGoAway -> handleGoAway sess hdr

-- | Handle a Data frame: read payload, manage stream state, deliver data.
handleDataFrame :: YamuxSession -> YamuxHeader -> IO ()
handleDataFrame sess hdr = do
  -- Read payload
  payload <-
    if yhLength hdr > 0
      then ysRead sess (fromIntegral (yhLength hdr))
      else pure BS.empty
  let sid = yhStreamId hdr
      flags = yhFlags hdr
  -- Handle SYN flag: create new inbound stream (with parity + duplicate validation)
  when (flagSYN flags) $ do
    valid <- atomically $ validateInboundSYN sess sid
    if not valid
      then sendGoAway sess GoAwayProtocol
      else do
        stream <- newStream sess sid StreamSYNReceived
        atomically $ do
          modifyTVar' (ysStreams sess) (Map.insert sid stream)
          writeTQueue (ysAcceptCh sess) stream
  -- Handle ACK flag: transition SYNSent -> Established
  when (flagACK flags) $ do
    mStream <- atomically $ Map.lookup sid <$> readTVar (ysStreams sess)
    case mStream of
      Just stream -> atomically $ do
        st <- readTVar (ysState stream)
        case st of
          StreamSYNSent -> writeTVar (ysState stream) StreamEstablished
          _ -> pure ()
      Nothing -> pure ()
  -- Deliver payload to stream buffer (with flow-control check)
  when (BS.length payload > 0) $ do
    mStream <- atomically $ Map.lookup sid <$> readTVar (ysStreams sess)
    case mStream of
      Just stream -> do
        let payloadLen = fromIntegral (BS.length payload)
        overWindow <- atomically $ do
          w <- readTVar (ysRecvWindow stream)
          if w < payloadLen
            then pure True
            else do
              writeTQueue (ysRecvBuf stream) payload
              writeTVar (ysRecvWindow stream) (w - payloadLen)
              pure False
        when overWindow $ sendGoAway sess GoAwayProtocol
      Nothing -> pure ()
  -- Handle FIN flag
  when (flagFIN flags) $ do
    mStream <- atomically $ Map.lookup sid <$> readTVar (ysStreams sess)
    case mStream of
      Just stream -> atomically $ do
        st <- readTVar (ysState stream)
        case st of
          StreamEstablished -> writeTVar (ysState stream) StreamRemoteClose
          StreamLocalClose -> writeTVar (ysState stream) StreamClosed
          StreamSYNSent -> writeTVar (ysState stream) StreamRemoteClose
          _ -> pure ()
      Nothing -> pure ()
  -- Handle RST flag
  when (flagRST flags) $ do
    mStream <- atomically $ Map.lookup sid <$> readTVar (ysStreams sess)
    case mStream of
      Just stream -> atomically $ writeTVar (ysState stream) StreamReset
      Nothing -> pure ()

-- | Handle a WindowUpdate frame: update send window, manage stream lifecycle.
handleWindowUpdate :: YamuxSession -> YamuxHeader -> IO ()
handleWindowUpdate sess hdr = do
  let sid = yhStreamId hdr
      flags = yhFlags hdr
      delta = yhLength hdr
  -- Handle SYN flag: create new inbound stream (with parity + duplicate validation)
  when (flagSYN flags) $ do
    valid <- atomically $ validateInboundSYN sess sid
    if not valid
      then sendGoAway sess GoAwayProtocol
      else do
        stream <- newStream sess sid StreamSYNReceived
        atomically $ do
          modifyTVar' (ysStreams sess) (Map.insert sid stream)
          writeTQueue (ysAcceptCh sess) stream
  -- Handle ACK flag
  when (flagACK flags) $ do
    mStream <- atomically $ Map.lookup sid <$> readTVar (ysStreams sess)
    case mStream of
      Just stream -> atomically $ do
        st <- readTVar (ysState stream)
        case st of
          StreamSYNSent -> writeTVar (ysState stream) StreamEstablished
          _ -> pure ()
      Nothing -> pure ()
  -- Update send window
  when (delta > 0) $ do
    mStream <- atomically $ Map.lookup sid <$> readTVar (ysStreams sess)
    case mStream of
      Just stream -> atomically $ do
        w <- readTVar (ysSendWindow stream)
        writeTVar (ysSendWindow stream) (w + delta)
      Nothing -> pure ()
  -- Handle FIN flag
  when (flagFIN flags) $ do
    mStream <- atomically $ Map.lookup sid <$> readTVar (ysStreams sess)
    case mStream of
      Just stream -> atomically $ do
        st <- readTVar (ysState stream)
        case st of
          StreamEstablished -> writeTVar (ysState stream) StreamRemoteClose
          StreamLocalClose -> writeTVar (ysState stream) StreamClosed
          _ -> pure ()
      Nothing -> pure ()
  -- Handle RST flag
  when (flagRST flags) $ do
    mStream <- atomically $ Map.lookup sid <$> readTVar (ysStreams sess)
    case mStream of
      Just stream -> atomically $ writeTVar (ysState stream) StreamReset
      Nothing -> pure ()

-- | Handle a Ping frame (StreamID must be 0).
-- SYN: echo back with ACK flag and same opaque value.
-- ACK: resolve the matching pending ping.
handlePing :: YamuxSession -> YamuxHeader -> IO ()
handlePing sess hdr
  | flagSYN (yhFlags hdr) = do
      -- Echo back Ping with ACK
      let respHdr =
            YamuxHeader
              { yhVersion = 0
              , yhType = FramePing
              , yhFlags = defaultFlags {flagACK = True}
              , yhStreamId = 0
              , yhLength = yhLength hdr -- echo opaque value
              }
      atomically $ writeTQueue (ysSendCh sess) (respHdr, BS.empty)
  | flagACK (yhFlags hdr) = do
      -- Resolve pending ping
      let pingId = yhLength hdr
      atomically $ do
        pMap <- readTVar (ysPings sess)
        case Map.lookup pingId pMap of
          Just waiter -> putTMVar waiter ()
          Nothing -> pure ()
  | otherwise = pure ()

-- | Handle a GoAway frame (StreamID must be 0).
-- Parse error code and set ysRemoteGoAway.
handleGoAway :: YamuxSession -> YamuxHeader -> IO ()
handleGoAway sess _hdr = do
  atomically $ writeTVar (ysRemoteGoAway sess) True

-- | Send loop: dequeues frames from ysSendCh and writes to transport.
sendLoop :: YamuxSession -> IO ()
sendLoop sess = go
  where
    go = do
      (hdr, payload) <- atomically $ readTQueue (ysSendCh sess)
      ysWrite sess (encodeHeader hdr)
      when (BS.length payload > 0) $ ysWrite sess payload
      go

-- | Create a new YamuxStream with the given initial state.
newStream :: YamuxSession -> Word32 -> StreamState -> IO YamuxStream
newStream sess sid initState = do
  stateVar <- newTVarIO initState
  sendWin <- newTVarIO initialWindowSize
  recvWin <- newTVarIO initialWindowSize
  recvBuf <- newTQueueIO
  sendNotify <- newEmptyTMVarIO
  pure
    YamuxStream
      { ysStreamId = sid
      , ysState = stateVar
      , ysSendWindow = sendWin
      , ysRecvWindow = recvWin
      , ysRecvBuf = recvBuf
      , ysSendNotify = sendNotify
      , ysSession = sess
      }

-- | Validate an inbound SYN stream ID for parity and uniqueness.
-- Returns True if valid, False if protocol error (caller must send GoAway).
-- Remote peers must use the opposite parity: client expects even, server expects odd.
validateInboundSYN :: YamuxSession -> Word32 -> STM Bool
validateInboundSYN sess sid = do
  let validParity = case ysRole sess of
        -- Server expects odd IDs (from client)
        RoleServer -> odd sid
        -- Client expects even IDs (from server)
        RoleClient -> even sid
  if sid == 0 || not validParity
    then pure False
    else do
      streams <- readTVar (ysStreams sess)
      pure (not (Map.member sid streams))

-- | Helper: execute action when condition is True.
when :: Bool -> IO () -> IO ()
when True action = action
when False _ = pure ()
