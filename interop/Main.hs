-- | Interop test daemon for libp2p test-plans framework.
--
-- Reads configuration from environment variables, acts as either
-- a listener or dialer, coordinating via Redis.
--
-- Environment variables:
--   transport          - must be "tcp"
--   security           - must be "noise"
--   muxer              - must be "yamux"
--   is_dialer          - "true" or "false"
--   ip                 - bind address (default: "0.0.0.0")
--   redis_addr         - Redis host:port (default: "redis:6379")
--   test_timeout_seconds - timeout in seconds (default: "180")
--   test_mode          - "ping" (default) or "gossipsub"
module Main (main) where

import Control.Concurrent (threadDelay, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, writeTVar)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Database.Redis as Redis
import LibP2P
  ( GossipSubNode (..)
  , Multiaddr (..)
  , PeerId
  , PingResult (..)
  , Protocol (..)
  , addTransport
  , defaultConnectionGater
  , defaultGossipSubParams
  , dial
  , fromPublicKey
  , fromText
  , generateKeyPair
  , gossipJoin
  , gossipPublish
  , newGossipSubNode
  , newSwitch
  , newTCPTransport
  , peerIdBytes
  , registerPingHandler
  , sendPing
  , splitP2P
  , startGossipSub
  , stopGossipSub
  , switchClose
  , switchListen
  , toBase58
  , toText
  , GossipSubParams (..)
  )
import LibP2P.Crypto.Key (publicKey)
import LibP2P.Protocol.GossipSub.Types (GossipSubRouter (..), PubSubMessage (..))
import LibP2P.Switch.Types (Switch)
import Network.Socket
  ( AddrInfo (..)
  , SockAddr (..)
  , defaultHints
  , getAddrInfo
  , hostAddressToTuple
  )
import qualified Network.Socket as Socket
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Timeout (timeout)

main :: IO ()
main = do
  -- Read environment variables
  transport <- getEnvRequired "transport"
  security  <- getEnvRequired "security"
  muxer     <- getEnvRequired "muxer"
  isDialer  <- getEnvRequired "is_dialer"
  ip        <- fromMaybe "0.0.0.0" <$> lookupEnv "ip"
  redisAddr <- fromMaybe "redis:6379" <$> lookupEnv "redis_addr"
  timeoutS  <- maybe 180 read <$> lookupEnv "test_timeout_seconds"

  testMode  <- fromMaybe "ping" <$> lookupEnv "test_mode"

  -- Validate supported protocols
  case validateProtocols transport security muxer of
    Left err -> do
      hPutStrLn stderr $ "Unsupported configuration: " ++ err
      exitFailure
    Right () -> pure ()

  -- Generate identity
  ekp <- generateKeyPair
  case ekp of
    Left err -> do
      hPutStrLn stderr $ "Key generation failed: " ++ err
      exitFailure
    Right kp -> do
      let pid = fromPublicKey (publicKey kp)
      logInfo $ "PeerId: " ++ T.unpack (toBase58 pid)

      -- Create Switch
      sw <- newSwitch pid kp
      tcp <- newTCPTransport
      addTransport sw tcp

      -- Connect to Redis
      let (redisHost, redisPort) = parseHostPort redisAddr
      let redisConnInfo = Redis.defaultConnectInfo
            { Redis.connectHost = redisHost
            , Redis.connectPort = Redis.PortNumber (fromIntegral redisPort)
            }
      redisConn <- Redis.checkedConnect redisConnInfo

      case testMode of
        "gossipsub" -> case isDialer of
          "false" -> runGossipSubListener sw pid ip redisConn timeoutS
          "true"  -> runGossipSubDialer sw pid redisConn timeoutS
          other   -> do
            hPutStrLn stderr $ "Invalid is_dialer value: " ++ other
            switchClose sw
            exitFailure
        _ -> do
          registerPingHandler sw
          case isDialer of
            "false" -> runListener sw pid ip redisConn timeoutS
            "true"  -> runDialer sw pid redisConn timeoutS
            other   -> do
              hPutStrLn stderr $ "Invalid is_dialer value: " ++ other
              switchClose sw
              exitFailure

-- | Listener mode: bind, publish address to Redis, wait.
runListener :: Switch -> PeerId -> String -> Redis.Connection -> Int -> IO ()
runListener sw pid ip redisConn timeoutS = do
  let bindAddr = case fromText (T.pack ("/ip4/" ++ ip ++ "/tcp/0")) of
        Right ma -> ma
        Left err -> error $ "Invalid bind address: " ++ err

  addrs <- switchListen sw defaultConnectionGater [bindAddr]
  case addrs of
    [] -> do
      hPutStrLn stderr "switchListen returned no addresses"
      switchClose sw
      exitFailure
    (listenAddr : _) -> do
      -- Resolve actual IP if bound to 0.0.0.0
      actualAddr <- resolveListenAddr listenAddr ip

      -- Construct full multiaddr with /p2p/<peerId>
      let peerIdMH = peerIdBytes pid
      let fullAddr = encapsulateP2P actualAddr peerIdMH
      let addrText = toText fullAddr

      logInfo $ "Listening on: " ++ T.unpack addrText

      -- Publish to Redis
      let addrBS = TE.encodeUtf8 addrText
      result <- Redis.runRedis redisConn $ Redis.rpush "listenerAddr" [addrBS]
      case result of
        Left err -> do
          hPutStrLn stderr $ "Redis RPUSH failed: " ++ show err
          switchClose sw
          exitFailure
        Right _ -> do
          logInfo "Address published to Redis, waiting..."
          -- The test runner kills this process once the dialer finishes.
          -- Reaching the timeout means we were never successfully dialed,
          -- which is a test failure (transport-interop README, Listener step 5).
          threadDelay (timeoutS * 1000000)
          hPutStrLn stderr "Listener timed out waiting to be dialed"
          switchClose sw
          exitFailure

-- | Dialer mode: get address from Redis, dial, ping, output JSON.
runDialer :: Switch -> PeerId -> Redis.Connection -> Int -> IO ()
runDialer sw _pid redisConn timeoutS = do
  logInfo "Waiting for listener address from Redis..."

  -- BLPOP with timeout
  result <- Redis.runRedis redisConn $
    Redis.blpop ["listenerAddr"] (fromIntegral timeoutS)

  case result of
    Left err -> do
      hPutStrLn stderr $ "Redis BLPOP failed: " ++ show err
      switchClose sw
      exitFailure
    Right Nothing -> do
      hPutStrLn stderr "Timed out waiting for listener address"
      switchClose sw
      exitFailure
    Right (Just (_key, addrBS)) -> do
      let addrText = TE.decodeUtf8 addrBS
      logInfo $ "Got listener address: " ++ T.unpack addrText

      case fromText addrText of
        Left err -> do
          hPutStrLn stderr $ "Failed to parse multiaddr: " ++ err
          switchClose sw
          exitFailure
        Right fullAddr -> case splitP2P fullAddr of
          Nothing -> do
            hPutStrLn stderr "Multiaddr has no /p2p/ component"
            switchClose sw
            exitFailure
          Just (transportAddr, remotePeerId) -> do
            logInfo $ "Dialing peer: " ++ T.unpack (toBase58 remotePeerId)

            -- handshakeStartInstant, recorded before connecting
            -- (transport-interop README, Dialer step 4).
            t0 <- getCurrentTime

            dialResult <- dial sw remotePeerId [transportAddr]
            case dialResult of
              Left err -> do
                hPutStrLn stderr $ "Dial failed: " ++ show err
                switchClose sw
                exitFailure
              Right conn -> do
                -- A single ping (README steps 5-6): its round-trip time is
                -- pingRTT, and the total elapsed since t0 is handshakePlusOneRTT.
                pingResult <- sendPing conn
                t1 <- getCurrentTime

                case pingResult of
                  Left err -> do
                    hPutStrLn stderr $ "Ping failed: " ++ show err
                    switchClose sw
                    exitFailure
                  Right pr -> do
                    let handshakePlusOneRTT = realToFrac (diffUTCTime t1 t0) * 1000 :: Double
                        pingRTTMs = realToFrac (pingRTT pr) * 1000 :: Double

                    -- Output JSON (pingRTTMilllis triple-L is the upstream spelling)
                    let jsonOutput = object
                          [ "handshakePlusOneRTTMillis" .= handshakePlusOneRTT
                          , "pingRTTMilllis" .= pingRTTMs
                          ]
                    LBS8.putStrLn (Aeson.encode jsonOutput)
                    hFlush stdout

                    switchClose sw
                    exitSuccess

-- | GossipSub listener: join topic, wait for message, reply, report to Redis.
runGossipSubListener :: Switch -> PeerId -> String -> Redis.Connection -> Int -> IO ()
runGossipSubListener sw pid ip redisConn timeoutS = do
  let gsParams = defaultGossipSubParams { paramHeartbeatInterval = 60.0 }
  gsNode <- newGossipSubNode sw gsParams
  startGossipSub gsNode

  -- Set up message callback
  msgMVar <- newEmptyMVar
  atomically $ writeTVar (gsOnMessage (gsnRouter gsNode))
    (\topic msg -> putMVar msgMVar (topic, msgData msg))

  let bindAddr = case fromText (T.pack ("/ip4/" ++ ip ++ "/tcp/0")) of
        Right ma -> ma
        Left err -> error $ "Invalid bind address: " ++ err

  addrs <- switchListen sw defaultConnectionGater [bindAddr]
  case addrs of
    [] -> do
      hPutStrLn stderr "switchListen returned no addresses"
      stopGossipSub gsNode; switchClose sw
      exitFailure
    (listenAddr : _) -> do
      actualAddr <- resolveListenAddr listenAddr ip
      let peerIdMH = peerIdBytes pid
      let fullAddr = encapsulateP2P actualAddr peerIdMH
      let addrText = toText fullAddr

      logInfo $ "GossipSub listener on: " ++ T.unpack addrText

      -- Publish address to Redis
      let addrBS = TE.encodeUtf8 addrText
      result <- Redis.runRedis redisConn $ Redis.rpush "listenerAddr" [addrBS]
      case result of
        Left err -> do
          hPutStrLn stderr $ "Redis RPUSH failed: " ++ show err
          stopGossipSub gsNode; switchClose sw
          exitFailure
        Right _ -> do
          logInfo "Address published to Redis"

          -- Join topic
          gossipJoin gsNode "interop-gossipsub-test"
          logInfo "Joined topic interop-gossipsub-test"

          -- Re-announce subscriptions to peers that connect after join.
          -- onNewConnection should handle this but cross-impl timing
          -- can cause the announcement to be lost.
          _ <- forkIO $ reannounceLoop gsNode "interop-gossipsub-test" 10

          -- Wait for message
          mResult <- timeout (timeoutS * 1000000) $ takeMVar msgMVar
          case mResult of
            Nothing -> do
              hPutStrLn stderr "Timeout waiting for GossipSub message"
              let jsonOutput = object
                    [ "gossipSubInterop" .= False
                    , "role" .= ("listener" :: T.Text)
                    , "error" .= ("timeout" :: T.Text)
                    ]
              void $ Redis.runRedis redisConn $
                Redis.rpush "gossipResult" [LBS.toStrict (Aeson.encode jsonOutput)]
              stopGossipSub gsNode; switchClose sw
              exitFailure
            Just (_topic, msgBytes) -> do
              let received = BS8.unpack msgBytes
              logInfo $ "Received message: " ++ received

              -- Publish reply
              threadDelay 500000  -- 0.5s for stability
              let replyMsg = "hs-reply-to-" ++ received
              gossipPublish gsNode "interop-gossipsub-test" (BS8.pack replyMsg)
              logInfo $ "Published reply: " ++ replyMsg

              let jsonOutput = object
                    [ "gossipSubInterop" .= True
                    , "role" .= ("listener" :: T.Text)
                    , "messageReceived" .= received
                    , "messageSent" .= replyMsg
                    ]
              void $ Redis.runRedis redisConn $
                Redis.rpush "gossipResult" [LBS.toStrict (Aeson.encode jsonOutput)]

              -- Keep alive briefly for reply delivery
              threadDelay 3000000
              stopGossipSub gsNode; switchClose sw

-- | GossipSub dialer: connect, publish message, wait for reply, report JSON.
runGossipSubDialer :: Switch -> PeerId -> Redis.Connection -> Int -> IO ()
runGossipSubDialer sw _pid redisConn timeoutS = do
  let gsParams = defaultGossipSubParams { paramHeartbeatInterval = 60.0 }
  gsNode <- newGossipSubNode sw gsParams
  startGossipSub gsNode

  -- Set up message callback — filter out our own message
  let sentMsg = "hs-rust-interop-test" :: BS8.ByteString
  msgMVar <- newEmptyMVar
  atomically $ writeTVar (gsOnMessage (gsnRouter gsNode))
    (\topic msg -> do
      let d = msgData msg
      logInfo $ "gsOnMessage got: " ++ BS8.unpack d
      if d /= sentMsg
        then putMVar msgMVar (topic, d)
        else pure ()
    )

  logInfo "GossipSub dialer: waiting for listener address from Redis..."

  result <- Redis.runRedis redisConn $
    Redis.blpop ["listenerAddr"] (fromIntegral timeoutS)

  case result of
    Left err -> do
      hPutStrLn stderr $ "Redis BLPOP failed: " ++ show err
      stopGossipSub gsNode; switchClose sw
      exitFailure
    Right Nothing -> do
      hPutStrLn stderr "Timed out waiting for listener address"
      stopGossipSub gsNode; switchClose sw
      exitFailure
    Right (Just (_key, addrBS)) -> do
      let addrText = TE.decodeUtf8 addrBS
      logInfo $ "Got listener address: " ++ T.unpack addrText

      case fromText addrText of
        Left err -> do
          hPutStrLn stderr $ "Failed to parse multiaddr: " ++ err
          stopGossipSub gsNode; switchClose sw
          exitFailure
        Right fullAddr -> case splitP2P fullAddr of
          Nothing -> do
            hPutStrLn stderr "Multiaddr has no /p2p/ component"
            stopGossipSub gsNode; switchClose sw
            exitFailure
          Just (transportAddr, remotePeerId) -> do
            logInfo $ "Dialing peer: " ++ T.unpack (toBase58 remotePeerId)

            t0 <- getCurrentTime

            dialResult <- dial sw remotePeerId [transportAddr]
            case dialResult of
              Left err -> do
                hPutStrLn stderr $ "Dial failed: " ++ show err
                stopGossipSub gsNode; switchClose sw
                exitFailure
              Right _conn -> do
                -- Wait for mux + stream setup
                threadDelay 2000000

                -- Join topic
                gossipJoin gsNode "interop-gossipsub-test"
                logInfo "Joined topic interop-gossipsub-test"

                -- Wait for subscription propagation
                threadDelay 2000000

                -- Publish test message
                let testMsg = "hs-rust-interop-test"
                gossipPublish gsNode "interop-gossipsub-test" testMsg
                logInfo $ "Published: " ++ BS8.unpack testMsg

                -- Wait for reply
                mResult <- timeout (timeoutS * 1000000) $ takeMVar msgMVar
                t1 <- getCurrentTime
                let roundTripMs = realToFrac (diffUTCTime t1 t0) * 1000 :: Double

                case mResult of
                  Nothing -> do
                    hPutStrLn stderr "Timeout waiting for GossipSub reply"
                    let jsonOutput = object
                          [ "gossipSubInterop" .= False
                          , "role" .= ("dialer" :: T.Text)
                          , "error" .= ("timeout" :: T.Text)
                          ]
                    LBS8.putStrLn (Aeson.encode jsonOutput)
                    hFlush stdout
                    stopGossipSub gsNode; switchClose sw
                    exitFailure
                  Just (_topic, replyBytes) -> do
                    let received = BS8.unpack replyBytes
                    logInfo $ "Received reply: " ++ received

                    let jsonOutput = object
                          [ "gossipSubInterop" .= True
                          , "role" .= ("dialer" :: T.Text)
                          , "messageSent" .= ("hs-rust-interop-test" :: T.Text)
                          , "messageReceived" .= received
                          , "roundTripMs" .= roundTripMs
                          ]
                    LBS8.putStrLn (Aeson.encode jsonOutput)
                    hFlush stdout

                    void $ Redis.runRedis redisConn $
                      Redis.rpush "gossipResult" [LBS.toStrict (Aeson.encode jsonOutput)]

                    stopGossipSub gsNode; switchClose sw
                    exitSuccess

-- | Periodically re-announce our topic subscription to all connected peers.
-- Ensures cross-implementation peers that connect after we've joined the topic
-- learn about our subscription even if the initial announcement is lost.
reannounceLoop :: GossipSubNode -> T.Text -> Int -> IO ()
reannounceLoop gsNode topic iterations = go iterations
  where
    go 0 = pure ()
    go n = do
      threadDelay 1000000  -- 1 second interval
      -- Re-join broadcasts subscription to all connected peers
      gossipJoin gsNode topic
      go (n - 1)

-- | Discard the result of an IO action.
void :: IO a -> IO ()
void action = action >> pure ()

-- | Validate that we support the requested protocol combination.
validateProtocols :: String -> String -> String -> Either String ()
validateProtocols transport security muxer = do
  case transport of
    "tcp" -> pure ()
    other -> Left $ "transport " ++ other ++ " not supported (only tcp)"
  case security of
    "noise" -> pure ()
    other -> Left $ "security " ++ other ++ " not supported (only noise)"
  case muxer of
    "yamux" -> pure ()
    other -> Left $ "muxer " ++ other ++ " not supported (only yamux)"

-- | Parse "host:port" string.
parseHostPort :: String -> (String, Int)
parseHostPort s = case break (== ':') s of
  (host, ':' : portStr) -> (host, read portStr)
  (host, _) -> (host, 6379)

-- | Resolve 0.0.0.0 to actual container IP for Docker networking.
resolveListenAddr :: Multiaddr -> String -> IO Multiaddr
resolveListenAddr addr ip
  | ip == "0.0.0.0" = do
      actualIP <- discoverContainerIP
      case protocols addr of
        (IP4 _ : rest) ->
          case fromText (T.pack ("/ip4/" ++ actualIP)) of
            Right (Multiaddr [IP4 w]) -> pure $ Multiaddr (IP4 w : rest)
            _ -> pure addr
        _ -> pure addr
  | otherwise = pure addr
  where
    protocols (Multiaddr ps) = ps

-- | Discover actual container IP via hostname resolution.
-- In Docker, HOSTNAME is set to the container ID, which resolves
-- to the container's IP address on the Docker network.
discoverContainerIP :: IO String
discoverContainerIP = do
  -- Docker sets HOSTNAME to container ID
  mHostname <- lookupEnv "HOSTNAME"
  case mHostname of
    Nothing -> pure "0.0.0.0"
    Just hostname -> do
      addrs <- getAddrInfo (Just defaultHints) (Just hostname) Nothing :: IO [AddrInfo]
      case find isNonLoopbackIPv4 addrs of
        Just ai -> pure $ sockAddrToIP (Socket.addrAddress ai)
        Nothing -> pure "0.0.0.0"

-- | Extract just the IP string from a SockAddr.
sockAddrToIP :: SockAddr -> String
sockAddrToIP (SockAddrInet _ hostAddr) =
  let (a, b, c, d) = hostAddressToTuple hostAddr
   in show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
sockAddrToIP other = show other

-- | Check if an AddrInfo is a non-loopback IPv4 address.
isNonLoopbackIPv4 :: AddrInfo -> Bool
isNonLoopbackIPv4 ai = case Socket.addrAddress ai of
  SockAddrInet _ hostAddr ->
    let (a, _, _, _) = hostAddressToTuple hostAddr
     in a /= 127
  _ -> False

-- | Encapsulate a /p2p/<peerId> suffix onto a multiaddr.
encapsulateP2P :: Multiaddr -> BS8.ByteString -> Multiaddr
encapsulateP2P (Multiaddr ps) mhBytes = Multiaddr (ps ++ [P2P mhBytes])

-- | Get a required environment variable, failing if not set.
getEnvRequired :: String -> IO String
getEnvRequired name = do
  val <- lookupEnv name
  case val of
    Just v  -> pure v
    Nothing -> do
      hPutStrLn stderr $ "Missing required environment variable: " ++ name
      exitFailure

-- | Log to stderr.
logInfo :: String -> IO ()
logInfo msg = hPutStrLn stderr msg >> hFlush stderr
