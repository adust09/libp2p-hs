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
module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Database.Redis as Redis
import Network.LibP2P
  ( Multiaddr (..)
  , PeerId
  , PingResult (..)
  , Protocol (..)
  , addTransport
  , defaultConnectionGater
  , dial
  , fromPublicKey
  , fromText
  , generateKeyPair
  , newSwitch
  , newTCPTransport
  , peerIdBytes
  , registerPingHandler
  , sendPing
  , splitP2P
  , switchClose
  , switchListen
  , toBase58
  , toText
  )
import Network.LibP2P.Crypto.Key (publicKey)
import Network.LibP2P.Switch.Types (Switch)
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
      registerPingHandler sw

      -- Connect to Redis
      let (redisHost, redisPort) = parseHostPort redisAddr
      let redisConnInfo = Redis.defaultConnectInfo
            { Redis.connectHost = redisHost
            , Redis.connectPort = Redis.PortNumber (fromIntegral redisPort)
            }
      redisConn <- Redis.checkedConnect redisConnInfo

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
          -- Block until timeout (framework kills container)
          threadDelay (timeoutS * 1000000)
          switchClose sw

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

            -- Measure handshake + first ping
            t0 <- getCurrentTime

            dialResult <- dial sw remotePeerId [transportAddr]
            case dialResult of
              Left err -> do
                hPutStrLn stderr $ "Dial failed: " ++ show err
                switchClose sw
                exitFailure
              Right conn -> do
                -- First ping (included in handshake timing)
                pingResult1 <- sendPing conn
                t1 <- getCurrentTime

                case pingResult1 of
                  Left err -> do
                    hPutStrLn stderr $ "First ping failed: " ++ show err
                    switchClose sw
                    exitFailure
                  Right _ -> do
                    let handshakePlusOneRTT = realToFrac (diffUTCTime t1 t0) * 1000 :: Double

                    -- Second ping (standalone RTT measurement)
                    pingResult2 <- sendPing conn
                    case pingResult2 of
                      Left err -> do
                        hPutStrLn stderr $ "Second ping failed: " ++ show err
                        switchClose sw
                        exitFailure
                      Right pr -> do
                        let pingRTTMs = realToFrac (pingRTT pr) * 1000 :: Double

                        -- Output JSON (note: pingRTTMilllis has 3 L's - intentional)
                        let jsonOutput = object
                              [ "handshakePlusOneRTTMillis" .= handshakePlusOneRTT
                              , "pingRTTMilllis" .= pingRTTMs
                              ]
                        LBS8.putStrLn (Aeson.encode jsonOutput)
                        hFlush stdout

                        switchClose sw
                        exitSuccess

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
