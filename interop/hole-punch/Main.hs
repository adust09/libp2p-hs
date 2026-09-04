-- | Hole-punch / DCUtR test daemon for libp2p/unified-testing.
--
-- Implements docs/write-a-hole-punch-test-app.md: Redis coordination
-- namespaced by TEST_KEY, three roles (relay / dialer / listener),
-- YAML results on stdout (dialer only), logging on stderr.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (filterM, forever)
import qualified Data.ByteString.Char8 as BS8
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import qualified Database.Redis as Redis
import LibP2P
  ( Connection (..)
  , Multiaddr (..)
  , PeerId
  , Protocol (..)
  , Switch
  , addTransport
  , defaultConnectionGater
  , defaultNATConfig
  , dial
  , fromPublicKey
  , fromText
  , generateKeyPair
  , newSwitch
  , newTCPTransport
  , parsePeerId
  , peerIdBytes
  , registerIdentifyHandlers
  , registerNATHandlers
  , registerPingHandler
  , sendPing
  , switchClose
  , switchListen
  , toBase58
  , toText
  )
import LibP2P.Crypto.Key (publicKey)
import LibP2P.Multiaddr (encapsulate, isRelayedAddr)
import LibP2P.Switch.ConnPool (lookupAllConns)
import LibP2P.Switch.Types (ConnState (..), swConnPool, swLocalPeerId)
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
import Text.Printf (printf)

testTimeoutSeconds :: Int
testTimeoutSeconds = 180

main :: IO ()
main = do
  redisAddr <- fromMaybe "hole-punch-redis:6379" <$> lookupEnv "REDIS_ADDR"
  testKey <- getEnvRequired "TEST_KEY"
  transport <- getEnvRequired "TRANSPORT"
  security <- lookupEnv "SECURE_CHANNEL"
  muxer <- lookupEnv "MUXER"
  case validateProtocols transport security muxer of
    Left err -> die err
    Right () -> pure ()
  sw <- newNode
  redisConn <- connectRedis redisAddr
  isRelay <- lookupEnv "IS_RELAY"
  isDialer <- lookupEnv "IS_DIALER"
  case isRelay of
    Just "true" -> runRelay sw redisConn testKey
    _ -> case isDialer of
      Just "true" -> runDialer sw redisConn testKey
      Just "false" -> runListener sw redisConn testKey
      other -> dieWith sw ("Invalid IS_DIALER value: " ++ show other)

newNode :: IO Switch
newNode = do
  ekp <- generateKeyPair
  kp <- either die pure ekp
  let pid = fromPublicKey (publicKey kp)
  logInfo $ "PeerId: " ++ T.unpack (toBase58 pid)
  sw <- newSwitch pid kp
  tcp <- newTCPTransport
  addTransport sw tcp
  registerIdentifyHandlers sw
  registerPingHandler sw
  _ <- registerNATHandlers sw defaultNATConfig
  pure sw

runRelay :: Switch -> Redis.Connection -> String -> IO ()
runRelay sw redisConn testKey = do
  ip <- fromMaybe "0.0.0.0" <$> lookupEnv "RELAY_IP"
  addrText <- listenTcp sw ip
  redisSet redisConn (redisKey testKey "relay_multiaddr") addrText
  logInfo $ "Relay listening on " ++ T.unpack addrText
  forever $ threadDelay 3600000000

runListener :: Switch -> Redis.Connection -> String -> IO ()
runListener sw redisConn testKey = do
  _ <- listenTcp sw =<< peerIp
  relayMA <- waitRelayAddr redisConn testKey sw
  reserveOnRelay sw relayMA
  redisSet redisConn (redisKey testKey "listener_peer_id") (toBase58 (swLocalPeerId sw))
  logInfo $ "Published listener peer id " ++ T.unpack (toBase58 (swLocalPeerId sw))
  forever $ threadDelay 3600000000

runDialer :: Switch -> Redis.Connection -> String -> IO ()
runDialer sw redisConn testKey = do
  _ <- listenTcp sw =<< peerIp
  relayMA <- waitRelayAddr redisConn testKey sw
  reserveOnRelay sw relayMA
  listenerId <- waitListenerId redisConn testKey sw
  logInfo $ "Dialing listener via relay: " ++ T.unpack (toBase58 listenerId)
  t0 <- getCurrentTime
  let circuitAddr =
        encapsulate relayMA (Multiaddr [P2PCircuit, P2P (peerIdBytes listenerId)])
  _ <- dial sw listenerId [circuitAddr]
        >>= either (\err -> dieWith sw ("Circuit dial failed: " ++ show err)) pure
  direct <- waitDirectConn sw listenerId (testTimeoutSeconds * 5)
  case direct of
    Nothing -> dieWith sw "DCUtR failed: no direct connection within timeout"
    Just conn -> finishDial sw conn t0

finishDial :: Switch -> Connection -> UTCTime -> IO ()
finishDial sw conn t0 = do
  t1 <- getCurrentTime
  pingResult <- sendPing sw conn
  case pingResult of
    Left err -> dieWith sw ("Ping over direct connection failed: " ++ show err)
    Right _ -> do
      let ms = realToFrac (diffUTCTime t1 t0) * 1000 :: Double
      logInfo "Direct connection established via DCUtR"
      printf "handshakeTime: %.2f\nunit: ms\n" ms
      hFlush stdout
      switchClose sw
      exitSuccess

listenTcp :: Switch -> String -> IO T.Text
listenTcp sw ip = do
  bindAddr <- either (\err -> dieWith sw ("Invalid bind address: " ++ err)) pure
                (fromText (T.pack ("/ip4/" ++ ip ++ "/tcp/0")))
  addrs <- switchListen sw defaultConnectionGater [bindAddr]
  case addrs of
    [] -> dieWith sw "switchListen returned no TCP addresses"
    (listenAddr : _) -> do
      actual <- resolveListenAddr listenAddr ip
      let full = encapsulate actual (Multiaddr [P2P (peerIdBytes (swLocalPeerId sw))])
      logInfo $ "Listening on " ++ T.unpack (toText full)
      pure (toText full)

reserveOnRelay :: Switch -> Multiaddr -> IO ()
reserveOnRelay sw relayMA = do
  let circuitListen = encapsulate relayMA (Multiaddr [P2PCircuit])
  _ <- switchListen sw defaultConnectionGater [circuitListen]
  logInfo $ "Reserved on relay " ++ T.unpack (toText relayMA)

waitRelayAddr :: Redis.Connection -> String -> Switch -> IO Multiaddr
waitRelayAddr redisConn testKey sw = do
  raw <- pollRedis redisConn (redisKey testKey "relay_multiaddr")
           >>= maybe (dieWith sw "Timed out waiting for relay multiaddr") pure
  either (\err -> dieWith sw ("Bad relay multiaddr: " ++ err)) pure (fromText (TE.decodeUtf8 raw))

waitListenerId :: Redis.Connection -> String -> Switch -> IO PeerId
waitListenerId redisConn testKey sw = do
  raw <- pollRedis redisConn (redisKey testKey "listener_peer_id")
           >>= maybe (dieWith sw "Timed out waiting for listener peer id") pure
  either (\err -> dieWith sw ("Failed to parse listener peer id: " ++ err)) pure
    (parsePeerId (TE.decodeUtf8 raw))

waitDirectConn :: Switch -> PeerId -> Int -> IO (Maybe Connection)
waitDirectConn sw pid attempts = go attempts
  where
    go 0 = pure Nothing
    go n = do
      conns <- atomically $ lookupAllConns (swConnPool sw) pid
      openDirect <- filterM isOpenDirect conns
      case openDirect of
        (c : _) -> pure (Just c)
        [] -> threadDelay 200000 >> go (n - 1)

isOpenDirect :: Connection -> IO Bool
isOpenDirect c = do
  st <- atomically $ readTVar (connState c)
  pure (st == ConnOpen && not (isRelayedAddr (connRemoteAddr c)))

peerIp :: IO String
peerIp = do
  mPeer <- lookupEnv "PEER_IP"
  mListener <- lookupEnv "LISTENER_IP"
  pure (fromMaybe "0.0.0.0" (mPeer <|> mListener))

redisKey :: String -> String -> BS8.ByteString
redisKey testKey suffix = BS8.pack (testKey ++ "_" ++ suffix)

connectRedis :: String -> IO Redis.Connection
connectRedis redisAddr = do
  let (host, port) = parseHostPort redisAddr
  Redis.checkedConnect Redis.defaultConnectInfo
    { Redis.connectHost = host
    , Redis.connectPort = Redis.PortNumber (fromIntegral port)
    }

redisSet :: Redis.Connection -> BS8.ByteString -> T.Text -> IO ()
redisSet conn key value = do
  result <- Redis.runRedis conn $ Redis.set key (TE.encodeUtf8 value)
  case result of
    Left err -> die ("Redis SET failed: " ++ show err)
    Right _ -> pure ()

pollRedis :: Redis.Connection -> BS8.ByteString -> IO (Maybe BS8.ByteString)
pollRedis conn key = go (testTimeoutSeconds * 2)
  where
    go 0 = pure Nothing
    go n = do
      result <- Redis.runRedis conn $ Redis.get key
      case result of
        Right (Just value) -> pure (Just value)
        _ -> threadDelay 500000 >> go (n - 1)

validateProtocols :: String -> Maybe String -> Maybe String -> Either String ()
validateProtocols transport security muxer = do
  case transport of
    "tcp" -> pure ()
    other -> Left $ "transport " ++ other ++ " not supported (only tcp)"
  case security of
    Just "noise" -> pure ()
    Just other -> Left $ "secure channel " ++ other ++ " not supported (only noise)"
    Nothing -> Left "SECURE_CHANNEL not set (required for tcp)"
  case muxer of
    Just "yamux" -> pure ()
    Just other -> Left $ "muxer " ++ other ++ " not supported (only yamux)"
    Nothing -> Left "MUXER not set (required for tcp)"

parseHostPort :: String -> (String, Int)
parseHostPort s = case break (== ':') s of
  (host, ':' : portStr) -> (host, read portStr)
  (host, _) -> (host, 6379)

resolveListenAddr :: Multiaddr -> String -> IO Multiaddr
resolveListenAddr addr ip
  | ip == "0.0.0.0" = do
      actualIP <- discoverContainerIP
      case addr of
        Multiaddr (IP4 _ : rest) ->
          case fromText (T.pack ("/ip4/" ++ actualIP)) of
            Right (Multiaddr [IP4 w]) -> pure $ Multiaddr (IP4 w : rest)
            _ -> pure addr
        _ -> pure addr
  | otherwise = pure addr

discoverContainerIP :: IO String
discoverContainerIP = do
  mHostname <- lookupEnv "HOSTNAME"
  case mHostname of
    Nothing -> pure "0.0.0.0"
    Just hostname -> do
      addrs <- getAddrInfo (Just defaultHints) (Just hostname) Nothing :: IO [AddrInfo]
      case find isNonLoopbackIPv4 addrs of
        Just ai -> pure $ sockAddrToIP (Socket.addrAddress ai)
        Nothing -> pure "0.0.0.0"

sockAddrToIP :: SockAddr -> String
sockAddrToIP (SockAddrInet _ hostAddr) =
  let (a, b, c, d) = hostAddressToTuple hostAddr
   in show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
sockAddrToIP _ = "0.0.0.0"

isNonLoopbackIPv4 :: AddrInfo -> Bool
isNonLoopbackIPv4 ai = case Socket.addrAddress ai of
  SockAddrInet _ hostAddr ->
    let (a, _, _, _) = hostAddressToTuple hostAddr
     in a /= 127
  _ -> False

getEnvRequired :: String -> IO String
getEnvRequired name = do
  val <- lookupEnv name
  case val of
    Just v -> pure v
    Nothing -> die ("Missing required environment variable: " ++ name)

dieWith :: Switch -> String -> IO a
dieWith sw msg = do
  hPutStrLn stderr msg
  switchClose sw
  exitFailure

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

logInfo :: String -> IO ()
logInfo msg = hPutStrLn stderr msg >> hFlush stderr
