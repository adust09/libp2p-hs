-- | TCP transport implementation.
--
-- Provides dial/listen for /ip4/.../tcp/... and /ip6/.../tcp/... multiaddrs.
-- Uses the network library for socket operations.
module Network.LibP2P.Transport.TCP
  ( newTCPTransport
  , multiaddrToHostPort
  , socketToStreamIO
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.IP (IPv6, fromHostAddress6, toHostAddress6)
import Data.Word (Word16, Word32, Word8)
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr (..))
import Network.LibP2P.Multiaddr.Protocol (Protocol (..))
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import Network.LibP2P.Transport.Transport (Listener (..), RawConnection (..), Transport (..))
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

-- | Create a new TCP transport.
newTCPTransport :: IO Transport
newTCPTransport = pure $ Transport
  { transportDial = tcpDial
  , transportListen = tcpListen
  , transportCanDial = canDialTCP
  }

-- | Check if a multiaddr is a TCP address (/ip4/.../tcp/... or /ip6/.../tcp/...).
canDialTCP :: Multiaddr -> Bool
canDialTCP (Multiaddr [IP4 _, TCP _]) = True
canDialTCP (Multiaddr [IP6 _, TCP _]) = True
canDialTCP _ = False

-- | Extract (HostName, ServiceName) from a TCP multiaddr.
multiaddrToHostPort :: Multiaddr -> Either String (String, String)
multiaddrToHostPort (Multiaddr [IP4 w, TCP port]) =
  Right (renderIPv4 w, show port)
multiaddrToHostPort (Multiaddr [IP6 bs, TCP port]) =
  Right (renderIPv6 bs, show port)
multiaddrToHostPort _ =
  Left "multiaddrToHostPort: expected /ip4/.../tcp/... or /ip6/.../tcp/..."

-- | Dial a TCP address by directly constructing a SockAddr from the Multiaddr.
tcpDial :: Multiaddr -> IO RawConnection
tcpDial addr@(Multiaddr [IP4 w, TCP port]) = do
  let hostAddr = NS.tupleToHostAddress (octet 3 w, octet 2 w, octet 1 w, octet 0 w)
      sockAddr = NS.SockAddrInet (fromIntegral port) hostAddr
  sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  NS.connect sock sockAddr
  mkRawConnection sock addr
tcpDial addr@(Multiaddr [IP6 bs, TCP port]) = do
  let ipv6 = bytesToIPv6 bs
      hostAddr6 = toHostAddress6 ipv6
      sockAddr = NS.SockAddrInet6 (fromIntegral port) 0 hostAddr6 0
  sock <- NS.socket NS.AF_INET6 NS.Stream NS.defaultProtocol
  NS.connect sock sockAddr
  mkRawConnection sock addr
tcpDial _ = fail "tcpDial: unsupported multiaddr"

-- | Create a RawConnection from a connected socket.
mkRawConnection :: NS.Socket -> Multiaddr -> IO RawConnection
mkRawConnection sock remoteAddr = do
  localSockAddr <- NS.getSocketName sock
  localAddr <- sockAddrToMultiaddr localSockAddr
  pure RawConnection
    { rcStreamIO = socketToStreamIO sock
    , rcLocalAddr = localAddr
    , rcRemoteAddr = remoteAddr
    , rcClose = NS.close sock
    }

-- | Listen on a TCP address.
tcpListen :: Multiaddr -> IO Listener
tcpListen (Multiaddr [IP4 w, TCP port]) = do
  let hostAddr = NS.tupleToHostAddress (octet 3 w, octet 2 w, octet 1 w, octet 0 w)
      sockAddr = NS.SockAddrInet (fromIntegral port) hostAddr
  sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  NS.setSocketOption sock NS.ReuseAddr 1
  NS.bind sock sockAddr
  NS.listen sock 256
  boundSockAddr <- NS.getSocketName sock
  boundAddr <- sockAddrToMultiaddr boundSockAddr
  pure Listener
    { listenerAccept = do
        (clientSock, clientSockAddr) <- NS.accept sock
        clientAddr <- sockAddrToMultiaddr clientSockAddr
        mkRawConnection clientSock clientAddr
    , listenerClose = NS.close sock
    , listenerAddr = boundAddr
    }
tcpListen (Multiaddr [IP6 bs, TCP port]) = do
  let ipv6 = bytesToIPv6 bs
      hostAddr6 = toHostAddress6 ipv6
      sockAddr = NS.SockAddrInet6 (fromIntegral port) 0 hostAddr6 0
  sock <- NS.socket NS.AF_INET6 NS.Stream NS.defaultProtocol
  NS.setSocketOption sock NS.ReuseAddr 1
  NS.bind sock sockAddr
  NS.listen sock 256
  boundSockAddr <- NS.getSocketName sock
  boundAddr <- sockAddrToMultiaddr boundSockAddr
  pure Listener
    { listenerAccept = do
        (clientSock, clientSockAddr) <- NS.accept sock
        clientAddr <- sockAddrToMultiaddr clientSockAddr
        mkRawConnection clientSock clientAddr
    , listenerClose = NS.close sock
    , listenerAddr = boundAddr
    }
tcpListen _ = fail "tcpListen: unsupported multiaddr"

-- | Convert a Socket to StreamIO.
socketToStreamIO :: NS.Socket -> StreamIO
socketToStreamIO sock = StreamIO
  { streamWrite = NSB.sendAll sock
  , streamReadByte = do
      bs <- NSB.recv sock 1
      if BS.null bs
        then fail "socketToStreamIO: connection closed"
        else pure (BS.head bs)
  , streamClose = NS.close sock
  }

-- | Convert a SockAddr to a Multiaddr.
sockAddrToMultiaddr :: NS.SockAddr -> IO Multiaddr
sockAddrToMultiaddr (NS.SockAddrInet port host) = do
  let (a, b, c, d) = NS.hostAddressToTuple host
      w =
        (fromIntegral a `shiftL` 24)
          .|. (fromIntegral b `shiftL` 16)
          .|. (fromIntegral c `shiftL` 8)
          .|. fromIntegral d
      p = fromIntegral port :: Word16
  pure $ Multiaddr [IP4 w, TCP p]
sockAddrToMultiaddr (NS.SockAddrInet6 port _ host6 _) = do
  let p = fromIntegral port :: Word16
      ipv6 = fromHostAddress6 host6
      bs = ipv6ToBytes ipv6
  pure $ Multiaddr [IP6 bs, TCP p]
sockAddrToMultiaddr other =
  fail $ "sockAddrToMultiaddr: unsupported address type: " <> show other

-- | Extract the nth octet from a Word32 (0 = least significant).
octet :: Int -> Word32 -> Word8
octet n w = fromIntegral ((w `shiftR` (n * 8)) .&. 0xff)

-- | Render an IPv4 Word32 to dotted-decimal string.
renderIPv4 :: Word32 -> String
renderIPv4 w =
  show (octet 3 w) <> "." <> show (octet 2 w)
    <> "." <> show (octet 1 w) <> "." <> show (octet 0 w)

-- | Render a 16-byte IPv6 ByteString to text string.
renderIPv6 :: BS.ByteString -> String
renderIPv6 bs = show (bytesToIPv6 bs)

-- | Convert 16-byte ByteString to IPv6 address.
bytesToIPv6 :: BS.ByteString -> IPv6
bytesToIPv6 bs =
  let readW32 off =
        (fromIntegral (BS.index bs off) `shiftL` 24)
          .|. (fromIntegral (BS.index bs (off + 1)) `shiftL` 16)
          .|. (fromIntegral (BS.index bs (off + 2)) `shiftL` 8)
          .|. fromIntegral (BS.index bs (off + 3))
  in fromHostAddress6 (readW32 0, readW32 4, readW32 8, readW32 12)

-- | Convert IPv6 address to 16-byte ByteString.
ipv6ToBytes :: IPv6 -> BS.ByteString
ipv6ToBytes ipv6 =
  let (w0, w1, w2, w3) = toHostAddress6 ipv6
      word32ToBytes w =
        [ fromIntegral (w `shiftR` 24)
        , fromIntegral (w `shiftR` 16)
        , fromIntegral (w `shiftR` 8)
        , fromIntegral w
        ]
  in BS.pack $ word32ToBytes w0 <> word32ToBytes w1 <> word32ToBytes w2 <> word32ToBytes w3

