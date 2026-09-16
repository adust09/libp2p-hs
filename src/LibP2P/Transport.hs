-- | Transport abstraction for libp2p.
--
-- Defines the record-of-functions pattern for transport-agnostic
-- connection management. Stream transports produce a byte stream for the
-- standard security/muxer upgrade; native multiplexed transports such as QUIC
-- provide an already authenticated stream multiplexer.
module LibP2P.Transport
  ( ConnectionEndpoint (..)
  , NativeMuxer (..)
  , RawConnection (..)
  , Listener (..)
  , Transport (..)
  ) where

import LibP2P.Crypto.PeerId (PeerId)
import LibP2P.Multiaddr (Multiaddr)
import LibP2P.MultistreamSelect.Negotiation (ProtocolId, StreamIO)

-- | A transport-native authenticated stream multiplexer.
data NativeMuxer = NativeMuxer
  { nativePeerId :: !PeerId
  , nativeSecurity :: !ProtocolId
  , nativeMuxerProtocol :: !ProtocolId
  , nativeOpenStream :: !(IO StreamIO)
  , nativeAcceptStream :: !(IO StreamIO)
  , nativeClose :: !(IO ())
  }

-- | The I/O endpoint established by a transport.
data ConnectionEndpoint
  = ByteStreamEndpoint !StreamIO
  | NativeMuxerEndpoint !NativeMuxer

-- | A connection established by a transport.
--
-- TCP and relayed connections carry an unencrypted byte stream. QUIC carries
-- a TLS-authenticated native multiplexer and therefore bypasses Noise/Yamux.
data RawConnection = RawConnection
  { rcEndpoint :: !ConnectionEndpoint
  , rcLocalAddr :: !Multiaddr
  , rcRemoteAddr :: !Multiaddr
  , rcClose :: !(IO ())
  }

-- | A listener that accepts inbound connections.
data Listener = Listener
  { listenerAccept :: !(IO RawConnection)
  , listenerClose :: !(IO ())
  , listenerAddr :: !Multiaddr
  }

-- | Transport provides dial/listen capabilities for a specific protocol.
data Transport = Transport
  { transportDial :: !(Multiaddr -> IO RawConnection)
  , transportDialFrom :: !(Maybe Multiaddr -> Multiaddr -> IO RawConnection)
  , transportListen :: !(Multiaddr -> IO Listener)
  , transportCanDial :: !(Multiaddr -> Bool)
  }
