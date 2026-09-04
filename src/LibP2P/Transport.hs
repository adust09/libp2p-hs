-- | Transport abstraction for libp2p.
--
-- Defines the record-of-functions pattern for transport-agnostic
-- connection management. Each transport (TCP, QUIC, etc.) provides
-- a Transport value with dial/listen/canDial implementations.
module LibP2P.Transport
  ( RawConnection (..)
  , Listener (..)
  , Transport (..)
  ) where

import LibP2P.Multiaddr (Multiaddr)
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..))

-- | A raw (unencrypted, un-muxed) connection from a transport.
-- Provides byte-level I/O via StreamIO, plus address info and cleanup.
data RawConnection = RawConnection
  { rcStreamIO :: !StreamIO -- ^ Byte-level read/write over the connection
  , rcLocalAddr :: !Multiaddr -- ^ Local multiaddr (e.g. /ip4/127.0.0.1/tcp/12345)
  , rcRemoteAddr :: !Multiaddr -- ^ Remote multiaddr
  , rcClose :: !(IO ()) -- ^ Close the underlying transport connection
  }

-- | A listener that accepts inbound connections.
data Listener = Listener
  { listenerAccept :: !(IO RawConnection) -- ^ Block until a connection arrives
  , listenerClose :: !(IO ()) -- ^ Stop listening and close the socket
  , listenerAddr :: !Multiaddr -- ^ Actual bound address (port 0 resolves here)
  }

-- | Transport provides dial/listen capabilities for a specific protocol.
data Transport = Transport
  { transportDial :: !(Multiaddr -> IO RawConnection)
    -- ^ Dial a remote peer from an ephemeral local port
  , transportDialFrom :: !(Maybe Multiaddr -> Multiaddr -> IO RawConnection)
    -- ^ Dial a remote peer, optionally binding the local socket first.
    -- Hole punching needs this: the outgoing SYN must leave from the
    -- listen port so the NAT mapping matches the address advertised in
    -- DCUtR CONNECT (specs/relay/DCUtR simultaneous connect).
  , transportListen :: !(Multiaddr -> IO Listener) -- ^ Listen for inbound connections
  , transportCanDial :: !(Multiaddr -> Bool) -- ^ Check if this transport can handle the address
  }
