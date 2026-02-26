-- | Yamux session management: create, openStream, acceptStream, ping, goaway.
--
-- Implements the session-level Yamux protocol per HashiCorp yamux spec.md.
-- The session manages a collection of multiplexed streams over a single
-- underlying transport connection.
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

import Data.ByteString (ByteString)
import Network.LibP2P.Mux.Yamux.Frame (GoAwayCode)
import Network.LibP2P.Mux.Yamux.Types
  ( SessionRole
  , YamuxError
  , YamuxSession
  , YamuxStream
  )

-- | Create a new Yamux session over a transport connection.
newSession :: SessionRole -> (ByteString -> IO ()) -> (Int -> IO ByteString) -> IO YamuxSession
newSession = error "TODO: newSession"

-- | Gracefully close the session by sending GoAway Normal.
closeSession :: YamuxSession -> IO ()
closeSession = error "TODO: closeSession"

-- | Open a new outbound stream. Allocates the next stream ID and sends SYN.
openStream :: YamuxSession -> IO (Either YamuxError YamuxStream)
openStream = error "TODO: openStream"

-- | Accept an inbound stream. Blocks until a remote SYN arrives.
acceptStream :: YamuxSession -> IO (Either YamuxError YamuxStream)
acceptStream = error "TODO: acceptStream"

-- | Send a Ping and wait for the ACK response.
ping :: YamuxSession -> IO (Either YamuxError ())
ping = error "TODO: ping"

-- | Send a GoAway frame with the specified error code.
sendGoAway :: YamuxSession -> GoAwayCode -> IO ()
sendGoAway = error "TODO: sendGoAway"

-- | Receive loop: reads 12-byte headers from transport and dispatches frames.
recvLoop :: YamuxSession -> IO ()
recvLoop = error "TODO: recvLoop"

-- | Send loop: dequeues frames from ysSendCh and writes to transport.
sendLoop :: YamuxSession -> IO ()
sendLoop = error "TODO: sendLoop"
