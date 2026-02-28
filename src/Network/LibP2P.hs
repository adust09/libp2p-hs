-- | libp2p-hs: Haskell implementation of the libp2p networking stack.
--
-- This is the public API facade for the library. Import this module
-- for the common types and functions needed to build a libp2p node:
--
-- @
-- import Network.LibP2P
--
-- main :: IO ()
-- main = do
--   (pid, kp) <- generateAndPeerId
--   sw <- newSwitch pid kp
--   tcp <- newTCPTransport
--   addTransport sw tcp
--   registerIdentifyHandlers sw
--   registerPingHandler sw
--   addrs <- switchListen sw defaultConnectionGater [fromText "/ip4/127.0.0.1/tcp/0"]
--   print addrs
--   -- ... dial other peers, etc.
--   switchClose sw
-- @
module Network.LibP2P
  ( -- * Identity
    PeerId
  , KeyPair
  , generateKeyPair
  , fromPublicKey

    -- * Addressing
  , Multiaddr (..)
  , Protocol (..)

    -- * Switch (central coordinator)
  , Switch
  , newSwitch
  , addTransport
  , switchListen
  , switchListenAddrs
  , switchClose
  , dial
  , DialError (..)
  , setStreamHandler
  , removeStreamHandler
  , StreamIO (..)
  , StreamHandler
  , ProtocolId
  , Connection (..)

    -- * Transport
  , Transport (..)
  , newTCPTransport

    -- * Connection gating
  , ConnectionGater (..)
  , defaultConnectionGater

    -- * Identify protocol
  , registerIdentifyHandlers
  , requestIdentify

    -- * Ping protocol
  , registerPingHandler
  , sendPing
  , PingResult (..)
  , PingError (..)

    -- * GossipSub
  , GossipSubNode (..)
  , GossipSubParams (..)
  , defaultGossipSubParams
  , newGossipSubNode
  , startGossipSub
  , stopGossipSub
  , gossipJoin
  , gossipLeave
  , gossipPublish
  ) where

import Network.LibP2P.Crypto.Ed25519 (generateKeyPair)
import Network.LibP2P.Crypto.Key (KeyPair)
import Network.LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr (..))
import Network.LibP2P.Multiaddr.Protocol (Protocol (..))
import Network.LibP2P.MultistreamSelect.Negotiation (ProtocolId, StreamIO (..))
import Network.LibP2P.Protocol.GossipSub.Handler
  ( GossipSubNode (..)
  , gossipJoin
  , gossipLeave
  , gossipPublish
  , newGossipSubNode
  , startGossipSub
  , stopGossipSub
  )
import Network.LibP2P.Protocol.GossipSub.Types (GossipSubParams (..), defaultGossipSubParams)
import Network.LibP2P.Protocol.Identify.Identify (registerIdentifyHandlers, requestIdentify)
import Network.LibP2P.Protocol.Ping.Ping (PingError (..), PingResult (..), registerPingHandler, sendPing)
import Network.LibP2P.Switch.Dial (dial)
import Network.LibP2P.Switch.Listen (ConnectionGater (..), defaultConnectionGater, switchListen, switchListenAddrs)
import Network.LibP2P.Switch.Switch (addTransport, newSwitch, removeStreamHandler, setStreamHandler, switchClose)
import Network.LibP2P.Switch.Types (Connection (..), DialError (..), StreamHandler, Switch)
import Network.LibP2P.Transport.TCP (newTCPTransport)
import Network.LibP2P.Transport.Transport (Transport (..))
