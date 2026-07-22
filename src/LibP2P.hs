-- | libp2p-hs: Haskell implementation of the libp2p networking stack.
--
-- This is the public API facade for the library. Import this module
-- for the common types and functions needed to build a libp2p node:
--
-- @
-- import LibP2P
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
module LibP2P
  ( -- * Identity
    PeerId
  , KeyPair
  , generateKeyPair
  , fromPublicKey
  , peerIdBytes
  , toBase58
  , parsePeerId
  , toCIDv1

    -- * Addressing
  , Multiaddr (..)
  , Protocol (..)
  , splitP2P
  , toText
  , fromText

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

import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (KeyPair)
import LibP2P.Crypto.PeerId (PeerId, fromPublicKey, peerIdBytes, toBase58, parsePeerId, toCIDv1)
import LibP2P.Multiaddr (Multiaddr (..), fromText, splitP2P, toText)
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.MultistreamSelect.Negotiation (ProtocolId, StreamIO (..))
import LibP2P.Protocol.GossipSub.Handler
  ( GossipSubNode (..)
  , gossipJoin
  , gossipLeave
  , gossipPublish
  , newGossipSubNode
  , startGossipSub
  , stopGossipSub
  )
import LibP2P.Protocol.GossipSub.Types (GossipSubParams (..), defaultGossipSubParams)
import LibP2P.Protocol.Identify (registerIdentifyHandlers, requestIdentify)
import LibP2P.Protocol.Ping (PingError (..), PingResult (..), registerPingHandler, sendPing)
import LibP2P.Switch.Dial (dial)
import LibP2P.Switch.Listen (ConnectionGater (..), defaultConnectionGater, switchListen, switchListenAddrs)
import LibP2P.Switch (addTransport, newSwitch, removeStreamHandler, setStreamHandler, switchClose)
import LibP2P.Switch.Types (Connection (..), DialError (..), StreamHandler, Switch)
import LibP2P.Transport.TCP (newTCPTransport)
import LibP2P.Transport (Transport (..))
