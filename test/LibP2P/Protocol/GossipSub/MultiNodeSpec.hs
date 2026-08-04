-- | Multi-router GossipSub tests (#175).
--
-- Two complete routers are wired to each other through their injectable
-- 'gsSendRPC' functions: every RPC a router emits is handed synchronously
-- to the other router's 'handleRPC', so the full wire protocol
-- (subscription announcements, GRAFT/PRUNE, publish, IHAVE/IWANT) runs
-- between two independent router states with no threads, no sleeps and no
-- shared TVars. A per-link gate simulates message loss for the
-- gossip-recovery scenario.
module LibP2P.Protocol.GossipSub.MultiNodeSpec (spec) where

import Test.Hspec

import Control.Concurrent.STM
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (KeyPair (..))
import LibP2P.Crypto.PeerId (PeerId, fromPublicKey)
import LibP2P.Protocol.GossipSub.Heartbeat (heartbeatOnce)
import LibP2P.Protocol.GossipSub.Router
import LibP2P.Protocol.GossipSub.Types

fixedTime :: UTCTime
fixedTime = posixSecondsToUTCTime 1000000

-- | A node: identity, router, delivered payloads, and a link gate that
-- drops this node's outbound RPCs while closed.
data Node = Node
  { nodePid       :: PeerId
  , nodeKey       :: KeyPair
  , nodeRouter    :: GossipSubRouter
  , nodeDelivered :: IORef [ByteString]
  , nodeGate      :: IORef Bool
  }

-- | Build two routers whose sendRPC functions feed each other's handleRPC
-- directly. Mesh-based publish (no flood) so mesh delivery and gossip
-- recovery are distinguishable.
mkLinkedPair :: IO (Node, Node)
mkLinkedPair = do
  kpA <- either (error . ("keygen failed: " <>)) pure =<< generateKeyPair
  kpB <- either (error . ("keygen failed: " <>)) pure =<< generateKeyPair
  let pidA = fromPublicKey (kpPublic kpA)
      pidB = fromPublicKey (kpPublic kpB)
      params = defaultGossipSubParams { paramFloodPublish = False }
  gateA <- newIORef True
  gateB <- newIORef True
  -- Tie the recursive knot: each sender needs the other router
  routerBRef <- newIORef (error "routerB not yet created")
  routerARef <- newIORef (error "routerA not yet created")
  let sendA to rpc = when (to == pidB) $ do
        open <- readIORef gateA
        when open $ do
          rb <- readIORef routerBRef
          handleRPC rb pidA rpc
      sendB to rpc = when (to == pidA) $ do
        open <- readIORef gateB
        when open $ do
          ra <- readIORef routerARef
          handleRPC ra pidB rpc
  routerA <- newRouter params pidA sendA (pure fixedTime)
  routerB <- newRouter params pidB sendB (pure fixedTime)
  writeIORef routerARef routerA
  writeIORef routerBRef routerB
  deliveredA <- newIORef []
  deliveredB <- newIORef []
  atomically $ do
    writeTVar (gsOnMessage routerA) (\_ msg -> modifyIORef' deliveredA (++ [msgData msg]))
    writeTVar (gsOnMessage routerB) (\_ msg -> modifyIORef' deliveredB (++ [msgData msg]))
  -- Both sides register the connection (normally done by the stream handler)
  addPeer routerA pidB GossipSubPeer True fixedTime
  addPeer routerB pidA GossipSubPeer False fixedTime
  pure ( Node pidA kpA routerA deliveredA gateA
       , Node pidB kpB routerB deliveredB gateB
       )

spec :: Spec
spec = do
  describe "GossipSub.MultiNode (two wired routers)" $ do
    it "forms a mesh over the wire: join announcements lead to mutual GRAFT" $ do
      (a, b) <- mkLinkedPair
      join (nodeRouter a) "t"   -- announces to B before B subscribes
      join (nodeRouter b) "t"   -- announces to A, then GRAFTs A
      -- B selected A into its mesh and A accepted the GRAFT
      meshB <- readTVarIO (gsMesh (nodeRouter b))
      Map.findWithDefault Set.empty "t" meshB `shouldBe` Set.singleton (nodePid a)
      meshA <- readTVarIO (gsMesh (nodeRouter a))
      Map.findWithDefault Set.empty "t" meshA `shouldBe` Set.singleton (nodePid b)
      -- Each router learned the other's subscription from the wire
      peersA <- readTVarIO (gsPeers (nodeRouter a))
      fmap psTopics (Map.lookup (nodePid b) peersA) `shouldBe` Just (Set.singleton "t")
      peersB <- readTVarIO (gsPeers (nodeRouter b))
      fmap psTopics (Map.lookup (nodePid a) peersB) `shouldBe` Just (Set.singleton "t")

    it "delivers via the mesh, then recovers a lost message via IHAVE/IWANT" $ do
      (a, b) <- mkLinkedPair
      join (nodeRouter a) "t"
      join (nodeRouter b) "t"

      -- Phase 1: mesh delivery. A publishes; the signed message crosses
      -- the wire, passes B's StrictSign validation and reaches B's app.
      publish (nodeRouter a) "t" (BS.pack [1]) (Just (nodeKey a))
      readIORef (nodeDelivered b) `shouldReturn` [BS.pack [1]]

      -- Phase 2: B drops out of A's mesh (PRUNE over the wire, with a
      -- backoff so A's heartbeat cannot immediately re-graft it).
      handleRPC (nodeRouter a) (nodePid b) emptyRPC
        { rpcControl = Just emptyControlMessage
            { ctrlPrune = [Prune "t" [] (Just 300)] } }
      meshA <- readTVarIO (gsMesh (nodeRouter a))
      Map.findWithDefault Set.empty "t" meshA `shouldBe` Set.empty

      -- Phase 3: the link drops while A publishes; B never sees the message.
      writeIORef (nodeGate a) False
      publish (nodeRouter a) "t" (BS.pack [2]) (Just (nodeKey a))
      readIORef (nodeDelivered b) `shouldReturn` [BS.pack [1]]

      -- Phase 4: link restored; A's heartbeat gossips IHAVE to B (a
      -- non-mesh topic peer), B IWANTs the unseen id, A serves it from
      -- the mcache, and B finally delivers it — all in one synchronous
      -- heartbeat call.
      writeIORef (nodeGate a) True
      heartbeatOnce (nodeRouter a)
      readIORef (nodeDelivered b) `shouldReturn` [BS.pack [1], BS.pack [2]]
      -- B is still outside A's mesh: this was gossip, not mesh repair
      meshA' <- readTVarIO (gsMesh (nodeRouter a))
      Map.findWithDefault Set.empty "t" meshA' `shouldBe` Set.empty
      -- The IWANT promise B recorded against A was fulfilled
      promisesB <- readTVarIO (gsIWantPromises (nodeRouter b))
      promisesB `shouldBe` Map.empty

    it "dedups over the wire: a re-sent message is not delivered twice" $ do
      (a, b) <- mkLinkedPair
      join (nodeRouter a) "t"
      join (nodeRouter b) "t"
      publish (nodeRouter a) "t" (BS.pack [7]) (Just (nodeKey a))
      readIORef (nodeDelivered b) `shouldReturn` [BS.pack [7]]
      -- Replay the exact message B already received (a duplicate forward)
      cacheB <- readTVarIO (gsMessageCache (nodeRouter b))
      case map ceMessage (Map.elems (mcIndex cacheB)) of
        [m] -> handleRPC (nodeRouter b) (nodePid a) emptyRPC { rpcPublish = [m] }
        _   -> expectationFailure "expected exactly one cached message at B"
      readIORef (nodeDelivered b) `shouldReturn` [BS.pack [7]]
      seenB <- readTVarIO (gsSeen (nodeRouter b))
      Map.size seenB `shouldBe` 1
