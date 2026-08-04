-- | Multi-router GossipSub tests (#175).
--
-- Complete routers are wired to each other through their injectable
-- 'gsSendRPC' functions: every RPC a router emits is handed synchronously
-- to the target router's 'handleRPC', so the full wire protocol
-- (subscription announcements, GRAFT/PRUNE, publish, IHAVE/IWANT) runs
-- between independent router states with no threads, no sleeps and no
-- shared TVars. Two harnesses share this design:
--
-- * a two-router pair with a per-link gate simulating message loss
--   (mesh formation, gossip recovery, wire-level dedup), and
-- * an N-router network with fully-connected peer discovery and a
--   shared injectable clock (mesh convergence at scale, healing after
--   mass disconnect, network-wide isolation of a bad-signature
--   publisher, end-to-end delivery). Heartbeats are driven explicitly
--   round by round, so every test is deterministic in time.
module LibP2P.Protocol.GossipSub.MultiNodeSpec (spec) where

import Test.Hspec

import Control.Concurrent.STM
import Control.Monad (forM, forM_, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import LibP2P.Crypto.Ed25519 (generateKeyPair)
import LibP2P.Crypto.Key (KeyPair (..))
import LibP2P.Crypto.PeerId (PeerId, fromPublicKey, peerIdBytes)
import LibP2P.Crypto.Protobuf (encodePublicKey)
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

-- N-router network harness (#175 large-scale mesh tests)

-- | A node in an N-router network. 'nnUp' gates the node's links in both
-- directions: a downed node neither emits nor receives RPCs.
data NetNode = NetNode
  { nnPid       :: PeerId
  , nnKey       :: KeyPair
  , nnRouter    :: GossipSubRouter
  , nnDelivered :: IORef [ByteString]
  , nnUp        :: IORef Bool
  }

netTopic :: Topic
netTopic = "t"

netSize :: Int
netSize = 20

-- | Mesh-based publish (no flood) so meshes actually carry the traffic
-- and gossip recovery is distinguishable from flooding.
netParams :: GossipSubParams
netParams = defaultGossipSubParams { paramFloodPublish = False }

-- | Build @n@ routers wired all-to-all: every router's sendRPC delivers
-- synchronously to the target router's handleRPC, every router knows every
-- other one (fully-connected peer discovery), and all routers read one
-- shared injectable clock. The @tweak@ hook adjusts the immutable router
-- record (e.g. scoring parameters) before it is used; per the Types
-- documentation all mutable state lives in TVars, so the tweaked record
-- operates on the same router.
mkNetwork :: Int -> (GossipSubRouter -> GossipSubRouter)
          -> IO ([NetNode], IORef UTCTime)
mkNetwork n tweak = do
  clock <- newIORef fixedTime
  registry <- newIORef (Map.empty :: Map.Map PeerId (GossipSubRouter, IORef Bool))
  nodes <- forM [1 .. n] $ \_ -> do
    kp <- either (error . ("keygen failed: " <>)) pure =<< generateKeyPair
    let pid = fromPublicKey (kpPublic kp)
    up <- newIORef True
    let sendRPC to rpc = do
          selfUp <- readIORef up
          when selfUp $ do
            reg <- readIORef registry
            case Map.lookup to reg of
              Nothing -> pure ()
              Just (target, targetUp) -> do
                open <- readIORef targetUp
                when open $ handleRPC target pid rpc
    router0 <- newRouter netParams pid sendRPC (readIORef clock)
    let router = tweak router0
    delivered <- newIORef []
    atomically $ writeTVar (gsOnMessage router)
      (\_ msg -> modifyIORef' delivered (++ [msgData msg]))
    modifyIORef' registry (Map.insert pid (router, up))
    pure (NetNode pid kp router delivered up)
  -- Fully-connected peer discovery: every router registers every other
  -- (normally done by the stream handler on connection establishment)
  forM_ nodes $ \a -> forM_ nodes $ \b ->
    when (nnPid a /= nnPid b) $
      addPeer (nnRouter a) (nnPid b) GossipSubPeer True fixedTime
  pure (nodes, clock)

-- | The node's mesh for 'netTopic'.
meshOf :: NetNode -> IO (Set.Set PeerId)
meshOf nd = Map.findWithDefault Set.empty netTopic <$> readTVarIO (gsMesh (nnRouter nd))

-- | One synchronous heartbeat on every node, in list order.
heartbeatRound :: [NetNode] -> IO ()
heartbeatRound = mapM_ (heartbeatOnce . nnRouter)

inDegreeBounds :: Int -> Bool
inDegreeBounds d = d >= paramDlo netParams && d <= paramDhi netParams

-- | Drive heartbeat rounds until every node's mesh degree lies within
-- [D_lo, D_hi]; that state is a fixed point (no fill below D_lo, no trim
-- above D_hi, all scores non-negative), so the meshes are converged.
-- Fails the test if the round cap is exceeded.
convergeMeshes :: Int -> [NetNode] -> IO ()
convergeMeshes cap nodes = go (1 :: Int)
  where
    go k = do
      heartbeatRound nodes
      degrees <- mapM (fmap Set.size . meshOf) nodes
      unless (all inDegreeBounds degrees) $
        if k >= cap
          then expectationFailure $
            "meshes did not converge into [D_lo, D_hi] within "
              <> show cap <> " heartbeat rounds; degrees: " <> show degrees
          else go (k + 1)

-- | Every mesh edge must be mutual: A in B's mesh iff B in A's mesh.
assertSymmetricMeshes :: [NetNode] -> Expectation
assertSymmetricMeshes nodes = do
  meshes <- Map.fromList <$> mapM (\nd -> (,) (nnPid nd) <$> meshOf nd) nodes
  let asymmetric =
        [ (a, b)
        | (a, mesh) <- Map.toList meshes
        , b <- Set.toList mesh
        , not (Set.member a (Map.findWithDefault Set.empty b meshes))
        ]
  asymmetric `shouldBe` []

-- | Drive heartbeat rounds (gossip recovery) until every node has
-- delivered the payload, failing the test beyond the round cap. Returns
-- the number of extra rounds that were needed.
deliverWithin :: Int -> [NetNode] -> ByteString -> IO Int
deliverWithin cap nodes payload = go 0
  where
    go k = do
      done <- and <$> mapM (fmap (elem payload) . readIORef . nnDelivered) nodes
      if done
        then pure k
        else if k >= cap
          then do
            expectationFailure $
              "payload not delivered everywhere within "
                <> show cap <> " heartbeat rounds"
            pure k
          else heartbeatRound nodes >> go (k + 1)

-- | Scoring parameters for the bad-publisher test: only P4 (invalid
-- message deliveries) carries weight, so honest peers score exactly 0 and
-- the offender's score is driven by its forged messages alone.
p4OnlyScoreParams :: PeerScoreParams
p4OnlyScoreParams = defaultPeerScoreParams
  { pspTopicParams = Map.singleton netTopic defaultTopicScoreParams
      { tspTimeInMeshWeight             = 0
      , tspFirstMessageDeliveriesWeight = 0
      , tspMeshMessageDeliveriesWeight  = 0
      , tspMeshFailurePenaltyWeight     = 0
      }
  }

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

  describe "GossipSub.MultiNode (20-router mesh at scale)" $ do
    it "converges all mesh degrees into [D_lo, D_hi] with symmetric meshes" $ do
      (nodes, _clock) <- mkNetwork netSize id
      mapM_ (\nd -> join (nnRouter nd) netTopic) nodes
      convergeMeshes 30 nodes
      degrees <- mapM (fmap Set.size . meshOf) nodes
      degrees `shouldSatisfy` all inDegreeBounds
      assertSymmetricMeshes nodes

    it "heals after disconnecting 8 of 20 routers and still delivers everywhere" $ do
      (nodes, clock) <- mkNetwork netSize id
      mapM_ (\nd -> join (nnRouter nd) netTopic) nodes
      convergeMeshes 30 nodes
      -- Take a batch of 8 offline: gate their links and remove them from
      -- every remaining router (what connection teardown would do)
      let (dead, alive) = splitAt 8 nodes
      forM_ dead $ \d -> writeIORef (nnUp d) False
      forM_ alive $ \a -> forM_ dead $ \d -> removePeer (nnRouter a) (nnPid d)
      -- Let the prune backoffs accumulated during convergence expire so
      -- the survivors may re-form any edge (deterministic clock step)
      modifyIORef' clock (addUTCTime 61)
      -- The remaining routers re-converge into [D_lo, D_hi]
      convergeMeshes 30 alive
      degrees <- mapM (fmap Set.size . meshOf) alive
      degrees `shouldSatisfy` all inDegreeBounds
      assertSymmetricMeshes alive
      -- No survivor still holds a dead peer in its mesh
      let deadPids = Set.fromList (map nnPid dead)
      forM_ alive $ \a -> do
        m <- meshOf a
        Set.intersection m deadPids `shouldBe` Set.empty
      -- A publish still reaches every remaining router, exactly once
      case alive of
        [] -> expectationFailure "no surviving routers"
        publisher : _ -> do
          let payload = BS.pack [42]
          publish (nnRouter publisher) netTopic payload (Just (nnKey publisher))
          _ <- deliverWithin 10 alive payload
          forM_ alive $ \a -> do
            d <- readIORef (nnDelivered a)
            length (filter (== payload) d) `shouldBe` 1

    it "isolates a publisher of invalidly signed messages network-wide" $ do
      (nodes, _clock) <- mkNetwork netSize
        (\r -> r { gsScoreParams = p4OnlyScoreParams })
      mapM_ (\nd -> join (nnRouter nd) netTopic) nodes
      convergeMeshes 30 nodes
      let offender = last nodes
          honest   = init nodes
          forgedPayload = BS.pack [66]
          -- Correctly shaped StrictSign message whose signature is garbage
          forged s = PubSubMessage
            { msgFrom      = Just (peerIdBytes (nnPid offender))
            , msgData      = forgedPayload
            , msgSeqNo     = Just (BS.pack [0, 0, 0, 0, 0, 0, 0, s])
            , msgTopic     = netTopic
            , msgSignature = Just (BS.replicate 64 0x5A)
            , msgKey       = Just (encodePublicKey (kpPublic (nnKey offender)))
            }
      victims0 <- meshOf offender
      Set.size victims0 `shouldSatisfy` (>= paramDlo netParams)
      -- The offender floods three forged messages to every honest router
      forM_ [1 .. 3] $ \s ->
        forM_ honest $ \h ->
          gsSendRPC (nnRouter offender) (nnPid h)
            emptyRPC { rpcPublish = [forged s] }
      -- No honest router delivered any of them
      forM_ honest $ \h ->
        readIORef (nnDelivered h) `shouldReturn` []
      -- The offender's score went negative at every router it offended
      forM_ honest $ \h -> do
        s <- peerScore (nnRouter h) (nnPid offender)
        s `shouldSatisfy` (< 0)
      -- One honest heartbeat: every honest router prunes the offender
      -- from its mesh (negative score), and the PRUNEs empty the
      -- offender's own mesh over the wire
      heartbeatRound honest
      forM_ honest $ \h -> do
        m <- meshOf h
        Set.member (nnPid offender) m `shouldBe` False
      meshOf offender `shouldReturn` Set.empty
      -- The offender's heartbeat tries to re-fill its mesh, but every
      -- honest router rejects the GRAFT: its score is negative everywhere
      heartbeatOnce (nnRouter offender)
      forM_ honest $ \h -> do
        m <- meshOf h
        Set.member (nnPid offender) m `shouldBe` False
      -- Honest traffic still flows across the healed honest meshes
      convergeMeshes 30 honest
      case honest of
        [] -> expectationFailure "no honest routers"
        publisher : _ -> do
          let payload = BS.pack [77]
          publish (nnRouter publisher) netTopic payload (Just (nnKey publisher))
          _ <- deliverWithin 10 honest payload
          forM_ honest $ \h -> do
            d <- readIORef (nnDelivered h)
            length (filter (== payload) d) `shouldBe` 1
      -- The forged payload never reached any application, anywhere
      forM_ nodes $ \nd -> do
        d <- readIORef (nnDelivered nd)
        d `shouldSatisfy` notElem forgedPayload

    it "delivers a publish from one corner to all 19 others exactly once" $ do
      (nodes, _clock) <- mkNetwork netSize id
      mapM_ (\nd -> join (nnRouter nd) netTopic) nodes
      convergeMeshes 30 nodes
      case nodes of
        [] -> expectationFailure "no routers"
        publisher : rest -> do
          let payload = BS.pack [9]
          publish (nnRouter publisher) netTopic payload (Just (nnKey publisher))
          -- Mesh forwarding is synchronous; any straggler outside the
          -- connected component is recovered by IHAVE/IWANT gossip within
          -- a bounded number of heartbeat rounds
          rounds <- deliverWithin 10 nodes payload
          rounds `shouldSatisfy` (<= 10)
          forM_ rest $ \nd -> do
            d <- readIORef (nnDelivered nd)
            length (filter (== payload) d) `shouldBe` 1
          -- The publisher's own local delivery is also exactly once
          dPub <- readIORef (nnDelivered publisher)
          length (filter (== payload) dPub) `shouldBe` 1
