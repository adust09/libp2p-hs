module Test.Network.LibP2P.Protocol.GossipSub.HeartbeatSpec (spec) where

import Test.Hspec

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.IORef
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.LibP2P.Crypto.PeerId (PeerId (..))
import Network.LibP2P.Protocol.GossipSub.Types
import Network.LibP2P.Protocol.GossipSub.Router (newRouter, addPeer, peerScore)
import Network.LibP2P.Protocol.GossipSub.MessageCache (newMessageCache, cachePut, cacheGetGossipIds)
import Network.LibP2P.Protocol.GossipSub.Heartbeat

-- Test helpers

mkPeerId :: Int -> PeerId
mkPeerId n = PeerId (BS.pack [fromIntegral n])

fixedTime :: UTCTime
fixedTime = posixSecondsToUTCTime 1000000

newSendLog :: IO (IORef [(PeerId, RPC)], PeerId -> RPC -> IO ())
newSendLog = do
  ref <- newIORef []
  let sendFn pid rpc = modifyIORef' ref (++ [(pid, rpc)])
  pure (ref, sendFn)

-- | Create a test router with adjustable time and topic score params.
mkHeartbeatRouter :: PeerId -> UTCTime -> IO (GossipSubRouter, IORef [(PeerId, RPC)], IORef UTCTime)
mkHeartbeatRouter localPid t = do
  (logRef, sendFn) <- newSendLog
  timeRef <- newIORef t
  let getTime = readIORef timeRef
  router <- newRouter defaultGossipSubParams localPid sendFn getTime
  pure (router, logRef, timeRef)

-- | Add peer that's subscribed and in mesh for a topic.
addMeshPeer :: GossipSubRouter -> PeerId -> Topic -> UTCTime -> IO ()
addMeshPeer router pid topic now = do
  addPeer router pid GossipSubPeer False now
  atomically $ do
    modifyTVar' (gsPeers router) $
      Map.adjust (\ps -> ps { psTopics = Set.singleton topic }) pid
    modifyTVar' (gsMesh router) $
      Map.insertWith Set.union topic (Set.singleton pid)

-- | Add peer that's subscribed but NOT in mesh.
addSubscribedPeer :: GossipSubRouter -> PeerId -> Topic -> UTCTime -> IO ()
addSubscribedPeer router pid topic now = do
  addPeer router pid GossipSubPeer False now
  atomically $ modifyTVar' (gsPeers router) $
    Map.adjust (\ps -> ps { psTopics = Set.singleton topic }) pid

localPid :: PeerId
localPid = mkPeerId 0

spec :: Spec
spec = do
  describe "GossipSub.Heartbeat" $ do

    describe "Mesh maintenance" $ do
      it "prunes negative-score peers from mesh" $ do
        (router, logRef, _) <- mkHeartbeatRouter localPid fixedTime
        let routerWithParams = router
              { gsScoreParams = defaultPeerScoreParams
                  { pspTopicParams = Map.singleton "t" defaultTopicScoreParams }
              }
        -- Add a peer with negative score (high invalid message count)
        let pid = mkPeerId 1
        addMeshPeer routerWithParams pid "t" fixedTime
        atomically $ modifyTVar' (gsPeers routerWithParams) $
          Map.adjust (\ps -> ps
            { psTopicState = Map.singleton "t"
                (defaultTopicPeerState { tpsInvalidMessages = 10 })
            }) pid
        -- Run heartbeat
        heartbeatOnce routerWithParams
        -- Peer should be removed from mesh
        mesh <- readTVarIO (gsMesh routerWithParams)
        Set.member pid (Map.findWithDefault Set.empty "t" mesh) `shouldBe` False
        -- Should have sent PRUNE
        sent <- readIORef logRef
        let pruneMsgs = filter (\(p, rpc) ->
              p == pid && case rpcControl rpc of
                Just ctrl -> not (null (ctrlPrune ctrl))
                Nothing -> False) sent
        length pruneMsgs `shouldSatisfy` (>= 1)

      it "GRAFTs when mesh is undersubscribed (< D_lo)" $ do
        (router, logRef, _) <- mkHeartbeatRouter localPid fixedTime
        -- D=6, D_lo=4: add 2 mesh peers + 4 more subscribed but not in mesh
        let meshPeers = map mkPeerId [1, 2]
            otherPeers = map mkPeerId [3, 4, 5, 6]
        mapM_ (\pid -> addMeshPeer router pid "t" fixedTime) meshPeers
        mapM_ (\pid -> addSubscribedPeer router pid "t" fixedTime) otherPeers
        -- Make sure mesh entry exists
        atomically $ modifyTVar' (gsMesh router) $
          Map.insertWith Set.union "t" Set.empty
        heartbeatOnce router
        -- Mesh should have grown toward D=6
        mesh <- readTVarIO (gsMesh router)
        let meshSize = Set.size (Map.findWithDefault Set.empty "t" mesh)
        meshSize `shouldSatisfy` (>= 4)  -- at least D_lo
        -- Should have sent GRAFT messages
        sent <- readIORef logRef
        let graftMsgs = filter (\(_, rpc) ->
              case rpcControl rpc of
                Just ctrl -> not (null (ctrlGraft ctrl))
                Nothing -> False) sent
        length graftMsgs `shouldSatisfy` (>= 1)

      it "skips peers in backoff during undersubscribed fill" $ do
        (router, logRef, _) <- mkHeartbeatRouter localPid fixedTime
        let meshPeer = mkPeerId 1
            backoffPeer = mkPeerId 2
            availPeer = mkPeerId 3
        addMeshPeer router meshPeer "t" fixedTime
        addSubscribedPeer router backoffPeer "t" fixedTime
        addSubscribedPeer router availPeer "t" fixedTime
        -- Put backoffPeer in backoff
        atomically $ modifyTVar' (gsBackoff router) $
          Map.insert (backoffPeer, "t") (addUTCTime 60 fixedTime)
        heartbeatOnce router
        -- backoffPeer should NOT be in mesh
        mesh <- readTVarIO (gsMesh router)
        let meshPeers = Map.findWithDefault Set.empty "t" mesh
        Set.member backoffPeer meshPeers `shouldBe` False

      it "PRUNEs when mesh is oversubscribed (> D_hi)" $ do
        (router, logRef, _) <- mkHeartbeatRouter localPid fixedTime
        -- D_hi=12: add 15 mesh peers
        let pids = map mkPeerId [1..15]
        mapM_ (\pid -> addMeshPeer router pid "t" fixedTime) pids
        heartbeatOnce router
        -- Mesh should be trimmed to D=6
        mesh <- readTVarIO (gsMesh router)
        let meshSize = Set.size (Map.findWithDefault Set.empty "t" mesh)
        meshSize `shouldBe` 6  -- trimmed to D
        -- Should have sent PRUNE to excess peers
        sent <- readIORef logRef
        let pruneMsgs = filter (\(_, rpc) ->
              case rpcControl rpc of
                Just ctrl -> not (null (ctrlPrune ctrl))
                Nothing -> False) sent
        length pruneMsgs `shouldBe` 9  -- 15 - 6 = 9

    describe "Fanout maintenance" $ do
      it "expires old fanout entries" $ do
        (router, _, timeRef) <- mkHeartbeatRouter localPid fixedTime
        let pid = mkPeerId 1
        addSubscribedPeer router pid "t" fixedTime
        -- Add fanout entry with old timestamp
        atomically $ do
          modifyTVar' (gsFanout router) $
            Map.insert "t" (Set.singleton pid)
          modifyTVar' (gsFanoutPub router) $
            Map.insert "t" fixedTime
        -- Advance time past fanout_ttl (60s)
        writeIORef timeRef (addUTCTime 61 fixedTime)
        heartbeatOnce router
        -- Fanout should be expired
        fanout <- readTVarIO (gsFanout router)
        Map.member "t" fanout `shouldBe` False

      it "fills fanout when < D" $ do
        (router, _, _) <- mkHeartbeatRouter localPid fixedTime
        let pids = map mkPeerId [1..8]
        mapM_ (\pid -> addSubscribedPeer router pid "t" fixedTime) pids
        -- Add fanout entry with 1 peer (recent, so won't expire)
        atomically $ do
          modifyTVar' (gsFanout router) $
            Map.insert "t" (Set.singleton (mkPeerId 1))
          modifyTVar' (gsFanoutPub router) $
            Map.insert "t" fixedTime
        heartbeatOnce router
        -- Fanout should be filled toward D=6
        fanout <- readTVarIO (gsFanout router)
        let fanoutSize = Set.size (Map.findWithDefault Set.empty "t" fanout)
        fanoutSize `shouldSatisfy` (>= 2)  -- at least grew

    describe "Gossip emission" $ do
      it "sends IHAVE to non-mesh peers" $ do
        (router, logRef, _) <- mkHeartbeatRouter localPid fixedTime
        -- Need D=6 mesh peers so mesh maintenance doesn't steal our non-mesh peer
        let meshPeers = map mkPeerId [1..6]
            nonMeshPeer = mkPeerId 10
        mapM_ (\pid -> addMeshPeer router pid "t" fixedTime) meshPeers
        addSubscribedPeer router nonMeshPeer "t" fixedTime
        -- Put a message in the cache
        let mid = BS.pack [42]
            msg = PubSubMessage (Just (BS.pack [1])) (BS.pack [1]) (Just mid) "t" Nothing Nothing
        atomically $ modifyTVar' (gsMessageCache router) $
          cachePut mid msg
        heartbeatOnce router
        -- nonMeshPeer should receive IHAVE
        sent <- readIORef logRef
        let ihaveMsgs = filter (\(pid, rpc) ->
              pid == nonMeshPeer && case rpcControl rpc of
                Just ctrl -> not (null (ctrlIHave ctrl))
                Nothing -> False) sent
        length ihaveMsgs `shouldSatisfy` (>= 1)

      it "does not send IHAVE to mesh peers" $ do
        (router, logRef, _) <- mkHeartbeatRouter localPid fixedTime
        -- Need D=6 mesh peers so mesh maintenance doesn't modify mesh
        let meshPeers = map mkPeerId [1..6]
        mapM_ (\pid -> addMeshPeer router pid "t" fixedTime) meshPeers
        let mid = BS.pack [42]
            msg = PubSubMessage (Just (BS.pack [1])) (BS.pack [1]) (Just mid) "t" Nothing Nothing
        atomically $ modifyTVar' (gsMessageCache router) $
          cachePut mid msg
        heartbeatOnce router
        -- Mesh peers should NOT receive IHAVE (only GRAFTs from maintenance)
        sent <- readIORef logRef
        let ihaveMsgs = filter (\(pid, rpc) ->
              Set.member pid (Set.fromList meshPeers) &&
              case rpcControl rpc of
                Just ctrl -> not (null (ctrlIHave ctrl))
                Nothing -> False) sent
        length ihaveMsgs `shouldBe` 0

      it "rotates message cache after gossip" $ do
        (router, _, _) <- mkHeartbeatRouter localPid fixedTime
        let mid = BS.pack [42]
            msg = PubSubMessage (Just (BS.pack [1])) (BS.pack [1]) (Just mid) "t" Nothing Nothing
        atomically $ modifyTVar' (gsMessageCache router) $
          cachePut mid msg
        -- After mcLen=5 heartbeats, message should be evicted
        mapM_ (\_ -> heartbeatOnce router) [1..5 :: Int]
        cache <- readTVarIO (gsMessageCache router)
        cacheGetGossipIds "t" cache `shouldBe` []

    describe "Score decay" $ do
      it "decays P2 counter" $ do
        (router, _, _) <- mkHeartbeatRouter localPid fixedTime
        let routerWithParams = router
              { gsScoreParams = defaultPeerScoreParams
                  { pspTopicParams = Map.singleton "t"
                      (defaultTopicScoreParams { tspFirstMessageDeliveriesDecay = 0.5 })
                  }
              }
        let pid = mkPeerId 1
        addPeer routerWithParams pid GossipSubPeer False fixedTime
        atomically $ modifyTVar' (gsPeers routerWithParams) $
          Map.adjust (\ps -> ps
            { psTopicState = Map.singleton "t"
                (defaultTopicPeerState { tpsFirstMessageDeliveries = 10 })
            }) pid
        heartbeatOnce routerWithParams
        peers <- readTVarIO (gsPeers routerWithParams)
        case Map.lookup pid peers of
          Just ps -> case Map.lookup "t" (psTopicState ps) of
            Just tps -> tpsFirstMessageDeliveries tps `shouldBe` 5
            Nothing -> expectationFailure "topic state not found"
          Nothing -> expectationFailure "peer not found"

      it "decays P7 counter" $ do
        (router, _, _) <- mkHeartbeatRouter localPid fixedTime
        let routerWithParams = router
              { gsScoreParams = defaultPeerScoreParams
                  { pspBehaviorPenaltyDecay = 0.5 }
              }
        let pid = mkPeerId 1
        addPeer routerWithParams pid GossipSubPeer False fixedTime
        atomically $ modifyTVar' (gsPeers routerWithParams) $
          Map.adjust (\ps -> ps { psBehaviorPenalty = 10 }) pid
        heartbeatOnce routerWithParams
        peers <- readTVarIO (gsPeers routerWithParams)
        case Map.lookup pid peers of
          Just ps -> psBehaviorPenalty ps `shouldBe` 5
          Nothing -> expectationFailure "peer not found"

    describe "Seen cache cleanup" $ do
      it "cleans expired entries from seen cache" $ do
        (router, _, timeRef) <- mkHeartbeatRouter localPid fixedTime
        -- Add seen entry at fixedTime
        atomically $ modifyTVar' (gsSeen router) $
          Map.insert (BS.pack [1]) fixedTime
        -- Advance time past SeenTTL (120s)
        writeIORef timeRef (addUTCTime 121 fixedTime)
        heartbeatOnce router
        seen <- readTVarIO (gsSeen router)
        Map.member (BS.pack [1]) seen `shouldBe` False

      it "preserves unexpired entries" $ do
        (router, _, timeRef) <- mkHeartbeatRouter localPid fixedTime
        atomically $ modifyTVar' (gsSeen router) $
          Map.insert (BS.pack [1]) fixedTime
        -- Only 10s later — still within TTL
        writeIORef timeRef (addUTCTime 10 fixedTime)
        heartbeatOnce router
        seen <- readTVarIO (gsSeen router)
        Map.member (BS.pack [1]) seen `shouldBe` True

    describe "Heartbeat counter" $ do
      it "increments on each heartbeat" $ do
        (router, _, _) <- mkHeartbeatRouter localPid fixedTime
        count0 <- readTVarIO (gsHeartbeatCount router)
        count0 `shouldBe` 0
        heartbeatOnce router
        count1 <- readTVarIO (gsHeartbeatCount router)
        count1 `shouldBe` 1
        heartbeatOnce router
        count2 <- readTVarIO (gsHeartbeatCount router)
        count2 `shouldBe` 2
