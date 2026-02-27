module Test.Network.LibP2P.Protocol.GossipSub.RouterSpec (spec) where

import Test.Hspec

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.IORef
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.LibP2P.Crypto.PeerId (PeerId (..))
import Network.LibP2P.Protocol.GossipSub.Types
import Network.LibP2P.Protocol.GossipSub.Router

-- Test helpers

-- | Create a PeerId from a byte.
mkPeerId :: Int -> PeerId
mkPeerId n = PeerId (BS.pack [fromIntegral n])

-- | Fixed time for deterministic tests.
fixedTime :: UTCTime
fixedTime = posixSecondsToUTCTime 1000000

-- | A time source that always returns fixedTime.
fixedTimeSource :: IO UTCTime
fixedTimeSource = pure fixedTime

-- | Adjustable time source for backoff tests.
newTimeRef :: UTCTime -> IO (IORef UTCTime, IO UTCTime)
newTimeRef t = do
  ref <- newIORef t
  pure (ref, readIORef ref)

-- | Record all sent RPCs for verification.
newSendLog :: IO (IORef [(PeerId, RPC)], PeerId -> RPC -> IO ())
newSendLog = do
  ref <- newIORef []
  let sendFn pid rpc = modifyIORef' ref (++ [(pid, rpc)])
  pure (ref, sendFn)

-- | Create a test router with send logging.
mkTestRouter :: PeerId -> IO (GossipSubRouter, IORef [(PeerId, RPC)])
mkTestRouter localPid = do
  (logRef, sendFn) <- newSendLog
  router <- newRouter defaultGossipSubParams localPid sendFn fixedTimeSource
  pure (router, logRef)

-- | Create a test router with adjustable time.
mkTestRouterWithTime :: PeerId -> UTCTime -> IO (GossipSubRouter, IORef [(PeerId, RPC)], IORef UTCTime)
mkTestRouterWithTime localPid t = do
  (logRef, sendFn) <- newSendLog
  (timeRef, getTime) <- newTimeRef t
  router <- newRouter defaultGossipSubParams localPid sendFn getTime
  pure (router, logRef, timeRef)

localPid :: PeerId
localPid = mkPeerId 0

-- | Add a peer that is subscribed to a topic.
addSubscribedPeer :: GossipSubRouter -> PeerId -> Topic -> IO ()
addSubscribedPeer router pid topic = do
  addPeer router pid GossipSubPeer False fixedTime
  atomically $ modifyTVar' (gsPeers router) $
    Map.adjust (\ps -> ps { psTopics = Set.singleton topic }) pid

spec :: Spec
spec = do
  describe "GossipSub.Router" $ do
    describe "newRouter" $ do
      it "creates router with empty state" $ do
        (router, _) <- mkTestRouter localPid
        mesh <- readTVarIO (gsMesh router)
        fanout <- readTVarIO (gsFanout router)
        peers <- readTVarIO (gsPeers router)
        seen <- readTVarIO (gsSeen router)
        mesh `shouldBe` Map.empty
        fanout `shouldBe` Map.empty
        peers `shouldBe` Map.empty
        seen `shouldBe` Map.empty

    describe "addPeer / removePeer" $ do
      it "addPeer registers peer with empty topics" $ do
        (router, _) <- mkTestRouter localPid
        let pid = mkPeerId 1
        addPeer router pid GossipSubPeer True fixedTime
        peers <- readTVarIO (gsPeers router)
        Map.member pid peers `shouldBe` True
        case Map.lookup pid peers of
          Just ps -> do
            psProtocol ps `shouldBe` GossipSubPeer
            psIsOutbound ps `shouldBe` True
            psTopics ps `shouldBe` Set.empty
          Nothing -> expectationFailure "peer not found"

      it "removePeer cleans up mesh and fanout" $ do
        (router, _) <- mkTestRouter localPid
        let pid = mkPeerId 1
        addPeer router pid GossipSubPeer False fixedTime
        -- Manually add to mesh
        atomically $ modifyTVar' (gsMesh router) $
          Map.insert "topic1" (Set.singleton pid)
        removePeer router pid
        peers <- readTVarIO (gsPeers router)
        Map.member pid peers `shouldBe` False
        mesh <- readTVarIO (gsMesh router)
        let topicPeers = Map.findWithDefault Set.empty "topic1" mesh
        Set.member pid topicPeers `shouldBe` False

    describe "join" $ do
      it "announces subscription to all peers" $ do
        (router, logRef) <- mkTestRouter localPid
        let peerA = mkPeerId 1
            peerB = mkPeerId 2
        addPeer router peerA GossipSubPeer False fixedTime
        addPeer router peerB GossipSubPeer False fixedTime
        join router "blocks"
        sent <- readIORef logRef
        -- Should have sent subscription announcement to both peers
        let subMsgs = filter (\(_, rpc) ->
              any (\s -> subSubscribe s && subTopicId s == "blocks")
                  (rpcSubscriptions rpc)) sent
        length subMsgs `shouldSatisfy` (>= 2)

      it "creates mesh with up to D peers" $ do
        (router, _) <- mkTestRouter localPid
        -- Add 8 peers subscribed to "blocks"
        let peerIds = map mkPeerId [1..8]
        mapM_ (\pid -> addSubscribedPeer router pid "blocks") peerIds
        join router "blocks"
        mesh <- readTVarIO (gsMesh router)
        let meshPeers = Map.findWithDefault Set.empty "blocks" mesh
        -- Should have D=6 peers in mesh
        Set.size meshPeers `shouldBe` 6

      it "sends GRAFT to new mesh peers" $ do
        (router, logRef) <- mkTestRouter localPid
        let peerIds = map mkPeerId [1..8]
        mapM_ (\pid -> addSubscribedPeer router pid "blocks") peerIds
        join router "blocks"
        sent <- readIORef logRef
        -- Check that GRAFT messages were sent
        let graftMsgs = filter (\(_, rpc) ->
              case rpcControl rpc of
                Just ctrl -> any (\(Graft t) -> t == "blocks") (ctrlGraft ctrl)
                Nothing -> False) sent
        length graftMsgs `shouldBe` 6

      it "transitions fanout peers to mesh" $ do
        (router, _) <- mkTestRouter localPid
        let peerA = mkPeerId 1
            peerB = mkPeerId 2
        addSubscribedPeer router peerA "topic1"
        addSubscribedPeer router peerB "topic1"
        -- Pre-populate fanout
        atomically $ modifyTVar' (gsFanout router) $
          Map.insert "topic1" (Set.fromList [peerA, peerB])
        join router "topic1"
        -- Fanout should be cleared
        fanout <- readTVarIO (gsFanout router)
        Map.member "topic1" fanout `shouldBe` False
        -- Mesh should contain former fanout peers
        mesh <- readTVarIO (gsMesh router)
        let meshPeers = Map.findWithDefault Set.empty "topic1" mesh
        Set.member peerA meshPeers `shouldBe` True
        Set.member peerB meshPeers `shouldBe` True

      it "fills mesh when fanout has fewer than D peers" $ do
        (router, _) <- mkTestRouter localPid
        -- 2 fanout peers + 6 more eligible
        let fanoutPeers = map mkPeerId [1, 2]
            morePeers = map mkPeerId [3..8]
        mapM_ (\pid -> addSubscribedPeer router pid "topic1") (fanoutPeers ++ morePeers)
        atomically $ modifyTVar' (gsFanout router) $
          Map.insert "topic1" (Set.fromList fanoutPeers)
        join router "topic1"
        mesh <- readTVarIO (gsMesh router)
        let meshPeers = Map.findWithDefault Set.empty "topic1" mesh
        -- Should have D=6 peers total (2 from fanout + 4 new)
        Set.size meshPeers `shouldBe` 6

    describe "leave" $ do
      it "announces unsubscription to all peers" $ do
        (router, logRef) <- mkTestRouter localPid
        let peerA = mkPeerId 1
        addPeer router peerA GossipSubPeer False fixedTime
        -- Set up mesh
        atomically $ modifyTVar' (gsMesh router) $
          Map.insert "blocks" (Set.singleton peerA)
        leave router "blocks"
        sent <- readIORef logRef
        let unsubMsgs = filter (\(_, rpc) ->
              any (\s -> not (subSubscribe s) && subTopicId s == "blocks")
                  (rpcSubscriptions rpc)) sent
        length unsubMsgs `shouldSatisfy` (>= 1)

      it "sends PRUNE with unsubscribe backoff (10s) to mesh peers" $ do
        (router, logRef) <- mkTestRouter localPid
        let peerA = mkPeerId 1
        addPeer router peerA GossipSubPeer False fixedTime
        atomically $ modifyTVar' (gsMesh router) $
          Map.insert "blocks" (Set.singleton peerA)
        leave router "blocks"
        sent <- readIORef logRef
        let pruneMsgs = filter (\(pid, rpc) ->
              pid == peerA && case rpcControl rpc of
                Just ctrl -> any (\p -> pruneTopic p == "blocks"
                                     && pruneBackoff p == Just 10) (ctrlPrune ctrl)
                Nothing -> False) sent
        length pruneMsgs `shouldBe` 1

      it "deletes mesh entry for topic" $ do
        (router, _) <- mkTestRouter localPid
        atomically $ modifyTVar' (gsMesh router) $
          Map.insert "blocks" (Set.singleton (mkPeerId 1))
        leave router "blocks"
        mesh <- readTVarIO (gsMesh router)
        Map.member "blocks" mesh `shouldBe` False

    describe "handleGraft" $ do
      it "accepts graft when subscribed and no backoff" $ do
        (router, logRef) <- mkTestRouter localPid
        let sender = mkPeerId 1
        addPeer router sender GossipSubPeer False fixedTime
        -- Create mesh entry (we're subscribed)
        atomically $ modifyTVar' (gsMesh router) $
          Map.insert "blocks" Set.empty
        handleGraft router sender [Graft "blocks"]
        -- Sender should be in mesh
        mesh <- readTVarIO (gsMesh router)
        let meshPeers = Map.findWithDefault Set.empty "blocks" mesh
        Set.member sender meshPeers `shouldBe` True
        -- No PRUNE should be sent
        sent <- readIORef logRef
        let pruneMsgs = filter (\(_, rpc) ->
              case rpcControl rpc of
                Just ctrl -> not (null (ctrlPrune ctrl))
                Nothing -> False) sent
        length pruneMsgs `shouldBe` 0

      it "rejects graft when not subscribed (sends PRUNE)" $ do
        (router, logRef) <- mkTestRouter localPid
        let sender = mkPeerId 1
        addPeer router sender GossipSubPeer False fixedTime
        -- No mesh entry = not subscribed
        handleGraft router sender [Graft "unknown-topic"]
        sent <- readIORef logRef
        let pruneMsgs = filter (\(pid, rpc) ->
              pid == sender && case rpcControl rpc of
                Just ctrl -> any (\p -> pruneTopic p == "unknown-topic") (ctrlPrune ctrl)
                Nothing -> False) sent
        length pruneMsgs `shouldBe` 1

      it "rejects graft during backoff (sends PRUNE with backoff)" $ do
        (router, logRef, timeRef) <- mkTestRouterWithTime localPid fixedTime
        let sender = mkPeerId 1
        addPeer router sender GossipSubPeer False fixedTime
        atomically $ modifyTVar' (gsMesh router) $
          Map.insert "blocks" Set.empty
        -- Set backoff that expires in the future
        let backoffExpiry = addUTCTime 60 fixedTime
        atomically $ modifyTVar' (gsBackoff router) $
          Map.insert (sender, "blocks") backoffExpiry
        handleGraft router sender [Graft "blocks"]
        -- Should send PRUNE
        sent <- readIORef logRef
        let pruneMsgs = filter (\(pid, rpc) ->
              pid == sender && case rpcControl rpc of
                Just ctrl -> any (\p -> pruneTopic p == "blocks") (ctrlPrune ctrl)
                Nothing -> False) sent
        length pruneMsgs `shouldBe` 1
        -- Sender should NOT be in mesh
        mesh <- readTVarIO (gsMesh router)
        Set.member sender (Map.findWithDefault Set.empty "blocks" mesh) `shouldBe` False

    describe "handlePrune" $ do
      it "removes peer from mesh" $ do
        (router, _) <- mkTestRouter localPid
        let sender = mkPeerId 1
        atomically $ modifyTVar' (gsMesh router) $
          Map.insert "blocks" (Set.singleton sender)
        handlePrune router sender [Prune "blocks" [] (Just 60)]
        mesh <- readTVarIO (gsMesh router)
        Set.member sender (Map.findWithDefault Set.empty "blocks" mesh) `shouldBe` False

      it "starts backoff timer" $ do
        (router, _, _) <- mkTestRouterWithTime localPid fixedTime
        let sender = mkPeerId 1
        atomically $ modifyTVar' (gsMesh router) $
          Map.insert "blocks" (Set.singleton sender)
        handlePrune router sender [Prune "blocks" [] (Just 60)]
        backoff <- readTVarIO (gsBackoff router)
        case Map.lookup (sender, "blocks") backoff of
          Just expires -> expires `shouldBe` addUTCTime 60 fixedTime
          Nothing -> expectationFailure "backoff timer not set"

      it "uses default backoff when not specified in PRUNE" $ do
        (router, _, _) <- mkTestRouterWithTime localPid fixedTime
        let sender = mkPeerId 1
        atomically $ modifyTVar' (gsMesh router) $
          Map.insert "blocks" (Set.singleton sender)
        handlePrune router sender [Prune "blocks" [] Nothing]
        backoff <- readTVarIO (gsBackoff router)
        case Map.lookup (sender, "blocks") backoff of
          Just expires -> expires `shouldBe` addUTCTime 60 fixedTime  -- default pruneBackoff=60
          Nothing -> expectationFailure "backoff timer not set"

    describe "handleSubscriptions" $ do
      it "updates peer topic set" $ do
        (router, _) <- mkTestRouter localPid
        let sender = mkPeerId 1
        addPeer router sender GossipSubPeer False fixedTime
        handleSubscriptions router sender
          [SubOpts True "blocks", SubOpts True "tx"]
        peers <- readTVarIO (gsPeers router)
        case Map.lookup sender peers of
          Just ps -> psTopics ps `shouldBe` Set.fromList ["blocks", "tx"]
          Nothing -> expectationFailure "peer not found"

      it "removes topics on unsubscribe" $ do
        (router, _) <- mkTestRouter localPid
        let sender = mkPeerId 1
        addPeer router sender GossipSubPeer False fixedTime
        handleSubscriptions router sender [SubOpts True "blocks", SubOpts True "tx"]
        handleSubscriptions router sender [SubOpts False "blocks"]
        peers <- readTVarIO (gsPeers router)
        case Map.lookup sender peers of
          Just ps -> psTopics ps `shouldBe` Set.singleton "tx"
          Nothing -> expectationFailure "peer not found"

      it "ignores subscriptions from unknown peers" $ do
        (router, _) <- mkTestRouter localPid
        let unknown = mkPeerId 99
        handleSubscriptions router unknown [SubOpts True "blocks"]
        peers <- readTVarIO (gsPeers router)
        Map.member unknown peers `shouldBe` False

    describe "handleIHave" $ do
      it "requests unseen message IDs via IWANT" $ do
        (router, logRef) <- mkTestRouter localPid
        let sender = mkPeerId 1
            mid1 = BS.pack [1, 2]
            mid2 = BS.pack [3, 4]
        addPeer router sender GossipSubPeer False fixedTime
        -- Mark mid1 as seen
        atomically $ modifyTVar' (gsSeen router) $
          Map.insert mid1 fixedTime
        handleIHave router sender [IHave "blocks" [mid1, mid2]]
        sent <- readIORef logRef
        -- Should send IWANT for mid2 only
        let iwantMsgs = concatMap (\(_, rpc) ->
              case rpcControl rpc of
                Just ctrl -> concatMap (\(IWant mids) -> mids) (ctrlIWant ctrl)
                Nothing -> []) sent
        iwantMsgs `shouldBe` [mid2]

      it "ignores IHAVE when all messages are seen" $ do
        (router, logRef) <- mkTestRouter localPid
        let sender = mkPeerId 1
            mid1 = BS.pack [1, 2]
        addPeer router sender GossipSubPeer False fixedTime
        atomically $ modifyTVar' (gsSeen router) $
          Map.insert mid1 fixedTime
        handleIHave router sender [IHave "blocks" [mid1]]
        sent <- readIORef logRef
        -- Should not send any IWANT
        sent `shouldBe` []

    describe "handleIWant" $ do
      it "is a stub in Phase 9a (does nothing)" $ do
        (router, logRef) <- mkTestRouter localPid
        let sender = mkPeerId 1
        handleIWant router sender [IWant [BS.pack [1]]]
        sent <- readIORef logRef
        sent `shouldBe` []

    describe "publish" $ do
      it "flood publishes to all topic peers" $ do
        (router, logRef) <- mkTestRouter localPid
        let peerA = mkPeerId 1
            peerB = mkPeerId 2
            peerC = mkPeerId 3  -- not subscribed
        addSubscribedPeer router peerA "blocks"
        addSubscribedPeer router peerB "blocks"
        addPeer router peerC GossipSubPeer False fixedTime
        publish router "blocks" (BS.pack [1, 2, 3]) Nothing
        sent <- readIORef logRef
        -- peerA and peerB should receive, but not peerC
        let publishedTo = map fst $ filter (\(_, rpc) -> not (null (rpcPublish rpc))) sent
        Set.fromList publishedTo `shouldBe` Set.fromList [peerA, peerB]

      it "marks published message as seen" $ do
        (router, _) <- mkTestRouter localPid
        let peerA = mkPeerId 1
        addSubscribedPeer router peerA "blocks"
        publish router "blocks" (BS.pack [1, 2, 3]) Nothing
        seen <- readTVarIO (gsSeen router)
        Map.size seen `shouldBe` 1

      it "delivers message to local application callback" $ do
        (router, _) <- mkTestRouter localPid
        deliveredRef <- newIORef ([] :: [(Topic, ByteString)])
        atomically $ writeTVar (gsOnMessage router) $ \topic msg ->
          modifyIORef' deliveredRef (++ [(topic, msgData msg)])
        let peerA = mkPeerId 1
        addSubscribedPeer router peerA "blocks"
        publish router "blocks" (BS.pack [42]) Nothing
        delivered <- readIORef deliveredRef
        length delivered `shouldBe` 1
        snd (head delivered) `shouldBe` BS.pack [42]

    describe "forwardMessage" $ do
      it "forwards to mesh peers excluding sender" $ do
        (router, logRef) <- mkTestRouter localPid
        let sender = mkPeerId 1
            peerB = mkPeerId 2
            peerC = mkPeerId 3
        -- Set up mesh with sender, peerB, peerC
        atomically $ modifyTVar' (gsMesh router) $
          Map.insert "blocks" (Set.fromList [sender, peerB, peerC])
        let msg = PubSubMessage Nothing (BS.pack [1]) Nothing "blocks" Nothing Nothing
        forwardMessage router sender msg
        sent <- readIORef logRef
        let sentTo = map fst sent
        -- Should forward to B and C, not sender
        Set.fromList sentTo `shouldBe` Set.fromList [peerB, peerC]

      it "does nothing when no mesh peers" $ do
        (router, logRef) <- mkTestRouter localPid
        let msg = PubSubMessage Nothing (BS.pack [1]) Nothing "empty" Nothing Nothing
        forwardMessage router (mkPeerId 1) msg
        sent <- readIORef logRef
        sent `shouldBe` []

    describe "handleRPC" $ do
      it "dispatches subscriptions, publish, and control" $ do
        (router, logRef) <- mkTestRouter localPid
        let sender = mkPeerId 1
        addPeer router sender GossipSubPeer False fixedTime
        -- Create mesh so GRAFT can be accepted
        atomically $ modifyTVar' (gsMesh router) $
          Map.insert "blocks" Set.empty
        let rpc = RPC
              { rpcSubscriptions = [SubOpts True "blocks"]
              , rpcPublish = []
              , rpcControl = Just ControlMessage
                  { ctrlIHave = []
                  , ctrlIWant = []
                  , ctrlGraft = [Graft "blocks"]
                  , ctrlPrune = []
                  }
              }
        handleRPC router sender rpc
        -- Subscription should be recorded
        peers <- readTVarIO (gsPeers router)
        case Map.lookup sender peers of
          Just ps -> Set.member "blocks" (psTopics ps) `shouldBe` True
          Nothing -> expectationFailure "peer not found"
        -- GRAFT should be accepted
        mesh <- readTVarIO (gsMesh router)
        Set.member sender (Map.findWithDefault Set.empty "blocks" mesh) `shouldBe` True

      it "deduplicates published messages via seen cache" $ do
        (router, _) <- mkTestRouter localPid
        let sender = mkPeerId 1
        addPeer router sender GossipSubPeer False fixedTime
        deliveredRef <- newIORef (0 :: Int)
        atomically $ writeTVar (gsOnMessage router) $ \_ _ ->
          modifyIORef' deliveredRef (+ 1)
        let msg = PubSubMessage (Just (BS.pack [1])) (BS.pack [1]) (Just (BS.pack [0,0,0,0,0,0,0,1])) "t" Nothing Nothing
            rpc = emptyRPC { rpcPublish = [msg] }
        -- Send same message twice
        handleRPC router sender rpc
        handleRPC router sender rpc
        delivered <- readIORef deliveredRef
        -- Should only be delivered once
        delivered `shouldBe` 1

    describe "peerScore" $ do
      it "returns 0 for all peers (Phase 9a stub)" $ do
        (router, _) <- mkTestRouter localPid
        score <- peerScore router (mkPeerId 1)
        score `shouldBe` 0
