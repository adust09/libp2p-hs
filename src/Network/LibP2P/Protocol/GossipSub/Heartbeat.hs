-- | GossipSub heartbeat procedure (docs/11-pubsub.md).
--
-- The heartbeat runs periodically and performs:
-- 1. Mesh maintenance: prune negative-score, fill undersubscribed, trim oversubscribed
-- 2. Fanout maintenance: expire old, fill undersubscribed
-- 3. Gossip emission: send IHAVE to non-mesh peers, rotate cache
-- 4. Score decay: decay all counters for all peers
-- 5. Seen cache cleanup: remove expired entries
-- 6. Heartbeat counter increment (for opportunistic graft timing)
module Network.LibP2P.Protocol.GossipSub.Heartbeat
  ( heartbeatOnce
  , runHeartbeat
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM
import Control.Monad (forM_, unless, when)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Word (Word64)
import List.Shuffle (sampleIO)
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.Protocol.GossipSub.Types
import Network.LibP2P.Protocol.GossipSub.MessageCache (cacheGetGossipIds, cacheShift)
import Network.LibP2P.Protocol.GossipSub.Score (computeScore, decayPeerCounters)

-- | Run a single heartbeat cycle. Exported for testing.
heartbeatOnce :: GossipSubRouter -> IO ()
heartbeatOnce router = do
  meshMaintenance router
  fanoutMaintenance router
  emitGossip router
  decayAllScores router
  cleanSeenCache router
  -- Increment heartbeat counter
  atomically $ modifyTVar' (gsHeartbeatCount router) (+ 1)

-- | Start the heartbeat background thread.
runHeartbeat :: GossipSubRouter -> IO (Async ())
runHeartbeat router = async $ heartbeatLoop router

heartbeatLoop :: GossipSubRouter -> IO ()
heartbeatLoop router = do
  let intervalUs = round (paramHeartbeatInterval (gsParams router) * 1000000) :: Int
  threadDelay intervalUs
  heartbeatOnce router
  heartbeatLoop router

-- Mesh maintenance

meshMaintenance :: GossipSubRouter -> IO ()
meshMaintenance router = do
  now <- gsGetTime router
  meshMap <- readTVarIO (gsMesh router)
  forM_ (Map.toList meshMap) $ \(topic, meshPeers) -> do
    -- Step 1: Remove negative-score peers
    remaining <- pruneNegativeScore router topic meshPeers now
    -- Step 2: Fill if undersubscribed (< D_lo)
    filled <- fillUndersubscribed router topic remaining now
    -- Step 3: Trim if oversubscribed (> D_hi)
    trimOversubscribed router topic filled

-- | Remove peers with negative score from mesh, send PRUNE.
pruneNegativeScore :: GossipSubRouter -> Topic -> Set.Set PeerId -> UTCTime -> IO (Set.Set PeerId)
pruneNegativeScore router topic meshPeers now = do
  let scoreParams = gsScoreParams router
  ipMap <- readTVarIO (gsIPPeerCount router)
  peers <- readTVarIO (gsPeers router)
  let negatives = Set.filter (\pid ->
        case Map.lookup pid peers of
          Nothing -> False
          Just ps -> computeScore scoreParams ps ipMap now < 0
        ) meshPeers
  -- Send PRUNE to negative-score peers
  forM_ (Set.toList negatives) $ \pid -> do
    let backoffSecs = round (paramPruneBackoff (gsParams router)) :: Word64
    gsSendRPC router pid emptyRPC
      { rpcControl = Just emptyControlMessage
          { ctrlPrune = [Prune topic [] (Just backoffSecs)] }
      }
    -- Start backoff
    atomically $ modifyTVar' (gsBackoff router) $
      Map.insert (pid, topic) (addUTCTime (paramPruneBackoff (gsParams router)) now)
  -- Update mesh
  let remaining = Set.difference meshPeers negatives
  atomically $ modifyTVar' (gsMesh router) $
    Map.insert topic remaining
  pure remaining

-- | Fill mesh if below D_lo with eligible peers (non-negative score, no backoff).
fillUndersubscribed :: GossipSubRouter -> Topic -> Set.Set PeerId -> UTCTime -> IO (Set.Set PeerId)
fillUndersubscribed router topic meshPeers now = do
  let params = gsParams router
      dlo = paramDlo params
      d   = paramD params
  if Set.size meshPeers >= dlo
    then pure meshPeers
    else do
      -- Find eligible peers: subscribed to topic, not in mesh, not in backoff, score >= 0
      peersMap <- readTVarIO (gsPeers router)
      backoffMap <- readTVarIO (gsBackoff router)
      ipMap <- readTVarIO (gsIPPeerCount router)
      let eligible = [ pid | (pid, ps) <- Map.toList peersMap
                           , Set.member topic (psTopics ps)
                           , not (Set.member pid meshPeers)
                           , not (isInBackoff backoffMap pid topic now)
                           , computeScore (gsScoreParams router) ps ipMap now >= 0
                           ]
      let needed = d - Set.size meshPeers
      selected <- sampleIO (min needed (length eligible)) eligible
      -- Send GRAFT and add to mesh
      forM_ selected $ \pid ->
        gsSendRPC router pid emptyRPC
          { rpcControl = Just emptyControlMessage { ctrlGraft = [Graft topic] } }
      let newMesh = Set.union meshPeers (Set.fromList selected)
      atomically $ modifyTVar' (gsMesh router) $
        Map.insert topic newMesh
      pure newMesh

-- | Trim mesh if above D_hi by randomly removing excess peers, send PRUNE.
trimOversubscribed :: GossipSubRouter -> Topic -> Set.Set PeerId -> IO ()
trimOversubscribed router topic meshPeers = do
  let params = gsParams router
      dhi = paramDhi params
      d   = paramD params
  when (Set.size meshPeers > dhi) $ do
    now <- gsGetTime router
    -- Keep D peers: select D random peers to keep
    let meshList = Set.toList meshPeers
    kept <- sampleIO d meshList
    let keptSet = Set.fromList kept
        toRemove = Set.difference meshPeers keptSet
    -- Send PRUNE to removed peers
    forM_ (Set.toList toRemove) $ \pid -> do
      let backoffSecs = round (paramPruneBackoff params) :: Word64
      gsSendRPC router pid emptyRPC
        { rpcControl = Just emptyControlMessage
            { ctrlPrune = [Prune topic [] (Just backoffSecs)] }
        }
      atomically $ modifyTVar' (gsBackoff router) $
        Map.insert (pid, topic) (addUTCTime (paramPruneBackoff params) now)
    -- Update mesh
    atomically $ modifyTVar' (gsMesh router) $
      Map.insert topic keptSet

-- Fanout maintenance

fanoutMaintenance :: GossipSubRouter -> IO ()
fanoutMaintenance router = do
  now <- gsGetTime router
  let ttl = paramFanoutTTL (gsParams router)
  fanoutMap <- readTVarIO (gsFanout router)
  fanoutPubMap <- readTVarIO (gsFanoutPub router)
  forM_ (Map.toList fanoutMap) $ \(topic, fanoutPeers) -> do
    let lastPub = Map.findWithDefault now topic fanoutPubMap
    if diffUTCTime now lastPub > ttl
      then -- Expire fanout entry
        atomically $ do
          modifyTVar' (gsFanout router) (Map.delete topic)
          modifyTVar' (gsFanoutPub router) (Map.delete topic)
      else do
        -- Fill if below D
        let d = paramD (gsParams router)
        when (Set.size fanoutPeers < d) $ do
          peersMap <- readTVarIO (gsPeers router)
          let eligible = [ pid | (pid, ps) <- Map.toList peersMap
                               , Set.member topic (psTopics ps)
                               , not (Set.member pid fanoutPeers)
                               ]
          let needed = d - Set.size fanoutPeers
          selected <- sampleIO (min needed (length eligible)) eligible
          let newFanout = Set.union fanoutPeers (Set.fromList selected)
          atomically $ modifyTVar' (gsFanout router) $
            Map.insert topic newFanout

-- Gossip emission

emitGossip :: GossipSubRouter -> IO ()
emitGossip router = do
  meshMap <- readTVarIO (gsMesh router)
  cache <- readTVarIO (gsMessageCache router)
  peersMap <- readTVarIO (gsPeers router)
  let params = gsParams router

  -- For each topic in mesh, send IHAVE to non-mesh peers
  forM_ (Map.keys meshMap) $ \topic -> do
    let gossipIds = cacheGetGossipIds topic cache
    unless (null gossipIds) $ do
      let meshPeers = Map.findWithDefault Set.empty topic meshMap
          -- Eligible: subscribed to topic, not in mesh
          nonMeshPeers = [ pid | (pid, ps) <- Map.toList peersMap
                               , Set.member topic (psTopics ps)
                               , not (Set.member pid meshPeers)
                               ]
          -- Select max(D_lazy, |eligible| * gossipFactor) targets
          dlazy = paramDlazy params
          factor = paramGossipFactor params
          targetCount = max dlazy (ceiling (factor * fromIntegral (length nonMeshPeers)))
      targets <- sampleIO (min targetCount (length nonMeshPeers)) nonMeshPeers
      forM_ targets $ \pid ->
        gsSendRPC router pid emptyRPC
          { rpcControl = Just emptyControlMessage
              { ctrlIHave = [IHave topic gossipIds] }
          }

  -- Rotate cache
  atomically $ modifyTVar' (gsMessageCache router) cacheShift

-- Score decay

decayAllScores :: GossipSubRouter -> IO ()
decayAllScores router = atomically $
  modifyTVar' (gsPeers router) $
    Map.map (decayPeerCounters (gsScoreParams router))

-- Seen cache cleanup

cleanSeenCache :: GossipSubRouter -> IO ()
cleanSeenCache router = do
  now <- gsGetTime router
  let ttl = paramSeenTTL (gsParams router)
  atomically $ modifyTVar' (gsSeen router) $
    Map.filter (\ts -> diffUTCTime now ts <= ttl)

-- Helpers

isInBackoff :: Map.Map (PeerId, Topic) UTCTime -> PeerId -> Topic -> UTCTime -> Bool
isInBackoff backoffMap pid topic now =
  case Map.lookup (pid, topic) backoffMap of
    Nothing -> False
    Just expires -> now < expires
