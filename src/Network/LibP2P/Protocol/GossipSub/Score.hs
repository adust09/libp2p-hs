-- | GossipSub peer scoring (P1-P7) per docs/11-pubsub.md.
--
-- Score formula:
--   Score(p) = TopicCap(Sum(t_i * (w1*P1 + w2*P2 + w3*P3 + w3b*P3b + w4*P4)))
--              + w5*P5 + w6*P6 + w7*P7
--
-- All penalty parameters (P3, P3b, P4, P6, P7) use quadratic escalation:
-- the score contribution is the square of the deficit/counter, mixed with
-- a negative weight. This makes small deficits tolerable but large ones
-- devastating.
module Network.LibP2P.Protocol.GossipSub.Score
  ( -- * Individual score components
    computeP1
  , computeP2
  , computeP3
  , computeP3b
  , computeP4
  , computeP6
  , computeP7
    -- * Aggregate score
  , computeScore
    -- * Decay
  , decayCounter
  , decayPeerCounters
    -- * Counter recording
  , recordFirstDelivery
  , recordMeshDelivery
  , recordInvalidMessage
  , recordMeshFailure
  , addP7Penalty
  ) where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (UTCTime, diffUTCTime)
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.Protocol.GossipSub.Types

-- | P1: Time in Mesh. Returns min(meshTime/quantum, cap).
-- Only counts when peer is actually in mesh.
computeP1 :: TopicScoreParams -> TopicPeerState -> Double
computeP1 tsp tps
  | not (tpsInMesh tps) = 0
  | otherwise =
      let meshTime = realToFrac (tpsMeshTime tps) :: Double
          quantum = realToFrac (tspTimeInMeshQuantum tsp) :: Double
          raw = if quantum > 0 then meshTime / quantum else 0
      in min raw (tspTimeInMeshCap tsp)

-- | P2: First Message Deliveries. Returns min(counter, cap).
computeP2 :: TopicScoreParams -> TopicPeerState -> Double
computeP2 tsp tps = min (tpsFirstMessageDeliveries tps) (tspFirstMessageDeliveriesCap tsp)

-- | P3: Mesh Message Deliveries. Returns deficit^2 when activated and below threshold.
-- The weight (negative) is applied externally in computeScore.
computeP3 :: TopicScoreParams -> TopicPeerState -> UTCTime -> Double
computeP3 tsp tps now
  | not (tpsInMesh tps) = 0
  | not activated = 0
  | deficit <= 0 = 0
  | otherwise = deficit * deficit
  where
    activated = case tpsGraftTime tps of
      Nothing -> False
      Just gt ->
        let elapsed = diffUTCTime now gt
        in elapsed >= tspMeshMessageDeliveriesActivation tsp
    threshold = tspMeshMessageDeliveriesThreshold tsp
    deliveries = min (tpsMeshMessageDeliveries tps) (tspMeshMessageDeliveriesCap tsp)
    deficit = threshold - deliveries

-- | P3b: Mesh Failure Penalty. Returns the stored penalty value (already squared at capture time).
computeP3b :: TopicPeerState -> Double
computeP3b = tpsMeshFailurePenalty

-- | P4: Invalid Messages. Returns counter^2.
computeP4 :: TopicPeerState -> Double
computeP4 tps = tpsInvalidMessages tps * tpsInvalidMessages tps

-- | P6: IP Colocation Factor. Returns (count - threshold)^2 if count > threshold, else 0.
computeP6 :: PeerScoreParams -> PeerState -> Map.Map ByteString (Set.Set PeerId) -> Double
computeP6 params ps ipMap = case psIPAddress ps of
  Nothing -> 0
  Just ip ->
    let count = maybe 0 Set.size (Map.lookup ip ipMap)
        threshold = pspIPColocationFactorThreshold params
        excess = count - threshold
    in if excess > 0
       then fromIntegral (excess * excess)
       else 0

-- | P7: Behavioral Penalty. Returns counter^2.
computeP7 :: PeerState -> Double
computeP7 ps = psBehaviorPenalty ps * psBehaviorPenalty ps

-- | Compute full peer score per the formula in docs/11-pubsub.md.
-- Takes explicit PeerId for the P5 application-specific callback.
computeScore :: PeerScoreParams -> PeerState -> Map.Map ByteString (Set.Set PeerId) -> UTCTime -> Double
computeScore params ps ipMap now =
  let -- Topic score: sum over topics
      topicScore = Map.foldlWithKey' (\acc topic tps ->
        case Map.lookup topic (pspTopicParams params) of
          Nothing -> acc  -- no params for this topic, skip
          Just tsp ->
            let p1 = tspTimeInMeshWeight tsp * computeP1 tsp tps
                p2 = tspFirstMessageDeliveriesWeight tsp * computeP2 tsp tps
                p3 = tspMeshMessageDeliveriesWeight tsp * computeP3 tsp tps now
                p3b = tspMeshFailurePenaltyWeight tsp * computeP3b tps
                p4 = tspInvalidMessageDeliveriesWeight tsp * computeP4 tps
                topicContribution = tspTopicWeight tsp * (p1 + p2 + p3 + p3b + p4)
            in acc + topicContribution
        ) 0 (psTopicState ps)

      -- Apply TopicScoreCap
      cappedTopicScore =
        let cap = pspTopicScoreCap params
        in if cap > 0 && topicScore > cap
           then cap
           else topicScore

      -- P5: not computed here (requires PeerId from router context).
      -- Use computeScoreForPeer in Router for full P5 support.

      -- P6: IP colocation
      p6 = pspIPColocationFactorWeight params * computeP6 params ps ipMap

      -- P7: behavioral penalty
      p7 = pspBehaviorPenaltyWeight params * computeP7 ps

  in cappedTopicScore + p6 + p7

-- Decay

-- | Decay a single counter by a factor.
decayCounter :: Double -> Double -> Double
decayCounter factor value = value * factor

-- | Apply counter decay to all scoring counters in a PeerState.
decayPeerCounters :: PeerScoreParams -> PeerState -> PeerState
decayPeerCounters params ps =
  let decayToZero = pspDecayToZero params
      zeroCheck v = if abs v < decayToZero then 0 else v

      -- Decay topic-level counters
      decayedTopicState = Map.mapWithKey (\topic tps ->
        case Map.lookup topic (pspTopicParams params) of
          Nothing -> tps
          Just tsp -> tps
            { tpsFirstMessageDeliveries =
                zeroCheck $ decayCounter (tspFirstMessageDeliveriesDecay tsp) (tpsFirstMessageDeliveries tps)
            , tpsMeshMessageDeliveries =
                zeroCheck $ decayCounter (tspMeshMessageDeliveriesDecay tsp) (tpsMeshMessageDeliveries tps)
            , tpsMeshFailurePenalty =
                zeroCheck $ decayCounter (tspMeshFailurePenaltyDecay tsp) (tpsMeshFailurePenalty tps)
            , tpsInvalidMessages =
                zeroCheck $ decayCounter (tspInvalidMessageDeliveriesDecay tsp) (tpsInvalidMessages tps)
            }
        ) (psTopicState ps)

      -- Decay P7 (behavioral penalty)
      decayedP7 = zeroCheck $ decayCounter (pspBehaviorPenaltyDecay params) (psBehaviorPenalty ps)

  in ps { psTopicState = decayedTopicState, psBehaviorPenalty = decayedP7 }

-- Counter recording

-- | Record a first message delivery for a topic (P2 increment, capped).
recordFirstDelivery :: TopicScoreParams -> TopicPeerState -> TopicPeerState
recordFirstDelivery tsp tps =
  let current = tpsFirstMessageDeliveries tps
      cap = tspFirstMessageDeliveriesCap tsp
  in tps { tpsFirstMessageDeliveries = min (current + 1) cap }

-- | Record a mesh message delivery (P3 increment, capped).
recordMeshDelivery :: TopicScoreParams -> TopicPeerState -> TopicPeerState
recordMeshDelivery tsp tps =
  let current = tpsMeshMessageDeliveries tps
      cap = tspMeshMessageDeliveriesCap tsp
  in tps { tpsMeshMessageDeliveries = min (current + 1) cap }

-- | Record an invalid message delivery (P4 increment).
recordInvalidMessage :: TopicPeerState -> TopicPeerState
recordInvalidMessage tps =
  tps { tpsInvalidMessages = tpsInvalidMessages tps + 1 }

-- | Record a mesh failure (P3b): capture deficit^2 at prune time.
-- deficit = threshold - deliveries (clamped to >= 0).
recordMeshFailure :: TopicScoreParams -> TopicPeerState -> TopicPeerState
recordMeshFailure tsp tps =
  let threshold = tspMeshMessageDeliveriesThreshold tsp
      deliveries = tpsMeshMessageDeliveries tps
      deficit = max 0 (threshold - deliveries)
  in tps { tpsMeshFailurePenalty = tpsMeshFailurePenalty tps + deficit * deficit }

-- | Increment P7 behavioral penalty counter by 1.
addP7Penalty :: PeerState -> PeerState
addP7Penalty ps = ps { psBehaviorPenalty = psBehaviorPenalty ps + 1 }
