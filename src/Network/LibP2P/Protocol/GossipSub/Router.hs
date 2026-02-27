-- | GossipSub mesh management: JOIN, LEAVE, GRAFT/PRUNE, message forwarding.
--
-- The router manages the mesh overlay and handles inbound/outbound
-- RPC messages. For testability, peer communication is injectable
-- via gsSendRPC on GossipSubRouter.
module Network.LibP2P.Protocol.GossipSub.Router
  ( -- * Construction
    newRouter
    -- * Peer management
  , addPeer
  , removePeer
    -- * Topic subscription
  , join
  , leave
    -- * Publishing
  , publish
    -- * Inbound RPC handling
  , handleRPC
    -- * Control message handlers
  , handleGraft
  , handlePrune
  , handleIHave
  , handleIWant
  , handleSubscriptions
    -- * Message forwarding
  , forwardMessage
    -- * Scoring
  , peerScore
  ) where

import Prelude
import Control.Monad (unless)
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (UTCTime, addUTCTime)
import Data.Word (Word64)
import Crypto.Random (getRandomBytes)
import List.Shuffle (sampleIO)
import Network.LibP2P.Crypto.PeerId (PeerId, peerIdBytes)
import Network.LibP2P.Crypto.Key (KeyPair (..), sign)
import Network.LibP2P.Crypto.Protobuf (encodePublicKey)
import Network.LibP2P.Protocol.GossipSub.Types
import Network.LibP2P.Protocol.GossipSub.Message (encodePubSubMessageBS)
import Network.LibP2P.Protocol.GossipSub.MessageCache (newMessageCache, cachePut, cacheGet)
import Network.LibP2P.Protocol.GossipSub.Score (computeScore, addP7Penalty, recordMeshFailure)

-- | Create a new GossipSub router with empty state.
newRouter :: GossipSubParams
          -> PeerId
          -> (PeerId -> RPC -> IO ())   -- ^ RPC sender
          -> IO UTCTime                 -- ^ Time source
          -> IO GossipSubRouter
newRouter params localPid sendRPC getTime = do
  mesh     <- newTVarIO Map.empty
  fanout   <- newTVarIO Map.empty
  fanoutPub <- newTVarIO Map.empty
  peers    <- newTVarIO Map.empty
  seen     <- newTVarIO Map.empty
  backoff  <- newTVarIO Map.empty
  ipCount  <- newTVarIO Map.empty
  mcache   <- newTVarIO (newMessageCache (paramMcacheLen params) (paramMcacheGossip params))
  hbCount  <- newTVarIO 0
  onMsg    <- newTVarIO (\_ _ -> pure ())
  pure GossipSubRouter
    { gsParams         = params
    , gsLocalPeerId    = localPid
    , gsMesh           = mesh
    , gsFanout         = fanout
    , gsFanoutPub      = fanoutPub
    , gsPeers          = peers
    , gsSeen           = seen
    , gsBackoff        = backoff
    , gsScoreParams    = defaultPeerScoreParams
    , gsThresholds     = defaultScoreThresholds
    , gsIPPeerCount    = ipCount
    , gsMessageCache   = mcache
    , gsHeartbeatCount = hbCount
    , gsSendRPC        = sendRPC
    , gsGetTime        = getTime
    , gsOnMessage      = onMsg
    }

-- Peer management

-- | Register a connected peer.
addPeer :: GossipSubRouter -> PeerId -> PeerProtocol -> Bool -> UTCTime -> IO ()
addPeer router pid proto isOutbound now = atomically $
  modifyTVar' (gsPeers router) $
    Map.insert pid PeerState
      { psProtocol        = proto
      , psTopics          = Set.empty
      , psIsOutbound      = isOutbound
      , psConnectedAt     = now
      , psTopicState      = Map.empty
      , psBehaviorPenalty = 0
      , psIPAddress       = Nothing
      , psCachedScore     = 0
      }

-- | Remove a disconnected peer and clean up mesh/fanout membership.
removePeer :: GossipSubRouter -> PeerId -> IO ()
removePeer router pid = atomically $ do
  modifyTVar' (gsPeers router) (Map.delete pid)
  modifyTVar' (gsMesh router) (Map.map (Set.delete pid))
  modifyTVar' (gsFanout router) (Map.map (Set.delete pid))

-- Topic subscription

-- | Subscribe to a topic (JOIN): announce, fanout→mesh transition, fill to D, GRAFT.
join :: GossipSubRouter -> Topic -> IO ()
join router topic = do
  -- 1. Announce subscription to all known peers
  peers <- readTVarIO (gsPeers router)
  let allPeerIds = Map.keys peers
      subRPC = emptyRPC { rpcSubscriptions = [SubOpts True topic] }
  mapM_ (\pid -> gsSendRPC router pid subRPC) allPeerIds

  -- 2. Check fanout and transition to mesh
  (fanoutPeers, topicPeers) <- atomically $ do
    fo <- readTVar (gsFanout router)
    let foPeers = Map.findWithDefault Set.empty topic fo
    -- Move fanout peers to mesh
    unless (Set.null foPeers) $ do
      modifyTVar' (gsMesh router) (Map.insert topic foPeers)
      modifyTVar' (gsFanout router) (Map.delete topic)
      modifyTVar' (gsFanoutPub router) (Map.delete topic)
    -- Get current mesh and all eligible peers
    meshNow <- readTVar (gsMesh router)
    let currentMesh = Map.findWithDefault Set.empty topic meshNow
    peerMap <- readTVar (gsPeers router)
    let eligible = Map.foldlWithKey' (\acc pid ps ->
          if Set.member topic (psTopics ps)
             && not (Set.member pid currentMesh)
             && pid /= gsLocalPeerId router
          then Set.insert pid acc
          else acc) Set.empty peerMap
    pure (foPeers, eligible)

  -- 3. Fill mesh to D if needed
  currentMesh <- atomically $ do
    m <- readTVar (gsMesh router)
    pure (Map.findWithDefault Set.empty topic m)
  let needed = paramD (gsParams router) - Set.size currentMesh
  newPeers <- if needed > 0 && not (Set.null topicPeers)
    then do
      selected <- sampleIO (min needed (Set.size topicPeers)) (Set.toList topicPeers)
      let newSet = Set.fromList selected
      atomically $ modifyTVar' (gsMesh router) $
        Map.insertWith Set.union topic newSet
      pure newSet
    else pure Set.empty

  -- 4. Send GRAFT to all new mesh peers (including former fanout peers)
  let allNewMeshPeers = Set.union (Set.difference fanoutPeers currentMesh) newPeers
  mapM_ (\pid -> gsSendRPC router pid (graftRPC topic)) (Set.toList allNewMeshPeers)

-- | Unsubscribe from a topic (LEAVE): announce, PRUNE with backoff, delete mesh.
leave :: GossipSubRouter -> Topic -> IO ()
leave router topic = do
  -- 1. Announce unsubscription to all known peers
  peers <- readTVarIO (gsPeers router)
  let allPeerIds = Map.keys peers
      unsubRPC = emptyRPC { rpcSubscriptions = [SubOpts False topic] }
  mapM_ (\pid -> gsSendRPC router pid unsubRPC) allPeerIds

  -- 2. Send PRUNE with unsubscribe backoff to mesh peers, then delete
  meshPeers <- atomically $ do
    m <- readTVar (gsMesh router)
    let mp = Map.findWithDefault Set.empty topic m
    modifyTVar' (gsMesh router) (Map.delete topic)
    pure mp
  let backoffSecs = round (paramUnsubBackoff (gsParams router)) :: Word64
  mapM_ (\pid -> gsSendRPC router pid (pruneRPC topic [] (Just backoffSecs))) (Set.toList meshPeers)

-- Publishing

-- | Publish a message to a topic.
-- In StrictSign mode, signs the message and populates from/seqno/signature/key.
-- With FloodPublish=True (default), sends to ALL topic peers above PublishThreshold.
-- Otherwise, sends via mesh (or fanout if not subscribed).
publish :: GossipSubRouter -> Topic -> ByteString -> Maybe KeyPair -> IO ()
publish router topic payload mKeyPair = do
  now <- gsGetTime router

  -- Build message (with signing if StrictSign)
  msg <- case paramSignaturePolicy (gsParams router) of
    StrictSign -> case mKeyPair of
      Nothing -> pure $ mkUnsignedMessage topic payload
      Just kp -> mkSignedMessage router topic payload kp
    StrictNoSign -> pure $ mkUnsignedMessage topic payload

  let msgId = paramMessageIdFn (gsParams router) msg

  -- Mark as seen
  atomically $ modifyTVar' (gsSeen router) (Map.insert msgId now)

  -- Build RPC with published message
  let pubRPC = emptyRPC { rpcPublish = [msg] }

  if paramFloodPublish (gsParams router)
    then do
      -- Flood publish: send to ALL peers in topic
      peers <- readTVarIO (gsPeers router)
      let targets = Map.foldlWithKey' (\acc pid ps ->
            if Set.member topic (psTopics ps) && pid /= gsLocalPeerId router
            then pid : acc
            else acc) [] peers
      mapM_ (\pid -> gsSendRPC router pid pubRPC) targets
    else do
      -- Mesh-based publish
      meshPeers <- atomically $ do
        m <- readTVar (gsMesh router)
        pure (Map.findWithDefault Set.empty topic m)
      if not (Set.null meshPeers)
        then mapM_ (\pid -> gsSendRPC router pid pubRPC) (Set.toList meshPeers)
        else do
          -- Fanout: use existing or create new
          foPeers <- atomically $ do
            fo <- readTVar (gsFanout router)
            pure (Map.findWithDefault Set.empty topic fo)
          targets <- if Set.null foPeers
            then do
              peers <- readTVarIO (gsPeers router)
              let eligible = Map.foldlWithKey' (\acc pid ps ->
                    if Set.member topic (psTopics ps) && pid /= gsLocalPeerId router
                    then pid : acc
                    else acc) [] peers
              selected <- sampleIO (min (paramD (gsParams router)) (length eligible)) eligible
              let selectedSet = Set.fromList selected
              atomically $ do
                modifyTVar' (gsFanout router) (Map.insert topic selectedSet)
                modifyTVar' (gsFanoutPub router) (Map.insert topic now)
              pure selectedSet
            else do
              atomically $ modifyTVar' (gsFanoutPub router) (Map.insert topic now)
              pure foPeers
          mapM_ (\pid -> gsSendRPC router pid pubRPC) (Set.toList targets)

  -- Deliver to local application
  onMsg <- readTVarIO (gsOnMessage router)
  onMsg topic msg

-- Inbound RPC handling

-- | Handle an inbound RPC from a peer.
handleRPC :: GossipSubRouter -> PeerId -> RPC -> IO ()
handleRPC router sender rpc = do
  -- Process subscriptions
  handleSubscriptions router sender (rpcSubscriptions rpc)

  -- Process published messages
  mapM_ (handlePublishedMessage router sender) (rpcPublish rpc)

  -- Process control messages
  case rpcControl rpc of
    Nothing -> pure ()
    Just ctrl -> do
      handleIHave router sender (ctrlIHave ctrl)
      handleIWant router sender (ctrlIWant ctrl)
      handleGraft router sender (ctrlGraft ctrl)
      handlePrune router sender (ctrlPrune ctrl)

-- | Process a published message: deduplicate, validate, forward, deliver.
handlePublishedMessage :: GossipSubRouter -> PeerId -> PubSubMessage -> IO ()
handlePublishedMessage router sender msg = do
  let msgId = paramMessageIdFn (gsParams router) msg
  now <- gsGetTime router

  -- Deduplicate
  alreadySeen <- atomically $ do
    s <- readTVar (gsSeen router)
    if Map.member msgId s
      then pure True
      else do
        writeTVar (gsSeen router) (Map.insert msgId now s)
        pure False

  if alreadySeen
    then pure ()
    else do
      -- Cache the message for IWANT responses
      atomically $ modifyTVar' (gsMessageCache router) $
        cachePut msgId msg

      -- Forward to mesh peers (excluding sender)
      forwardMessage router sender msg

      -- Deliver to application
      onMsg <- readTVarIO (gsOnMessage router)
      onMsg (msgTopic msg) msg

-- Control message handlers

-- | Handle GRAFT: accept if subscribed, non-negative score, and no backoff.
handleGraft :: GossipSubRouter -> PeerId -> [Graft] -> IO ()
handleGraft router sender grafts = do
  now <- gsGetTime router
  pruneResponses <- mapM (handleOneGraft router sender now) grafts
  let prunes = concat pruneResponses
  unless (null prunes) $
    gsSendRPC router sender emptyRPC
      { rpcControl = Just emptyControlMessage { ctrlPrune = prunes } }

-- | Handle a single GRAFT request.
handleOneGraft :: GossipSubRouter -> PeerId -> UTCTime -> Graft -> IO [Prune]
handleOneGraft router sender now (Graft topic) = do
  -- Check if we are subscribed to this topic
  meshMap <- readTVarIO (gsMesh router)
  let subscribed = Map.member topic meshMap

  if not subscribed
    then pure [Prune topic [] Nothing]  -- Not subscribed → PRUNE
    else do
      -- Check backoff
      backoffMap <- readTVarIO (gsBackoff router)
      let inBackoff = case Map.lookup (sender, topic) backoffMap of
            Nothing -> False
            Just expires -> now < expires

      -- Check score (stub: always >= 0 in Phase 9a)
      score <- peerScore router sender

      if inBackoff
        then do
          -- P7 penalty: GRAFT during backoff is a protocol violation
          atomically $ modifyTVar' (gsPeers router) $
            Map.adjust addP7Penalty sender
          let backoffSecs = round (paramPruneBackoff (gsParams router)) :: Word64
          pure [Prune topic [] (Just backoffSecs)]
        else if score < 0
          then pure [Prune topic [] Nothing]
          else do
            -- Accept: add sender to mesh
            atomically $ modifyTVar' (gsMesh router) $
              Map.insertWith Set.union topic (Set.singleton sender)
            pure []

-- | Handle PRUNE: remove from mesh and start backoff.
handlePrune :: GossipSubRouter -> PeerId -> [Prune] -> IO ()
handlePrune router sender prunes = do
  now <- gsGetTime router
  mapM_ (handleOnePrune router sender now) prunes

handleOnePrune :: GossipSubRouter -> PeerId -> UTCTime -> Prune -> IO ()
handleOnePrune router sender now prune = do
  let topic = pruneTopic prune
  -- Record P3b mesh failure: snapshot delivery deficit before removing
  let scoreParams = gsScoreParams router
  case Map.lookup topic (pspTopicParams scoreParams) of
    Just tsp -> atomically $ modifyTVar' (gsPeers router) $
      Map.adjust (\ps ->
        let topicSt = Map.findWithDefault defaultTopicPeerState topic (psTopicState ps)
            topicSt' = recordMeshFailure tsp topicSt
        in ps { psTopicState = Map.insert topic topicSt' (psTopicState ps) }
      ) sender
    Nothing -> pure ()
  -- Remove sender from mesh
  atomically $ modifyTVar' (gsMesh router) $
    Map.adjust (Set.delete sender) topic
  -- Start backoff timer
  let backoffDuration = case pruneBackoff prune of
        Just secs -> fromIntegral secs
        Nothing   -> paramPruneBackoff (gsParams router)
      expires = addUTCTime backoffDuration now
  atomically $ modifyTVar' (gsBackoff router) $
    Map.insert (sender, topic) expires

-- | Handle IHAVE: request unseen messages via IWANT.
handleIHave :: GossipSubRouter -> PeerId -> [IHave] -> IO ()
handleIHave router sender ihaves = do
  seenMap <- readTVarIO (gsSeen router)
  let unseen = concatMap (\(IHave _ mids) ->
        filter (\mid -> not (Map.member mid seenMap)) mids) ihaves
  unless (null unseen) $
    gsSendRPC router sender emptyRPC
      { rpcControl = Just emptyControlMessage { ctrlIWant = [IWant unseen] } }

-- | Handle IWANT: respond with cached messages from the message cache.
handleIWant :: GossipSubRouter -> PeerId -> [IWant] -> IO ()
handleIWant router sender iwants = do
  cache <- readTVarIO (gsMessageCache router)
  let requestedIds = concatMap iwantMessageIds iwants
      found = [ msg | mid <- requestedIds
                     , Just msg <- [cacheGet mid cache] ]
  unless (null found) $
    gsSendRPC router sender emptyRPC { rpcPublish = found }

-- | Handle subscription changes from a peer.
handleSubscriptions :: GossipSubRouter -> PeerId -> [SubOpts] -> IO ()
handleSubscriptions router sender subs = atomically $
  modifyTVar' (gsPeers router) $ \peerMap ->
    case Map.lookup sender peerMap of
      Nothing -> peerMap  -- Unknown peer, ignore
      Just ps ->
        let topics' = foldl (\ts sub ->
              if subSubscribe sub
                then Set.insert (subTopicId sub) ts
                else Set.delete (subTopicId sub) ts
              ) (psTopics ps) subs
        in Map.insert sender ps { psTopics = topics' } peerMap

-- Message forwarding

-- | Forward a message to mesh peers for its topic, excluding the sender.
forwardMessage :: GossipSubRouter -> PeerId -> PubSubMessage -> IO ()
forwardMessage router sender msg = do
  let topic = msgTopic msg
  meshPeers <- atomically $ do
    m <- readTVar (gsMesh router)
    pure (Map.findWithDefault Set.empty topic m)
  let targets = Set.delete sender meshPeers
      fwdRPC = emptyRPC { rpcPublish = [msg] }
  mapM_ (\pid -> gsSendRPC router pid fwdRPC) (Set.toList targets)

-- Scoring

-- | Compute peer score using Score.computeScore (P1-P4, P6, P7).
-- P5 (application-specific) is not included here.
peerScore :: GossipSubRouter -> PeerId -> IO Double
peerScore router pid = do
  peers <- readTVarIO (gsPeers router)
  now <- gsGetTime router
  ipMap <- readTVarIO (gsIPPeerCount router)
  case Map.lookup pid peers of
    Nothing -> pure 0
    Just ps -> pure $ computeScore (gsScoreParams router) ps ipMap now

-- Helper: construct a GRAFT RPC
graftRPC :: Topic -> RPC
graftRPC topic = emptyRPC
  { rpcControl = Just emptyControlMessage { ctrlGraft = [Graft topic] } }

-- Helper: construct a PRUNE RPC
pruneRPC :: Topic -> [PeerExchangeInfo] -> Maybe Word64 -> RPC
pruneRPC topic peers backoff = emptyRPC
  { rpcControl = Just emptyControlMessage { ctrlPrune = [Prune topic peers backoff] } }

-- Helpers: message construction

mkUnsignedMessage :: Topic -> ByteString -> PubSubMessage
mkUnsignedMessage topic payload = PubSubMessage
  { msgFrom      = Nothing
  , msgData      = payload
  , msgSeqNo     = Nothing
  , msgTopic     = topic
  , msgSignature = Nothing
  , msgKey       = Nothing
  }

mkSignedMessage :: GossipSubRouter -> Topic -> ByteString -> KeyPair -> IO PubSubMessage
mkSignedMessage router topic payload kp = do
  seqno <- getRandomBytes 8 :: IO ByteString
  let from = peerIdBytes (gsLocalPeerId router)
      pubKeyBytes = encodePublicKey (kpPublic kp)
      -- Build unsigned message for signing
      unsigned = PubSubMessage
        { msgFrom      = Just from
        , msgData      = payload
        , msgSeqNo     = Just seqno
        , msgTopic     = topic
        , msgSignature = Nothing
        , msgKey       = Just pubKeyBytes
        }
      -- Sign: prefix "libp2p-pubsub:" + marshalled message
      signData = "libp2p-pubsub:" <> marshalForSigning unsigned
  case sign (kpPrivate kp) signData of
    Left _err -> pure unsigned  -- Signing failed, send unsigned
    Right sig -> pure unsigned { msgSignature = Just sig }

-- | Marshal a message for signature computation (protobuf encoding, minus signature).
marshalForSigning :: PubSubMessage -> ByteString
marshalForSigning msg = encodePubSubMessageBS (msg { msgSignature = Nothing })
