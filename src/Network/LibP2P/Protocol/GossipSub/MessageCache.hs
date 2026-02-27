-- | GossipSub sliding-window message cache operations (docs/11-pubsub.md).
--
-- Stores recently seen messages in circular windows for IWANT responses
-- and IHAVE gossip emission. Each heartbeat calls 'cacheShift' to rotate
-- windows; messages older than 'mcLen' windows are evicted.
--
-- For gossip emission, only the 'mcGossip' most recent windows are
-- consulted (subset of full cache).
--
-- Pure operations — the MessageCache and CacheEntry types are defined
-- in Types.hs to avoid circular imports with Router.hs.
module Network.LibP2P.Protocol.GossipSub.MessageCache
  ( newMessageCache
  , cachePut
  , cacheGet
  , cacheGetGossipIds
  , cacheShift
  ) where

import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Network.LibP2P.Protocol.GossipSub.Types
  ( Topic, MessageId, PubSubMessage (..)
  , CacheEntry (..), MessageCache (..)
  )

-- | Create an empty message cache with given window count and gossip window count.
newMessageCache :: Int -> Int -> MessageCache
newMessageCache len gossip = MessageCache
  { mcWindows = Seq.replicate len []
  , mcIndex   = Map.empty
  , mcLen     = len
  , mcGossip  = gossip
  }

-- | Add a message to the current (newest) window.
cachePut :: MessageId -> PubSubMessage -> MessageCache -> MessageCache
cachePut mid msg mc =
  let entry = CacheEntry mid msg (msgTopic msg)
      -- Prepend entry to the newest window (index 0)
      windows = case Seq.viewl (mcWindows mc) of
        Seq.EmptyL -> Seq.singleton [entry]
        newest Seq.:< rest -> (entry : newest) Seq.<| rest
  in mc
    { mcWindows = windows
    , mcIndex   = Map.insert mid entry (mcIndex mc)
    }

-- | Look up a message by ID.
cacheGet :: MessageId -> MessageCache -> Maybe PubSubMessage
cacheGet mid mc = ceMessage <$> Map.lookup mid (mcIndex mc)

-- | Get message IDs for gossip emission (IHAVE) — only from the gossip windows.
-- Filters by topic.
cacheGetGossipIds :: Topic -> MessageCache -> [MessageId]
cacheGetGossipIds topic mc =
  let gossipWindows = Seq.take (mcGossip mc) (mcWindows mc)
      entries = concatMap id (toList gossipWindows)
  in [ ceMessageId e | e <- entries, ceTopic e == topic ]

-- | Rotate windows: prepend a new empty window, drop the oldest.
-- Entries in the dropped window are removed from the index.
cacheShift :: MessageCache -> MessageCache
cacheShift mc =
  let windows = mcWindows mc
      -- Drop the oldest window
      (kept, dropped) = case Seq.viewr windows of
        Seq.EmptyR -> (Seq.empty, [])
        rest Seq.:> oldest -> (rest, oldest)
      -- Add new empty window at front
      newWindows = (Seq.empty Seq.|> []) Seq.>< kept
      -- Remove dropped entries from index
      droppedIds = map ceMessageId dropped
      newIndex = foldl' (\idx mid -> Map.delete mid idx) (mcIndex mc) droppedIds
  in mc
    { mcWindows = newWindows
    , mcIndex   = newIndex
    }
