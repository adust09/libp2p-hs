-- | k-Bucket routing table for the Kademlia DHT.
--
-- Organizes peers into 256 buckets by XOR distance prefix length.
-- Each bucket holds up to k=20 peers, ordered by last-seen time
-- (head = least-recently-seen, tail = most-recently-seen).
module Network.LibP2P.DHT.RoutingTable
  ( KBucket (..)
  , RoutingTable (..)
  , newRoutingTable
  , emptyBucket
  , insertPeer
  , removePeer
  , closestPeers
  , bucketForPeer
  , bucketSize
  , allPeers
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.DHT.Distance (commonPrefixLength, peerIdToKey, sortByDistance)
import Network.LibP2P.DHT.Types
  ( BucketEntry (..)
  , DHTKey (..)
  , InsertResult (..)
  , kValue
  , numBuckets
  )

-- | A k-bucket holding up to k peers.
-- Ordered by last-seen: head = LRS (least recently seen), tail = MRS.
data KBucket = KBucket
  { bucketEntries :: !(Seq BucketEntry)
  } deriving (Show)

-- | Full routing table: 256 k-buckets indexed by prefix length.
-- Uses sparse IntMap — only non-empty buckets are stored.
data RoutingTable = RoutingTable
  { rtSelfKey :: !DHTKey
  , rtBuckets :: !(IntMap KBucket)
  , rtK       :: !Int
  } deriving (Show)

-- | Create a new empty routing table for the given local peer.
newRoutingTable :: PeerId -> RoutingTable
newRoutingTable localPeer = RoutingTable
  { rtSelfKey = peerIdToKey localPeer
  , rtBuckets = IntMap.empty
  , rtK       = kValue
  }

-- | An empty k-bucket.
emptyBucket :: KBucket
emptyBucket = KBucket Seq.empty

-- | Insert a peer into the routing table.
--
-- Rules:
-- 1. Self is never inserted (returns Updated as no-op).
-- 2. If peer already exists in the bucket, move it to tail → Updated.
-- 3. If bucket has space, append to tail → Inserted.
-- 4. If bucket is full, return BucketFull with the LRS peer ID.
insertPeer :: BucketEntry -> RoutingTable -> (RoutingTable, InsertResult)
insertPeer entry rt
  -- Reject self-insertion
  | entryKey entry == rtSelfKey rt = (rt, Updated)
  | otherwise =
    let idx = bucketIndex (rtSelfKey rt) (entryKey entry)
        bucket = IntMap.findWithDefault emptyBucket idx (rtBuckets rt)
        entries = bucketEntries bucket
    in case findEntryIndex (entryPeerId entry) entries of
         -- Peer already in bucket: remove from current position, append to tail
         Just i ->
           let entries' = Seq.deleteAt i entries Seq.|> entry
               rt' = rt { rtBuckets = IntMap.insert idx (KBucket entries') (rtBuckets rt) }
           in (rt', Updated)
         -- Peer not in bucket
         Nothing
           | Seq.length entries < rtK rt ->
             -- Space available: append to tail
             let entries' = entries Seq.|> entry
                 rt' = rt { rtBuckets = IntMap.insert idx (KBucket entries') (rtBuckets rt) }
             in (rt', Inserted)
           | otherwise ->
             -- Bucket full: return LRS peer for potential eviction
             case entries of
               lrs :<| _ -> (rt, BucketFull (entryPeerId lrs))
               _         -> (rt, BucketFull (entryPeerId entry)) -- should not happen

-- | Remove a peer from the routing table.
removePeer :: PeerId -> RoutingTable -> RoutingTable
removePeer pid rt =
  let key = peerIdToKey pid
      idx = bucketIndex (rtSelfKey rt) key
      bucket = IntMap.findWithDefault emptyBucket idx (rtBuckets rt)
      entries = bucketEntries bucket
  in case findEntryIndex pid entries of
       Nothing -> rt  -- not found, no-op
       Just i ->
         let entries' = Seq.deleteAt i entries
             buckets' = if Seq.null entries'
                        then IntMap.delete idx (rtBuckets rt)
                        else IntMap.insert idx (KBucket entries') (rtBuckets rt)
         in rt { rtBuckets = buckets' }

-- | Find the n closest peers to a target key, sorted by XOR distance.
-- Searches across all buckets.
closestPeers :: DHTKey -> Int -> RoutingTable -> [BucketEntry]
closestPeers target n rt =
  let allEntries = concatMap (toList . bucketEntries) (IntMap.elems (rtBuckets rt))
      sorted = sortByDistance target allEntries
  in take n sorted
  where
    toList = foldr (:) []

-- | Compute the bucket index for a peer key relative to the local key.
-- This is the common prefix length (0-255).
bucketForPeer :: DHTKey -> RoutingTable -> Int
bucketForPeer key rt =
  bucketIndex (rtSelfKey rt) key

-- | Get the number of entries in a specific bucket.
bucketSize :: Int -> RoutingTable -> Int
bucketSize idx rt =
  case IntMap.lookup idx (rtBuckets rt) of
    Nothing -> 0
    Just bucket -> Seq.length (bucketEntries bucket)

-- | Get all peers across all buckets.
allPeers :: RoutingTable -> [BucketEntry]
allPeers rt =
  concatMap (toList . bucketEntries) (IntMap.elems (rtBuckets rt))
  where
    toList = foldr (:) []

-- Internal helpers

-- | Compute bucket index: common prefix length, clamped to [0, numBuckets-1].
bucketIndex :: DHTKey -> DHTKey -> Int
bucketIndex selfKey peerKey =
  let cpl = commonPrefixLength selfKey peerKey
  in min cpl (numBuckets - 1)

-- | Find the index of a peer in a sequence by PeerId.
findEntryIndex :: PeerId -> Seq BucketEntry -> Maybe Int
findEntryIndex pid entries =
  Seq.findIndexL (\e -> entryPeerId e == pid) entries
