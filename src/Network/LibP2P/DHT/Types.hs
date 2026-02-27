-- | Core types for the Kademlia DHT.
--
-- Defines the DHT keyspace (256-bit SHA-256 keys), routing table entry
-- structure, and protocol constants (k=20, alpha=10).
module Network.LibP2P.DHT.Types
  ( DHTKey (..)
  , BucketEntry (..)
  , ConnectionType (..)
  , InsertResult (..)
  , kValue
  , alphaValue
  , numBuckets
  ) where

import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr)

-- | 256-bit key in the DHT keyspace (always exactly 32 bytes, SHA-256 output).
newtype DHTKey = DHTKey ByteString
  deriving (Eq, Ord, Show)

-- | Replication parameter: each k-bucket holds up to k peers.
kValue :: Int
kValue = 20

-- | Concurrency parameter for iterative lookups.
alphaValue :: Int
alphaValue = 10

-- | Number of buckets (one per bit of the 256-bit keyspace).
numBuckets :: Int
numBuckets = 256

-- | Connection status of a peer (from DHT protobuf spec).
data ConnectionType
  = NotConnected  -- ^ 0: no connection, no extra info
  | Connected     -- ^ 1: live connection
  | CanConnect    -- ^ 2: recently connected
  | CannotConnect -- ^ 3: recently failed to connect
  deriving (Show, Eq, Enum, Bounded)

-- | A single entry in a k-bucket.
data BucketEntry = BucketEntry
  { entryPeerId   :: !PeerId
  , entryKey      :: !DHTKey        -- ^ Cached SHA-256 of peer ID
  , entryAddrs    :: ![Multiaddr]
  , entryLastSeen :: !UTCTime
  , entryConnType :: !ConnectionType
  } deriving (Show, Eq)

-- | Result of attempting to insert a peer into the routing table.
data InsertResult
  = Inserted          -- ^ Peer was added to the bucket
  | Updated           -- ^ Existing peer was moved to tail (most recently seen)
  | BucketFull !PeerId -- ^ Bucket is full; returns LRS peer ID for pinging
  deriving (Show, Eq)
