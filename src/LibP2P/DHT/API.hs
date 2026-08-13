-- | High-level DHT API for content publishing and retrieval.
--
-- Provides the user-facing operations that libp2p applications use:
-- 'provide', 'putValue', and 'findProviders'. These compose the lower-level
-- iterative lookups ('iterativeFindNode', 'iterativeGetProviders') with
-- direct RPC sending to the closest peers found.
module LibP2P.DHT.API
  ( -- * Content provider operations
    provide
  , putValue
  , findProviders
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.Text as T
import LibP2P.Crypto.PeerId (PeerId (..), peerIdBytes)
import LibP2P.DHT
  ( DHTNode (..)
  , ProviderEntry (..)
  , Validator (..)
  , addProvider
  , storeRecord
  )
import LibP2P.DHT.Lookup (iterativeFindNode, iterativeGetProviders)
import LibP2P.DHT.Message
import LibP2P.DHT.Types (BucketEntry (..), ConnectionType (..), entryPeerId, kValue)
import LibP2P.Multiaddr (Multiaddr, toBytes)

-- | Announce that the local node provides a given content key.
--
-- Performs an iterative FIND_NODE lookup for the content key, then sends
-- ADD_PROVIDER messages to the k closest peers found. The local peer's
-- listen addresses are included so remote peers can dial back.
provide :: DHTNode -> [Multiaddr] -> ByteString -> IO ()
provide node addrs key = do
  -- Find the k closest peers to this content key via iterative lookup
  closest <- iterativeFindNode node (PeerId key)

  -- Send ADD_PROVIDER to each of the k closest
  let providerPeer = DHTPeer (peerIdBytes (dhtLocalPeerId node))
                             (map toBytes addrs)
                             Connected
      providerMsg = emptyDHTMessage
        { msgType = AddProvider
        , msgKey  = key
        , msgProviderPeers = [providerPeer]
        }

  void $ mapConcurrently (\entry ->
    (dhtSendRequest node) (entryPeerId entry) providerMsg
      `catch` (\(_ :: SomeException) -> pure (Left "send failed"))
    ) (take kValue closest)

  -- Also store locally
  now <- getCurrentTime
  addProvider node key
    ProviderEntry
      { peProvider  = dhtLocalPeerId node
      , peAddrs     = addrs
      , peTimestamp = now
      }

-- | Store a value in the DHT under the given key.
--
-- Performs an iterative FIND_NODE lookup for the key, then sends
-- PUT_VALUE messages to the k closest peers. Also stores the value
-- locally. The value is validated using the node's configured validator;
-- if validation fails, the function returns 'Left' with an error message.
putValue :: DHTNode -> Validator -> ByteString -> ByteString -> IO (Either String ())
putValue node validator key value = do
  -- Validate first
  case valValidate validator key value of
    Left err -> pure (Left $ "value validation failed: " ++ err)
    Right () -> do
      -- Find the k closest peers
      closest <- iterativeFindNode node (PeerId key)

      -- Create the record and send PUT_VALUE
      now <- getCurrentTime
      let record = DHTRecord key value (T.pack (iso8601Show now))
          putMsg = emptyDHTMessage
            { msgType = PutValue
            , msgKey  = key
            , msgRecord = Just record
            }

      void $ mapConcurrently (\entry ->
        (dhtSendRequest node) (entryPeerId entry) putMsg
          `catch` (\(_ :: SomeException) -> pure (Left "send failed"))
        ) (take kValue closest)

      -- Also store locally
      storeRecord node record
      pure (Right ())

-- | Find providers for a content key.
--
-- Convenience wrapper around 'iterativeGetProviders'.
findProviders :: DHTNode -> ByteString -> IO [ProviderEntry]
findProviders node key = iterativeGetProviders node key
