-- | STM-based connection pool for the Switch.
--
-- Tracks active connections per peer. All operations are STM-safe
-- for concurrent access from dial, listen, and cleanup threads.
--
-- Connection identity uses TVar pointer equality (connState field)
-- since Connection contains function fields that prevent deriving Eq.
module Network.LibP2P.Switch.ConnPool
  ( newConnPool
  , lookupConn
  , lookupAllConns
  , addConn
  , removeConn
  , allConns
  ) where

import Control.Concurrent.STM (STM, TVar, newTVarIO, readTVar, writeTVar)
import qualified Data.Map.Strict as Map
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.Switch.Types (ConnState (..), Connection (..))

-- | Create a new empty connection pool.
newConnPool :: IO (TVar (Map.Map PeerId [Connection]))
newConnPool = newTVarIO Map.empty

-- | Look up the first Open connection for a peer.
-- Returns Nothing if no connection exists or none are in ConnOpen state.
lookupConn :: TVar (Map.Map PeerId [Connection]) -> PeerId -> STM (Maybe Connection)
lookupConn poolVar pid = do
  pool <- readTVar poolVar
  case Map.lookup pid pool of
    Nothing -> pure Nothing
    Just conns -> findOpen conns
  where
    findOpen [] = pure Nothing
    findOpen (c : rest) = do
      st <- readTVar (connState c)
      if st == ConnOpen
        then pure (Just c)
        else findOpen rest

-- | Look up all connections for a peer (any state).
lookupAllConns :: TVar (Map.Map PeerId [Connection]) -> PeerId -> STM [Connection]
lookupAllConns poolVar pid = do
  pool <- readTVar poolVar
  pure $ Map.findWithDefault [] pid pool

-- | Add a connection to the pool, keyed by its peer ID.
addConn :: TVar (Map.Map PeerId [Connection]) -> Connection -> STM ()
addConn poolVar conn = do
  pool <- readTVar poolVar
  let pid = connPeerId conn
      conns = Map.findWithDefault [] pid pool
  writeTVar poolVar (Map.insert pid (conns ++ [conn]) pool)

-- | Remove a specific connection from the pool.
-- Uses TVar reference equality (connState) to identify the connection.
-- Removes empty entries from the map to prevent memory leaks.
removeConn :: TVar (Map.Map PeerId [Connection]) -> Connection -> STM ()
removeConn poolVar conn = do
  pool <- readTVar poolVar
  let pid = connPeerId conn
      targetState = connState conn
  case Map.lookup pid pool of
    Nothing -> pure ()
    Just conns -> do
      let remaining = filter (\c -> connState c /= targetState) conns
      if null remaining
        then writeTVar poolVar (Map.delete pid pool)
        else writeTVar poolVar (Map.insert pid remaining pool)

-- | Get all connections across all peers.
allConns :: TVar (Map.Map PeerId [Connection]) -> STM [Connection]
allConns poolVar = do
  pool <- readTVar poolVar
  pure $ concat (Map.elems pool)
