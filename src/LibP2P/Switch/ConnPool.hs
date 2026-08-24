-- | STM-based connection pool for the Switch.
--
-- Tracks active connections per peer. All operations are STM-safe
-- for concurrent access from dial, listen, and cleanup threads.
--
-- Connection identity uses TVar pointer equality (connState field)
-- since Connection contains function fields that prevent deriving Eq.
module LibP2P.Switch.ConnPool
  ( newConnPool
  , lookupConn
  , lookupAllConns
  , addConn
  , removeConn
  , allConns
  ) where

import Control.Concurrent.STM (STM, TVar, newTVarIO, readTVar, writeTVar)
import qualified Data.Map.Strict as Map
import LibP2P.Crypto.PeerId (PeerId)
import LibP2P.Multiaddr (isRelayedAddr)
import LibP2P.Switch.Types (ConnState (..), Connection (..))

-- | Create a new empty connection pool.
newConnPool :: IO (TVar (Map.Map PeerId [Connection]))
newConnPool = newTVarIO Map.empty

-- | Look up the best Open connection for a peer, preferring a direct
-- connection over a relayed one.
--
-- specs/relay/DCUtR: after a hole punch "the peers should migrate to the
-- established connection by prioritizing over the existing relay
-- connection. All new streams should be opened in the direct
-- connection." Keeping that preference here means every caller migrates
-- at once, the way go-libp2p concentrates it in @bestConnToPeer@ ("If
-- one is limited and not the other, prefer the unlimited connection").
--
-- A relayed connection is still returned when it is all there is, so a
-- failed hole punch leaves the relay usable as before.
--
-- Returns Nothing if no connection exists or none are in ConnOpen state.
lookupConn :: TVar (Map.Map PeerId [Connection]) -> PeerId -> STM (Maybe Connection)
lookupConn poolVar pid = do
  pool <- readTVar poolVar
  case Map.lookup pid pool of
    Nothing -> pure Nothing
    Just conns -> pickBest conns Nothing
  where
    -- Walk once, returning the first direct connection and remembering
    -- the first relayed one as the fallback.
    pickBest [] fallback = pure fallback
    pickBest (c : rest) fallback = do
      st <- readTVar (connState c)
      if st /= ConnOpen
        then pickBest rest fallback
        else if isRelayedAddr (connRemoteAddr c)
          then pickBest rest (maybe (Just c) Just fallback)
          else pure (Just c)

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
