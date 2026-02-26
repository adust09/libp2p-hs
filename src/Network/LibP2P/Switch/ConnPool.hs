-- | STM-based connection pool for the Switch.
--
-- Tracks active connections per peer. All operations are STM-safe
-- for concurrent access from dial, listen, and cleanup threads.
module Network.LibP2P.Switch.ConnPool
  ( newConnPool
  , lookupConn
  , lookupAllConns
  , addConn
  , removeConn
  , allConns
  ) where

import Control.Concurrent.STM (STM, TVar, newTVarIO)
import qualified Data.Map.Strict as Map
import Network.LibP2P.Crypto.PeerId (PeerId)
import Network.LibP2P.Switch.Types (Connection (..))

-- | Create a new empty connection pool.
newConnPool :: IO (TVar (Map.Map PeerId [Connection]))
newConnPool = newTVarIO Map.empty

-- | Look up the first Open connection for a peer.
lookupConn :: TVar (Map.Map PeerId [Connection]) -> PeerId -> STM (Maybe Connection)
lookupConn = error "lookupConn: not yet implemented"

-- | Look up all connections for a peer (any state).
lookupAllConns :: TVar (Map.Map PeerId [Connection]) -> PeerId -> STM [Connection]
lookupAllConns = error "lookupAllConns: not yet implemented"

-- | Add a connection to the pool.
addConn :: TVar (Map.Map PeerId [Connection]) -> Connection -> STM ()
addConn = error "addConn: not yet implemented"

-- | Remove a specific connection from the pool (by reference equality on TVar).
removeConn :: TVar (Map.Map PeerId [Connection]) -> Connection -> STM ()
removeConn = error "removeConn: not yet implemented"

-- | Get all connections across all peers.
allConns :: TVar (Map.Map PeerId [Connection]) -> STM [Connection]
allConns = error "allConns: not yet implemented"
