-- | Hierarchical resource management for the Switch (docs/08-switch.md §Resource Limits).
--
-- Implements a scope tree (System → Peer → Connection → Stream) with
-- STM-based reserve/release that atomically walks leaf→root to enforce
-- per-scope limits. If any scope exceeds its limit, the entire
-- transaction rolls back.
module Network.LibP2P.Switch.ResourceManager
  ( -- * Direction
    Direction (..)
    -- * Resource tracking
  , ResourceUsage (..)
  , emptyUsage
    -- * Limits configuration
  , ResourceLimits (..)
  , noLimits
  , DefaultLimits (..)
  , defaultSystemLimits
  , defaultPeerLimits
    -- * Scope hierarchy
  , ResourceScope (..)
  , ScopeName (..)
  , ResourceError (..)
    -- * Resource manager
  , ResourceManager (..)
  , newResourceManager
    -- * Scope management
  , getOrCreatePeerScope
    -- * Reserve / release
  , reserveConnection
  , releaseConnection
  , reserveStream
  , releaseStream
    -- * Bracket patterns
  , withConnection
  , withStream
  ) where

import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar)
import Control.Exception (bracket_)
import Control.Concurrent.STM (atomically)
import qualified Data.Map.Strict as Map
import Network.LibP2P.Crypto.PeerId (PeerId)

-- | Direction of a connection relative to this node.
-- Defined here to avoid circular dependency with Switch.Types.
data Direction
  = Inbound   -- ^ Remote peer initiated the connection
  | Outbound  -- ^ Local node initiated the connection
  deriving (Show, Eq)

-- | Tracked resource usage at a single scope.
data ResourceUsage = ResourceUsage
  { ruConnsInbound   :: !Int
  , ruConnsOutbound  :: !Int
  , ruStreamsInbound  :: !Int
  , ruStreamsOutbound :: !Int
  , ruMemory         :: !Int
  } deriving (Show, Eq)

-- | Zero usage.
emptyUsage :: ResourceUsage
emptyUsage = ResourceUsage 0 0 0 0 0

-- | Configurable limits per scope. Nothing = unlimited.
data ResourceLimits = ResourceLimits
  { rlMaxConnsInbound   :: !(Maybe Int)
  , rlMaxConnsOutbound  :: !(Maybe Int)
  , rlMaxConnsTotal     :: !(Maybe Int)
  , rlMaxStreamsInbound  :: !(Maybe Int)
  , rlMaxStreamsOutbound :: !(Maybe Int)
  , rlMaxStreamsTotal    :: !(Maybe Int)
  , rlMaxMemory         :: !(Maybe Int)
  } deriving (Show, Eq)

-- | No limits (all Nothing).
noLimits :: ResourceLimits
noLimits = ResourceLimits Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | A node in the scope hierarchy.
data ResourceScope = ResourceScope
  { rsName   :: !ScopeName
  , rsUsage  :: !(TVar ResourceUsage)
  , rsLimits :: !ResourceLimits
  , rsParent :: !(Maybe ResourceScope)
  }

-- | Scope name for debugging and identification.
data ScopeName
  = SystemScope
  | PeerScope !PeerId
  | ConnectionScope
  | StreamScope
  deriving (Show, Eq)

-- | Resource limit violation error.
data ResourceError
  = ResourceLimitExceeded !ScopeName !String
  deriving (Show, Eq)

-- | Default limits for auto-created scopes.
data DefaultLimits = DefaultLimits
  { dlSystemLimits :: !ResourceLimits
  , dlPeerLimits   :: !ResourceLimits
  } deriving (Show, Eq)

-- | Sensible default system limits.
defaultSystemLimits :: ResourceLimits
defaultSystemLimits = ResourceLimits
  { rlMaxConnsInbound   = Just 256
  , rlMaxConnsOutbound  = Just 256
  , rlMaxConnsTotal     = Just 512
  , rlMaxStreamsInbound  = Just 4096
  , rlMaxStreamsOutbound = Just 4096
  , rlMaxStreamsTotal    = Just 8192
  , rlMaxMemory         = Just (256 * 1024 * 1024)  -- 256 MiB
  }

-- | Sensible default per-peer limits.
defaultPeerLimits :: ResourceLimits
defaultPeerLimits = ResourceLimits
  { rlMaxConnsInbound   = Just 4
  , rlMaxConnsOutbound  = Just 4
  , rlMaxConnsTotal     = Just 8
  , rlMaxStreamsInbound  = Just 256
  , rlMaxStreamsOutbound = Just 256
  , rlMaxStreamsTotal    = Just 512
  , rlMaxMemory         = Just (16 * 1024 * 1024)  -- 16 MiB
  }

-- | Top-level resource manager.
data ResourceManager = ResourceManager
  { rmSystemScope :: !ResourceScope
  , rmDefaults    :: !DefaultLimits
  , rmPeerScopes  :: !(TVar (Map.Map PeerId ResourceScope))
  }

-- | Create a new resource manager with the given default limits.
newResourceManager :: DefaultLimits -> IO ResourceManager
newResourceManager defaults = do
  usageVar <- atomically $ newTVar emptyUsage
  peersVar <- atomically $ newTVar Map.empty
  let systemScope = ResourceScope
        { rsName   = SystemScope
        , rsUsage  = usageVar
        , rsLimits = dlSystemLimits defaults
        , rsParent = Nothing
        }
  pure ResourceManager
    { rmSystemScope = systemScope
    , rmDefaults    = defaults
    , rmPeerScopes  = peersVar
    }

-- | Get or create a peer scope under the system scope.
getOrCreatePeerScope :: ResourceManager -> PeerId -> STM ResourceScope
getOrCreatePeerScope rm pid = do
  peers <- readTVar (rmPeerScopes rm)
  case Map.lookup pid peers of
    Just scope -> pure scope
    Nothing -> do
      usageVar <- newTVar emptyUsage
      let scope = ResourceScope
            { rsName   = PeerScope pid
            , rsUsage  = usageVar
            , rsLimits = dlPeerLimits (rmDefaults rm)
            , rsParent = Just (rmSystemScope rm)
            }
      writeTVar (rmPeerScopes rm) (Map.insert pid scope peers)
      pure scope

-- | Reserve a connection in the hierarchy (peer scope → system scope).
-- Returns Left on limit violation (STM auto-rolls back all increments).
reserveConnection :: ResourceManager -> PeerId -> Direction -> STM (Either ResourceError ())
reserveConnection rm pid dir = do
  peerScope <- getOrCreatePeerScope rm pid
  reserveConnInScope peerScope dir

-- | Release a connection in the hierarchy (peer scope → system scope).
releaseConnection :: ResourceManager -> PeerId -> Direction -> STM ()
releaseConnection rm pid dir = do
  peers <- readTVar (rmPeerScopes rm)
  case Map.lookup pid peers of
    Nothing -> pure ()
    Just peerScope -> releaseConnInScope peerScope dir

-- | Reserve a stream in the given scope (walks up to parent).
reserveStream :: ResourceScope -> Direction -> STM (Either ResourceError ())
reserveStream scope dir = reserveStreamInScope scope dir

-- | Release a stream in the given scope (walks up to parent).
releaseStream :: ResourceScope -> Direction -> STM ()
releaseStream scope dir = releaseStreamInScope scope dir

-- | Bracket: reserve connection, run action, release on exit (even on exception).
withConnection :: ResourceManager -> PeerId -> Direction -> IO a -> IO a
withConnection rm pid dir action =
  bracket_
    (atomically (reserveConnection rm pid dir) >>= either (fail . show) pure)
    (atomically (releaseConnection rm pid dir))
    action

-- | Bracket: reserve stream, run action, release on exit (even on exception).
withStream :: ResourceScope -> Direction -> IO a -> IO a
withStream scope dir action =
  bracket_
    (atomically (reserveStream scope dir) >>= either (fail . show) pure)
    (atomically (releaseStream scope dir))
    action

-- Internal: two-phase reserve for connections.
-- Phase 1: Walk leaf→root, compute new usages and check limits.
-- Phase 2: If all checks pass, commit all writes atomically.
-- This prevents phantom usage when a parent scope blocks.
reserveConnInScope :: ResourceScope -> Direction -> STM (Either ResourceError ())
reserveConnInScope scope dir = do
  proposed <- collectConnUpdates scope dir
  case proposed of
    Left err -> pure (Left err)
    Right updates -> do
      mapM_ (\(s, u) -> writeTVar (rsUsage s) u) updates
      pure (Right ())

-- Collect proposed (scope, newUsage) pairs from leaf to root.
-- Returns Left if any scope exceeds its limit.
collectConnUpdates :: ResourceScope -> Direction -> STM (Either ResourceError [(ResourceScope, ResourceUsage)])
collectConnUpdates scope dir = do
  usage <- readTVar (rsUsage scope)
  let (newUsage, checkField, dirName) = case dir of
        Inbound  -> (usage { ruConnsInbound = ruConnsInbound usage + 1 },
                     ruConnsInbound, "inbound connections")
        Outbound -> (usage { ruConnsOutbound = ruConnsOutbound usage + 1 },
                     ruConnsOutbound, "outbound connections")
      totalConns = ruConnsInbound newUsage + ruConnsOutbound newUsage
      limits = rsLimits scope
  case connDirLimit dir limits of
    Just lim | checkField newUsage > lim ->
      pure (Left (ResourceLimitExceeded (rsName scope) dirName))
    _ ->
      case rlMaxConnsTotal limits of
        Just lim | totalConns > lim ->
          pure (Left (ResourceLimitExceeded (rsName scope) "total connections"))
        _ -> case rsParent scope of
          Nothing -> pure (Right [(scope, newUsage)])
          Just parent -> do
            parentResult <- collectConnUpdates parent dir
            case parentResult of
              Left err -> pure (Left err)
              Right parentUpdates -> pure (Right ((scope, newUsage) : parentUpdates))
  where
    connDirLimit Inbound  = rlMaxConnsInbound
    connDirLimit Outbound = rlMaxConnsOutbound

-- Internal: release a connection at this scope and walk up to parent.
releaseConnInScope :: ResourceScope -> Direction -> STM ()
releaseConnInScope scope dir = do
  usage <- readTVar (rsUsage scope)
  let newUsage = case dir of
        Inbound  -> usage { ruConnsInbound = max 0 (ruConnsInbound usage - 1) }
        Outbound -> usage { ruConnsOutbound = max 0 (ruConnsOutbound usage - 1) }
  writeTVar (rsUsage scope) newUsage
  case rsParent scope of
    Nothing -> pure ()
    Just parent -> releaseConnInScope parent dir

-- Internal: two-phase reserve for streams (same pattern as connections).
reserveStreamInScope :: ResourceScope -> Direction -> STM (Either ResourceError ())
reserveStreamInScope scope dir = do
  proposed <- collectStreamUpdates scope dir
  case proposed of
    Left err -> pure (Left err)
    Right updates -> do
      mapM_ (\(s, u) -> writeTVar (rsUsage s) u) updates
      pure (Right ())

-- Collect proposed stream updates from leaf to root.
collectStreamUpdates :: ResourceScope -> Direction -> STM (Either ResourceError [(ResourceScope, ResourceUsage)])
collectStreamUpdates scope dir = do
  usage <- readTVar (rsUsage scope)
  let (newUsage, checkField, dirName) = case dir of
        Inbound  -> (usage { ruStreamsInbound = ruStreamsInbound usage + 1 },
                     ruStreamsInbound, "inbound streams")
        Outbound -> (usage { ruStreamsOutbound = ruStreamsOutbound usage + 1 },
                     ruStreamsOutbound, "outbound streams")
      totalStreams = ruStreamsInbound newUsage + ruStreamsOutbound newUsage
      limits = rsLimits scope
  case streamDirLimit dir limits of
    Just lim | checkField newUsage > lim ->
      pure (Left (ResourceLimitExceeded (rsName scope) dirName))
    _ ->
      case rlMaxStreamsTotal limits of
        Just lim | totalStreams > lim ->
          pure (Left (ResourceLimitExceeded (rsName scope) "total streams"))
        _ -> case rsParent scope of
          Nothing -> pure (Right [(scope, newUsage)])
          Just parent -> do
            parentResult <- collectStreamUpdates parent dir
            case parentResult of
              Left err -> pure (Left err)
              Right parentUpdates -> pure (Right ((scope, newUsage) : parentUpdates))
  where
    streamDirLimit Inbound  = rlMaxStreamsInbound
    streamDirLimit Outbound = rlMaxStreamsOutbound

-- Internal: release a stream at this scope and walk up to parent.
releaseStreamInScope :: ResourceScope -> Direction -> STM ()
releaseStreamInScope scope dir = do
  usage <- readTVar (rsUsage scope)
  let newUsage = case dir of
        Inbound  -> usage { ruStreamsInbound = max 0 (ruStreamsInbound usage - 1) }
        Outbound -> usage { ruStreamsOutbound = max 0 (ruStreamsOutbound usage - 1) }
  writeTVar (rsUsage scope) newUsage
  case rsParent scope of
    Nothing -> pure ()
    Just parent -> releaseStreamInScope parent dir
