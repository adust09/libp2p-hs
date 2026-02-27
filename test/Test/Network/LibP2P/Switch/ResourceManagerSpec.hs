module Test.Network.LibP2P.Switch.ResourceManagerSpec (spec) where

import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Exception (SomeException, try)
import Network.LibP2P.Crypto.PeerId (PeerId (..))
import Network.LibP2P.Switch.ResourceManager
import Network.LibP2P.Switch.Types (Direction (..))
import Test.Hspec

-- | Test peer IDs.
peerA, peerB :: PeerId
peerA = PeerId "peer-a-rm"
peerB = PeerId "peer-b-rm"

-- | Create a DefaultLimits with small limits for testing.
testDefaults :: DefaultLimits
testDefaults = DefaultLimits
  { dlSystemLimits = ResourceLimits
      { rlMaxConnsInbound   = Just 4
      , rlMaxConnsOutbound  = Just 4
      , rlMaxConnsTotal     = Just 6
      , rlMaxStreamsInbound  = Just 8
      , rlMaxStreamsOutbound = Just 8
      , rlMaxStreamsTotal    = Just 12
      , rlMaxMemory         = Just (1024 * 1024)
      }
  , dlPeerLimits = ResourceLimits
      { rlMaxConnsInbound   = Just 2
      , rlMaxConnsOutbound  = Just 2
      , rlMaxConnsTotal     = Just 3
      , rlMaxStreamsInbound  = Just 4
      , rlMaxStreamsOutbound = Just 4
      , rlMaxStreamsTotal    = Just 6
      , rlMaxMemory         = Just (256 * 1024)
      }
  }

-- | Helper: reserve N connections and expect all to succeed.
reserveN :: ResourceManager -> PeerId -> Direction -> Int -> IO ()
reserveN rm pid dir n = mapM_ (\_ -> do
  result <- atomically $ reserveConnection rm pid dir
  result `shouldBe` Right ()
  ) [1..n]

spec :: Spec
spec = do
  -- ===== Group 1: Types (3 tests) =====
  describe "Types" $ do
    it "emptyUsage has all zeros" $ do
      emptyUsage `shouldBe` ResourceUsage 0 0 0 0 0

    it "defaultSystemLimits has sensible values" $ do
      rlMaxConnsTotal defaultSystemLimits `shouldBe` Just 512
      rlMaxStreamsTotal defaultSystemLimits `shouldBe` Just 8192
      rlMaxMemory defaultSystemLimits `shouldBe` Just (256 * 1024 * 1024)

    it "defaultPeerLimits has sensible values" $ do
      rlMaxConnsTotal defaultPeerLimits `shouldBe` Just 8
      rlMaxStreamsTotal defaultPeerLimits `shouldBe` Just 512
      rlMaxMemory defaultPeerLimits `shouldBe` Just (16 * 1024 * 1024)

  -- ===== Group 2: Single-scope reserve/release (9 tests) =====
  describe "Single-scope reserve/release" $ do
    it "reserveConnection succeeds within limits" $ do
      rm <- newResourceManager testDefaults
      result <- atomically $ reserveConnection rm peerA Inbound
      result `shouldBe` Right ()

    it "reserveConnection fails when inbound limit exceeded" $ do
      rm <- newResourceManager testDefaults
      -- Peer limit: 2 inbound
      reserveN rm peerA Inbound 2
      result <- atomically $ reserveConnection rm peerA Inbound
      case result of
        Left (ResourceLimitExceeded (PeerScope _) _) -> pure ()
        other -> expectationFailure $ "Expected PeerScope limit error, got: " ++ show other

    it "reserveConnection fails when outbound limit exceeded" $ do
      rm <- newResourceManager testDefaults
      -- Peer limit: 2 outbound
      reserveN rm peerA Outbound 2
      result <- atomically $ reserveConnection rm peerA Outbound
      case result of
        Left (ResourceLimitExceeded (PeerScope _) _) -> pure ()
        other -> expectationFailure $ "Expected PeerScope limit error, got: " ++ show other

    it "releaseConnection frees resources for reuse" $ do
      rm <- newResourceManager testDefaults
      -- Fill to limit
      reserveN rm peerA Inbound 2
      -- Release one
      atomically $ releaseConnection rm peerA Inbound
      -- Should succeed again
      result <- atomically $ reserveConnection rm peerA Inbound
      result `shouldBe` Right ()

    it "reserveStream succeeds within limits" $ do
      rm <- newResourceManager testDefaults
      peerScope <- atomically $ getOrCreatePeerScope rm peerA
      result <- atomically $ reserveStream peerScope Inbound
      result `shouldBe` Right ()

    it "reserveStream fails when limit exceeded" $ do
      rm <- newResourceManager testDefaults
      peerScope <- atomically $ getOrCreatePeerScope rm peerA
      -- Peer limit: 4 inbound streams
      mapM_ (\_ -> do
        r <- atomically $ reserveStream peerScope Inbound
        r `shouldBe` Right ()
        ) [1..4 :: Int]
      result <- atomically $ reserveStream peerScope Inbound
      case result of
        Left (ResourceLimitExceeded (PeerScope _) _) -> pure ()
        other -> expectationFailure $ "Expected PeerScope stream limit error, got: " ++ show other

    it "releaseStream frees resources for reuse" $ do
      rm <- newResourceManager testDefaults
      peerScope <- atomically $ getOrCreatePeerScope rm peerA
      -- Fill to limit
      mapM_ (\_ -> atomically $ reserveStream peerScope Outbound) [1..4 :: Int]
      -- Release one
      atomically $ releaseStream peerScope Outbound
      -- Should succeed again
      result <- atomically $ reserveStream peerScope Outbound
      result `shouldBe` Right ()

    it "release below zero is clamped to zero" $ do
      rm <- newResourceManager testDefaults
      -- Release without prior reserve should not go negative
      atomically $ releaseConnection rm peerA Inbound
      peerScope <- atomically $ getOrCreatePeerScope rm peerA
      usage <- atomically $ readTVar (rsUsage peerScope)
      ruConnsInbound usage `shouldBe` 0

    it "multiple reserves increment correctly" $ do
      rm <- newResourceManager testDefaults
      reserveN rm peerA Outbound 2
      peerScope <- atomically $ getOrCreatePeerScope rm peerA
      usage <- atomically $ readTVar (rsUsage peerScope)
      ruConnsOutbound usage `shouldBe` 2

  -- ===== Group 3: Hierarchical enforcement (4 tests) =====
  describe "Hierarchical enforcement" $ do
    it "reserve propagates to parent (system scope)" $ do
      rm <- newResourceManager testDefaults
      _ <- atomically $ reserveConnection rm peerA Inbound
      sysUsage <- atomically $ readTVar (rsUsage (rmSystemScope rm))
      ruConnsInbound sysUsage `shouldBe` 1

    it "parent limit blocks even when child has room" $ do
      -- System limit: 4 inbound total. Peer limit: 2 each.
      -- Use 2 peers × 2 inbound = 4, then 5th should fail at system level.
      rm <- newResourceManager testDefaults
      reserveN rm peerA Inbound 2
      reserveN rm peerB Inbound 2
      -- peerA still has room (peer limit 2, used 2). But system is full (4/4).
      -- Create peerC, which has fresh peer-level room but system is full
      let peerC = PeerId "peer-c-rm"
      result <- atomically $ reserveConnection rm peerC Inbound
      case result of
        Left (ResourceLimitExceeded SystemScope _) -> pure ()
        other -> expectationFailure $ "Expected SystemScope limit error, got: " ++ show other

    it "release propagates to parent" $ do
      rm <- newResourceManager testDefaults
      _ <- atomically $ reserveConnection rm peerA Outbound
      atomically $ releaseConnection rm peerA Outbound
      sysUsage <- atomically $ readTVar (rsUsage (rmSystemScope rm))
      ruConnsOutbound sysUsage `shouldBe` 0

    it "failed parent check does not leave phantom usage at child" $ do
      -- System limit: 4 inbound. Fill with 2 peers × 2 = 4.
      rm <- newResourceManager testDefaults
      reserveN rm peerA Inbound 2
      reserveN rm peerB Inbound 2
      -- peerC reserve should fail at system level
      let peerC = PeerId "peer-c-rm"
      result <- atomically $ reserveConnection rm peerC Inbound
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected limit error"
      -- peerC's scope should NOT have phantom usage
      peerCScope <- atomically $ getOrCreatePeerScope rm peerC
      usage <- atomically $ readTVar (rsUsage peerCScope)
      ruConnsInbound usage `shouldBe` 0

    it "three-level hierarchy (stream → peer → system)" $ do
      rm <- newResourceManager testDefaults
      peerScope <- atomically $ getOrCreatePeerScope rm peerA
      _ <- atomically $ reserveStream peerScope Inbound
      -- Check peer scope usage
      peerUsage <- atomically $ readTVar (rsUsage peerScope)
      ruStreamsInbound peerUsage `shouldBe` 1
      -- Check system scope usage
      sysUsage <- atomically $ readTVar (rsUsage (rmSystemScope rm))
      ruStreamsInbound sysUsage `shouldBe` 1

  -- ===== Group 4: Peer scope management (3 tests) =====
  describe "Peer scope management" $ do
    it "getOrCreatePeerScope creates new scope" $ do
      rm <- newResourceManager testDefaults
      scope <- atomically $ getOrCreatePeerScope rm peerA
      rsName scope `shouldBe` PeerScope peerA

    it "getOrCreatePeerScope returns existing scope" $ do
      rm <- newResourceManager testDefaults
      scope1 <- atomically $ getOrCreatePeerScope rm peerA
      _ <- atomically $ do
        usage <- readTVar (rsUsage scope1)
        writeTVar (rsUsage scope1) usage { ruConnsInbound = 99 }
      scope2 <- atomically $ getOrCreatePeerScope rm peerA
      usage2 <- atomically $ readTVar (rsUsage scope2)
      -- Should see the mutation from scope1
      ruConnsInbound usage2 `shouldBe` 99

    it "peer scope has correct parent" $ do
      rm <- newResourceManager testDefaults
      scope <- atomically $ getOrCreatePeerScope rm peerA
      case rsParent scope of
        Just parent -> rsName parent `shouldBe` SystemScope
        Nothing     -> expectationFailure "Expected peer scope to have SystemScope parent"

  -- ===== Group 5: Bracket patterns (4 tests) =====
  describe "Bracket patterns" $ do
    it "withConnection reserves and releases on success" $ do
      rm <- newResourceManager testDefaults
      withConnection rm peerA Inbound (pure ())
      -- After bracket, resources should be released
      peerScope <- atomically $ getOrCreatePeerScope rm peerA
      usage <- atomically $ readTVar (rsUsage peerScope)
      ruConnsInbound usage `shouldBe` 0

    it "withConnection releases on exception" $ do
      rm <- newResourceManager testDefaults
      _ <- try @SomeException $
        withConnection rm peerA Outbound (error "test exception")
      -- Resources should be released despite exception
      peerScope <- atomically $ getOrCreatePeerScope rm peerA
      usage <- atomically $ readTVar (rsUsage peerScope)
      ruConnsOutbound usage `shouldBe` 0

    it "withStream reserves and releases on success" $ do
      rm <- newResourceManager testDefaults
      peerScope <- atomically $ getOrCreatePeerScope rm peerA
      withStream peerScope Inbound (pure ())
      usage <- atomically $ readTVar (rsUsage peerScope)
      ruStreamsInbound usage `shouldBe` 0

    it "withStream releases on exception" $ do
      rm <- newResourceManager testDefaults
      peerScope <- atomically $ getOrCreatePeerScope rm peerA
      _ <- try @SomeException $
        withStream peerScope Outbound (error "test exception")
      usage <- atomically $ readTVar (rsUsage peerScope)
      ruStreamsOutbound usage `shouldBe` 0

  -- ===== Group 6: Direction-aware counting (3 tests) =====
  describe "Direction-aware counting" $ do
    it "inbound and outbound are tracked separately" $ do
      rm <- newResourceManager testDefaults
      _ <- atomically $ reserveConnection rm peerA Inbound
      _ <- atomically $ reserveConnection rm peerA Outbound
      peerScope <- atomically $ getOrCreatePeerScope rm peerA
      usage <- atomically $ readTVar (rsUsage peerScope)
      ruConnsInbound usage `shouldBe` 1
      ruConnsOutbound usage `shouldBe` 1

    it "total limit applies across both directions" $ do
      rm <- newResourceManager testDefaults
      -- Peer total limit: 3
      _ <- atomically $ reserveConnection rm peerA Inbound
      _ <- atomically $ reserveConnection rm peerA Inbound
      _ <- atomically $ reserveConnection rm peerA Outbound
      -- Now at 3 total (2 in + 1 out), should fail
      result <- atomically $ reserveConnection rm peerA Outbound
      case result of
        Left (ResourceLimitExceeded (PeerScope _) "total connections") -> pure ()
        other -> expectationFailure $ "Expected total limit error, got: " ++ show other

    it "release only affects the specified direction" $ do
      rm <- newResourceManager testDefaults
      _ <- atomically $ reserveConnection rm peerA Inbound
      _ <- atomically $ reserveConnection rm peerA Outbound
      atomically $ releaseConnection rm peerA Inbound
      peerScope <- atomically $ getOrCreatePeerScope rm peerA
      usage <- atomically $ readTVar (rsUsage peerScope)
      ruConnsInbound usage `shouldBe` 0
      ruConnsOutbound usage `shouldBe` 1

  -- ===== Group 7: Construction (2 tests) =====
  describe "Construction" $ do
    it "newResourceManager initializes with empty usage" $ do
      rm <- newResourceManager testDefaults
      usage <- atomically $ readTVar (rsUsage (rmSystemScope rm))
      usage `shouldBe` emptyUsage

    it "newResourceManager uses provided limits" $ do
      let custom = DefaultLimits
            { dlSystemLimits = noLimits { rlMaxConnsTotal = Just 99 }
            , dlPeerLimits   = noLimits { rlMaxConnsTotal = Just 10 }
            }
      rm <- newResourceManager custom
      rlMaxConnsTotal (rsLimits (rmSystemScope rm)) `shouldBe` Just 99
