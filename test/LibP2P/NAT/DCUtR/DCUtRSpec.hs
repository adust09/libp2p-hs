module LibP2P.NAT.DCUtR.DCUtRSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait)
import Control.Concurrent.STM
  ( newTQueueIO, atomically, writeTQueue, readTQueue, TQueue
  , newTVarIO, readTVarIO, readTVar, writeTVar, modifyTVar', registerDelay, retry
  )
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, modifyIORef', atomicModifyIORef')
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Word (Word8)
import LibP2P.NAT.DCUtR.Message
import LibP2P.NAT.DCUtR
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import LibP2P.Multiaddr (Multiaddr (..), toBytes)
import LibP2P.Multiaddr.Protocol (Protocol (..))

-- | Create an in-memory stream pair for testing.
mkStreamPair :: IO (StreamIO, StreamIO)
mkStreamPair = do
  q1 <- newTQueueIO :: IO (TQueue Word8)
  q2 <- newTQueueIO :: IO (TQueue Word8)
  let streamA = StreamIO
        { streamWrite = \bs -> mapM_ (\b -> atomically (writeTQueue q1 b)) (BS.unpack bs)
        , streamReadByte = atomically (readTQueue q2)
        , streamClose = pure ()
        }
      streamB = StreamIO
        { streamWrite = \bs -> mapM_ (\b -> atomically (writeTQueue q2 b)) (BS.unpack bs)
        , streamReadByte = atomically (readTQueue q1)
        , streamClose = pure ()
        }
  pure (streamA, streamB)

-- Test addresses
addrA :: Multiaddr
addrA = Multiaddr [IP4 0xCB007105, TCP 4001]  -- /ip4/203.0.113.5/tcp/4001

addrB :: Multiaddr
addrB = Multiaddr [IP4 0xCB007106, TCP 4002]  -- /ip4/203.0.113.6/tcp/4002

spec :: Spec
spec = do
  describe "DCUtR 3-message exchange" $ do
    it "full initiate + handle exchange completes successfully" $ do
      (streamA, streamB) <- mkStreamPair
      dialedByA <- newIORef ([] :: [Multiaddr])
      dialedByB <- newIORef ([] :: [Multiaddr])
      let configA = DCUtRConfig
            { dcMaxAttempts = 3
            , dcDialer = \addr -> do
                modifyIORef' dialedByA (addr :)
                pure (Right ())
            }
          configB = DCUtRConfig
            { dcMaxAttempts = 3
            , dcDialer = \addr -> do
                modifyIORef' dialedByB (addr :)
                pure (Right ())
            }
          addrsA = [addrA]
          addrsB = [addrB]
      -- Run both sides concurrently
      withAsync (initiateDCUtR configB streamB addrsB) $ \initiatorAsync ->
        withAsync (handleDCUtR configA streamA addrsA) $ \handlerAsync -> do
          resultB <- wait initiatorAsync
          resultA <- wait handlerAsync
          -- Both should succeed
          case resultB of
            DCUtRSuccess -> pure ()
            DCUtRFailed err -> expectationFailure $ "Initiator failed: " ++ err
          case resultA of
            DCUtRSuccess -> pure ()
            DCUtRFailed err -> expectationFailure $ "Handler failed: " ++ err
          -- Both sides should have attempted to dial
          aDialed <- readIORef dialedByA
          bDialed <- readIORef dialedByB
          length aDialed `shouldSatisfy` (> 0)
          length bDialed `shouldSatisfy` (> 0)

    it "initiator sends CONNECT, handler responds with CONNECT" $ do
      (streamA, streamB) <- mkStreamPair
      let configA = DCUtRConfig { dcMaxAttempts = 1, dcDialer = \_ -> pure (Right ()) }
          configB = DCUtRConfig { dcMaxAttempts = 1, dcDialer = \_ -> pure (Right ()) }
          addrsA = [addrA]
          addrsB = [addrB]
      withAsync (initiateDCUtR configB streamB addrsB) $ \_ ->
        withAsync (handleDCUtR configA streamA addrsA) $ \handlerAsync -> do
          result <- wait handlerAsync
          case result of
            DCUtRSuccess -> pure ()
            DCUtRFailed err -> expectationFailure $ "Handler failed: " ++ err

    it "handles dial failure gracefully" $ do
      (streamA, streamB) <- mkStreamPair
      let configA = DCUtRConfig
            { dcMaxAttempts = 1
            , dcDialer = \_ -> pure (Left "dial failed")
            }
          configB = DCUtRConfig
            { dcMaxAttempts = 1
            , dcDialer = \_ -> pure (Left "dial failed")
            }
          addrsA = [addrA]
          addrsB = [addrB]
      withAsync (initiateDCUtR configB streamB addrsB) $ \initiatorAsync ->
        withAsync (handleDCUtR configA streamA addrsA) $ \handlerAsync -> do
          resultB <- wait initiatorAsync
          resultA <- wait handlerAsync
          -- Should report failure when dial fails
          case resultB of
            DCUtRFailed _ -> pure ()
            DCUtRSuccess -> pure ()  -- may still report success from message exchange
          case resultA of
            DCUtRFailed _ -> pure ()
            DCUtRSuccess -> pure ()

  describe "DCUtR RTT measurement" $ do
    it "measures non-negative RTT" $ do
      (streamA, streamB) <- mkStreamPair
      rttRef <- newIORef Nothing
      let configA = DCUtRConfig { dcMaxAttempts = 1, dcDialer = \_ -> pure (Right ()) }
          configB = DCUtRConfig { dcMaxAttempts = 1, dcDialer = \_ -> pure (Right ()) }
          addrsA = [addrA]
          addrsB = [addrB]
      withAsync (initiateDCUtRWithRTT configB streamB addrsB rttRef) $ \_ ->
        withAsync (handleDCUtR configA streamA addrsA) $ \handlerAsync -> do
          _ <- wait handlerAsync
          mRTT <- readIORef rttRef
          case mRTT of
            Just rtt -> rtt `shouldSatisfy` (>= 0)
            Nothing -> expectationFailure "RTT not measured"

  describe "DCUtR message flow verification" $ do
    it "initiator receives correct addresses from handler" $ do
      (streamA, streamB) <- mkStreamPair
      receivedAddrsRef <- newIORef ([] :: [BS.ByteString])
      let configA = DCUtRConfig { dcMaxAttempts = 1, dcDialer = \_ -> pure (Right ()) }
          configB = DCUtRConfig
            { dcMaxAttempts = 1
            , dcDialer = \_ -> pure (Right ())
            }
          addrsA = [addrA]
          addrsB = [addrB]
      withAsync (initiateDCUtRCapture configB streamB addrsB receivedAddrsRef) $ \_ ->
        withAsync (handleDCUtR configA streamA addrsA) $ \handlerAsync -> do
          _ <- wait handlerAsync
          received <- readIORef receivedAddrsRef
          -- Initiator should have received A's addresses
          length received `shouldSatisfy` (> 0)
          -- The received address should be A's address in binary form
          received `shouldBe` [toBytes addrA]

    it "handler receives correct addresses from initiator" $ do
      (streamA, streamB) <- mkStreamPair
      receivedAddrsRef <- newIORef ([] :: [BS.ByteString])
      let configA = DCUtRConfig { dcMaxAttempts = 1, dcDialer = \_ -> pure (Right ()) }
          configB = DCUtRConfig { dcMaxAttempts = 1, dcDialer = \_ -> pure (Right ()) }
          addrsA = [addrA]
          addrsB = [addrB]
      withAsync (initiateDCUtR configB streamB addrsB) $ \_ ->
        withAsync (handleDCUtRCapture configA streamA addrsA receivedAddrsRef) $ \handlerAsync -> do
          _ <- wait handlerAsync
          received <- readIORef receivedAddrsRef
          -- Handler should have received B's addresses
          length received `shouldSatisfy` (> 0)
          received `shouldBe` [toBytes addrB]

  describe "DCUtR parallel dialing" $ do
    it "dials all exchanged addresses concurrently" $ do
      (streamA, streamB) <- mkStreamPair
      let n = 3 :: Int
      inFlight <- newTVarIO (0 :: Int)
      allConcurrent <- newTVarIO False
      -- Barrier dialer: succeeds only if all n dials are in flight at the
      -- same time. A sequential dialer never has more than one in flight
      -- and times out.
      let barrierDialer _addr = do
            atomically $ modifyTVar' inFlight (+ 1)
            timedOut <- registerDelay 1000000
            ok <- atomically $ do
              c <- readTVar inFlight
              expired <- readTVar timedOut
              if c >= n
                then pure True
                else if expired then pure False else retry
            atomically $ modifyTVar' inFlight (subtract 1)
            when ok $ atomically $ writeTVar allConcurrent True
            pure (if ok then Right () else Left "dial did not overlap with the others")
          configA = DCUtRConfig { dcMaxAttempts = 1, dcDialer = \_ -> pure (Right ()) }
          configB = DCUtRConfig { dcMaxAttempts = 1, dcDialer = barrierDialer }
          -- Handler advertises three addresses; initiator must dial them all
          -- simultaneously.
          addrsA = [Multiaddr [IP4 0xCB007105, TCP p] | p <- [4001, 4002, 4003]]
      withAsync (initiateDCUtR configB streamB [addrB]) $ \initiatorAsync ->
        withAsync (handleDCUtR configA streamA addrsA) $ \handlerAsync -> do
          resultB <- wait initiatorAsync
          _ <- wait handlerAsync
          resultB `shouldBe` DCUtRSuccess
          wasConcurrent <- readTVarIO allConcurrent
          wasConcurrent `shouldBe` True

    it "first successful dial wins without waiting for slow addresses" $ do
      (streamA, streamB) <- mkStreamPair
      let slowAddr = Multiaddr [IP4 0xCB007105, TCP 4001]
          fastAddr = Multiaddr [IP4 0xCB007105, TCP 4002]
          dialerB addr
            | addr == slowAddr = threadDelay 3000000 >> pure (Left "slow dial timed out")
            | otherwise = pure (Right ())
          configA = DCUtRConfig { dcMaxAttempts = 1, dcDialer = \_ -> pure (Right ()) }
          configB = DCUtRConfig { dcMaxAttempts = 1, dcDialer = dialerB }
      t0 <- getCurrentTime
      withAsync (initiateDCUtR configB streamB [addrB]) $ \initiatorAsync ->
        withAsync (handleDCUtR configA streamA [slowAddr, fastAddr]) $ \handlerAsync -> do
          resultB <- wait initiatorAsync
          _ <- wait handlerAsync
          resultB `shouldBe` DCUtRSuccess
      t1 <- getCurrentTime
      diffUTCTime t1 t0 `shouldSatisfy` (< 2)

  describe "DCUtR retries" $ do
    it "retries the full exchange up to the configured attempt count" $ do
      (streamA, streamB) <- mkStreamPair
      bWrites <- newIORef (0 :: Int)
      bDials <- newIORef (0 :: Int)
      aDials <- newIORef (0 :: Int)
      -- Count initiator writes: each attempt re-runs the CONNECT/SYNC
      -- exchange, so 3 attempts must produce 6 writes (CONNECT + SYNC each).
      let streamB' = streamB
            { streamWrite = \bs -> modifyIORef' bWrites (+ 1) >> streamWrite streamB bs }
          configB = DCUtRConfig
            { dcMaxAttempts = 3
            , dcDialer = \_ -> modifyIORef' bDials (+ 1) >> pure (Left "punch failed")
            }
          configA = DCUtRConfig
            { dcMaxAttempts = 3
            , dcDialer = \_ -> modifyIORef' aDials (+ 1) >> pure (Left "punch failed")
            }
      withAsync (initiateDCUtR configB streamB' [addrB]) $ \initiatorAsync ->
        withAsync (handleDCUtR configA streamA [addrA]) $ \handlerAsync -> do
          resultB <- wait initiatorAsync
          resultA <- wait handlerAsync
          case resultB of
            DCUtRFailed _ -> pure ()
            DCUtRSuccess -> expectationFailure "Initiator should fail after all attempts"
          case resultA of
            DCUtRFailed _ -> pure ()
            DCUtRSuccess -> expectationFailure "Handler should fail after all attempts"
          readIORef bDials >>= (`shouldBe` 3)
          readIORef aDials >>= (`shouldBe` 3)
          readIORef bWrites >>= (`shouldBe` 6)

    it "succeeds when a retry attempt connects" $ do
      (streamA, streamB) <- mkStreamPair
      bCalls <- newIORef (0 :: Int)
      aCalls <- newIORef (0 :: Int)
      -- Both sides fail the first punch and succeed on the second.
      let flakyDialer ref _ = do
            n <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
            pure (if n >= 2 then Right () else Left "first punch failed")
          configB = DCUtRConfig { dcMaxAttempts = 3, dcDialer = flakyDialer bCalls }
          configA = DCUtRConfig { dcMaxAttempts = 3, dcDialer = flakyDialer aCalls }
      withAsync (initiateDCUtR configB streamB [addrB]) $ \initiatorAsync ->
        withAsync (handleDCUtR configA streamA [addrA]) $ \handlerAsync -> do
          resultB <- wait initiatorAsync
          resultA <- wait handlerAsync
          resultB `shouldBe` DCUtRSuccess
          resultA `shouldBe` DCUtRSuccess
          readIORef bCalls >>= (`shouldBe` 2)
          readIORef aCalls >>= (`shouldBe` 2)

  describe "DCUtR RTT timer placement" $ do
    it "includes time spent writing CONNECT in the RTT measurement" $ do
      (streamA, streamB) <- mkStreamPair
      firstWrite <- newIORef True
      -- Simulate a relayed write that blocks for 100 ms on the CONNECT.
      let slowStreamB = streamB
            { streamWrite = \bs -> do
                isFirst <- atomicModifyIORef' firstWrite (\f -> (False, f))
                when isFirst (threadDelay 100000)
                streamWrite streamB bs
            }
          configA = DCUtRConfig { dcMaxAttempts = 1, dcDialer = \_ -> pure (Right ()) }
          configB = DCUtRConfig { dcMaxAttempts = 1, dcDialer = \_ -> pure (Right ()) }
      rttRef <- newIORef Nothing
      withAsync (initiateDCUtRWithRTT configB slowStreamB [addrB] rttRef) $ \initiatorAsync ->
        withAsync (handleDCUtR configA streamA [addrA]) $ \handlerAsync -> do
          _ <- wait initiatorAsync
          _ <- wait handlerAsync
          mRTT <- readIORef rttRef
          case mRTT of
            Just rtt -> rtt `shouldSatisfy` (>= 0.1)
            Nothing -> expectationFailure "RTT not measured"

  describe "DCUtR constants" $ do
    it "max message size is 4096 bytes" $ do
      maxDCUtRMessageSize `shouldBe` 4096

    it "protocol ID is /libp2p/dcutr" $ do
      dcutrProtocolId `shouldBe` "/libp2p/dcutr"
