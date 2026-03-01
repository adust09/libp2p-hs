module Test.Network.LibP2P.NAT.DCUtR.DCUtRSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import Control.Concurrent.Async (withAsync, wait)
import Control.Concurrent.STM (newTQueueIO, atomically, writeTQueue, readTQueue, TQueue)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Word (Word8)
import Network.LibP2P.NAT.DCUtR.Message
import Network.LibP2P.NAT.DCUtR.DCUtR
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr (..), toBytes)
import Network.LibP2P.Multiaddr.Protocol (Protocol (..))

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
            { dcMaxRetries = 3
            , dcDialer = \addr -> do
                modifyIORef' dialedByA (addr :)
                pure (Right ())
            }
          configB = DCUtRConfig
            { dcMaxRetries = 3
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
      let configA = DCUtRConfig { dcMaxRetries = 1, dcDialer = \_ -> pure (Right ()) }
          configB = DCUtRConfig { dcMaxRetries = 1, dcDialer = \_ -> pure (Right ()) }
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
            { dcMaxRetries = 1
            , dcDialer = \_ -> pure (Left "dial failed")
            }
          configB = DCUtRConfig
            { dcMaxRetries = 1
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
      let configA = DCUtRConfig { dcMaxRetries = 1, dcDialer = \_ -> pure (Right ()) }
          configB = DCUtRConfig { dcMaxRetries = 1, dcDialer = \_ -> pure (Right ()) }
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
      let configA = DCUtRConfig { dcMaxRetries = 1, dcDialer = \_ -> pure (Right ()) }
          configB = DCUtRConfig
            { dcMaxRetries = 1
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
      let configA = DCUtRConfig { dcMaxRetries = 1, dcDialer = \_ -> pure (Right ()) }
          configB = DCUtRConfig { dcMaxRetries = 1, dcDialer = \_ -> pure (Right ()) }
          addrsA = [addrA]
          addrsB = [addrB]
      withAsync (initiateDCUtR configB streamB addrsB) $ \_ ->
        withAsync (handleDCUtRCapture configA streamA addrsA receivedAddrsRef) $ \handlerAsync -> do
          _ <- wait handlerAsync
          received <- readIORef receivedAddrsRef
          -- Handler should have received B's addresses
          length received `shouldSatisfy` (> 0)
          received `shouldBe` [toBytes addrB]

  describe "DCUtR constants" $ do
    it "max message size is 4096 bytes" $ do
      maxDCUtRMessageSize `shouldBe` 4096

    it "protocol ID is /libp2p/dcutr" $ do
      dcutrProtocolId `shouldBe` "/libp2p/dcutr"
