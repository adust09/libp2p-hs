module Test.Network.LibP2P.NAT.AutoNAT.AutoNATSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (newTQueueIO, atomically, writeTQueue, readTQueue, TQueue)
import Data.Word (Word8)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Network.LibP2P.NAT.AutoNAT.Message
import Network.LibP2P.NAT.AutoNAT.AutoNAT
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr (..))
import Network.LibP2P.Multiaddr.Protocol (Protocol (..))
import Network.LibP2P.Multiaddr.Multiaddr (toBytes)
import Network.LibP2P.Crypto.PeerId (PeerId (..))

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

-- Test helpers

testPeerId :: PeerId
testPeerId = PeerId (BS.pack [0x00, 0x24, 0x08, 0x01, 0x12, 0x20, 0xAA, 0xBB, 0xCC, 0xDD])

remotePeerId :: PeerId
remotePeerId = PeerId (BS.pack [0x00, 0x24, 0x08, 0x01, 0x12, 0x20, 0x11, 0x22, 0x33, 0x44])

-- | Public IP address for testing: /ip4/203.0.113.5/tcp/4001
publicAddr :: Multiaddr
publicAddr = Multiaddr [IP4 0xCB007105, TCP 4001]

-- | Private IP address: /ip4/192.168.1.1/tcp/4001
privateAddr :: Multiaddr
privateAddr = Multiaddr [IP4 0xC0A80101, TCP 4001]

-- | Relayed address: /ip4/203.0.113.1/tcp/4001/p2p/<relay>/p2p-circuit/p2p/<target>
relayedAddr :: Multiaddr
relayedAddr = Multiaddr [IP4 0xCB007101, TCP 4001, P2P (BS.pack [1,2,3]), P2PCircuit, P2P (BS.pack [4,5,6])]

-- | Remote observed address: /ip4/203.0.113.5/tcp/12345
remoteObservedAddr :: Multiaddr
remoteObservedAddr = Multiaddr [IP4 0xCB007105, TCP 12345]

spec :: Spec
spec = do
  describe "AutoNAT handler (server side)" $ do
    it "responds OK when dial-back succeeds" $ do
      (clientStream, serverStream) <- mkStreamPair
      let config = AutoNATConfig
            { natThreshold = 3
            , natDialBack = \_pid _addrs -> pure (Right ())
            }
      -- Client sends DIAL request
      let addrBytes = toBytes publicAddr
          dialMsg = AutoNATMessage
            { anMsgType = Just DIAL
            , anMsgDial = Just AutoNATDial
                { anDialPeer = Just AutoNATPeerInfo
                    { anPeerId = let PeerId bs = testPeerId in bs
                    , anAddrs = [addrBytes]
                    }
                }
            , anMsgDialResponse = Nothing
            }
      writeAutoNATMessage clientStream dialMsg
      -- Server handles request
      handleAutoNAT config serverStream remotePeerId remoteObservedAddr
      -- Read response
      result <- readAutoNATMessage clientStream maxAutoNATMessageSize
      case result of
        Right resp -> do
          anMsgType resp `shouldBe` Just DIAL_RESPONSE
          case anMsgDialResponse resp of
            Just dr -> anRespStatus dr `shouldBe` Just StatusOK
            Nothing -> expectationFailure "Expected DialResponse"
        Left err -> expectationFailure $ "Read failed: " ++ err

    it "responds E_DIAL_ERROR when dial-back fails" $ do
      (clientStream, serverStream) <- mkStreamPair
      let config = AutoNATConfig
            { natThreshold = 3
            , natDialBack = \_pid _addrs -> pure (Left "connection refused")
            }
      let addrBytes = toBytes publicAddr
          dialMsg = AutoNATMessage
            { anMsgType = Just DIAL
            , anMsgDial = Just AutoNATDial
                { anDialPeer = Just AutoNATPeerInfo
                    { anPeerId = let PeerId bs = testPeerId in bs
                    , anAddrs = [addrBytes]
                    }
                }
            , anMsgDialResponse = Nothing
            }
      writeAutoNATMessage clientStream dialMsg
      handleAutoNAT config serverStream remotePeerId remoteObservedAddr
      result <- readAutoNATMessage clientStream maxAutoNATMessageSize
      case result of
        Right resp -> do
          anMsgType resp `shouldBe` Just DIAL_RESPONSE
          case anMsgDialResponse resp of
            Just dr -> anRespStatus dr `shouldBe` Just EDialError
            Nothing -> expectationFailure "Expected DialResponse"
        Left err -> expectationFailure $ "Read failed: " ++ err

    it "responds E_BAD_REQUEST when no addresses provided" $ do
      (clientStream, serverStream) <- mkStreamPair
      let config = AutoNATConfig
            { natThreshold = 3
            , natDialBack = \_pid _addrs -> pure (Right ())
            }
          dialMsg = AutoNATMessage
            { anMsgType = Just DIAL
            , anMsgDial = Just AutoNATDial
                { anDialPeer = Just AutoNATPeerInfo
                    { anPeerId = let PeerId bs = testPeerId in bs
                    , anAddrs = []
                    }
                }
            , anMsgDialResponse = Nothing
            }
      writeAutoNATMessage clientStream dialMsg
      handleAutoNAT config serverStream remotePeerId remoteObservedAddr
      result <- readAutoNATMessage clientStream maxAutoNATMessageSize
      case result of
        Right resp ->
          case anMsgDialResponse resp of
            Just dr -> anRespStatus dr `shouldBe` Just EBadRequest
            Nothing -> expectationFailure "Expected DialResponse"
        Left err -> expectationFailure $ "Read failed: " ++ err

    it "responds E_BAD_REQUEST when no peer info provided" $ do
      (clientStream, serverStream) <- mkStreamPair
      let config = AutoNATConfig
            { natThreshold = 3
            , natDialBack = \_pid _addrs -> pure (Right ())
            }
          dialMsg = AutoNATMessage
            { anMsgType = Just DIAL
            , anMsgDial = Just AutoNATDial { anDialPeer = Nothing }
            , anMsgDialResponse = Nothing
            }
      writeAutoNATMessage clientStream dialMsg
      handleAutoNAT config serverStream remotePeerId remoteObservedAddr
      result <- readAutoNATMessage clientStream maxAutoNATMessageSize
      case result of
        Right resp ->
          case anMsgDialResponse resp of
            Just dr -> anRespStatus dr `shouldBe` Just EBadRequest
            Nothing -> expectationFailure "Expected DialResponse"
        Left err -> expectationFailure $ "Read failed: " ++ err

    it "filters addresses to match observed IP" $ do
      -- Only addresses matching the remote's observed IP should be dialed
      dialedRef <- newIORef ([] :: [[Multiaddr]])
      (clientStream, serverStream) <- mkStreamPair
      let config = AutoNATConfig
            { natThreshold = 3
            , natDialBack = \_pid addrs -> do
                modifyIORef' dialedRef (addrs :)
                pure (Right ())
            }
          -- Provide both matching and non-matching addresses
          matchAddr = toBytes publicAddr           -- 203.0.113.5 matches remoteObservedAddr
          nonMatchAddr = toBytes privateAddr       -- 192.168.1.1 does NOT match
          dialMsg = AutoNATMessage
            { anMsgType = Just DIAL
            , anMsgDial = Just AutoNATDial
                { anDialPeer = Just AutoNATPeerInfo
                    { anPeerId = let PeerId bs = testPeerId in bs
                    , anAddrs = [matchAddr, nonMatchAddr]
                    }
                }
            , anMsgDialResponse = Nothing
            }
      writeAutoNATMessage clientStream dialMsg
      handleAutoNAT config serverStream remotePeerId remoteObservedAddr
      -- Read response to ensure handler completed
      _ <- readAutoNATMessage clientStream maxAutoNATMessageSize
      -- Check that dial-back only received matching addresses
      dialed <- readIORef dialedRef
      case dialed of
        [addrs] -> do
          length addrs `shouldBe` 1
          addrs `shouldBe` [publicAddr]
        _ -> expectationFailure $ "Expected 1 dial-back call, got " ++ show (length dialed)

    it "rejects requests from relayed connections" $ do
      (clientStream, serverStream) <- mkStreamPair
      let config = AutoNATConfig
            { natThreshold = 3
            , natDialBack = \_pid _addrs -> pure (Right ())
            }
          addrBytes = toBytes publicAddr
          dialMsg = AutoNATMessage
            { anMsgType = Just DIAL
            , anMsgDial = Just AutoNATDial
                { anDialPeer = Just AutoNATPeerInfo
                    { anPeerId = let PeerId bs = testPeerId in bs
                    , anAddrs = [addrBytes]
                    }
                }
            , anMsgDialResponse = Nothing
            }
      writeAutoNATMessage clientStream dialMsg
      -- Pass a relayed address as the observed address
      handleAutoNAT config serverStream remotePeerId relayedAddr
      result <- readAutoNATMessage clientStream maxAutoNATMessageSize
      case result of
        Right resp ->
          case anMsgDialResponse resp of
            Just dr -> anRespStatus dr `shouldBe` Just EDialRefused
            Nothing -> expectationFailure "Expected DialResponse"
        Left err -> expectationFailure $ "Read failed: " ++ err

  describe "AutoNAT client (requestAutoNAT)" $ do
    it "sends DIAL and receives OK response" $ do
      (clientStream, serverStream) <- mkStreamPair
      let localAddrs = [publicAddr]
      -- Simulate server in background: read request, write OK response
      let serverAction = do
            _ <- readAutoNATMessage serverStream maxAutoNATMessageSize
            let resp = AutoNATMessage
                  { anMsgType = Just DIAL_RESPONSE
                  , anMsgDial = Nothing
                  , anMsgDialResponse = Just AutoNATDialResponse
                      { anRespStatus = Just StatusOK
                      , anRespStatusText = Nothing
                      , anRespAddr = Just (toBytes publicAddr)
                      }
                  }
            writeAutoNATMessage serverStream resp
      withAsync serverAction $ \_ -> do
        result <- requestAutoNAT clientStream testPeerId localAddrs
        case result of
          Right dr -> anRespStatus dr `shouldBe` Just StatusOK
          Left err -> expectationFailure $ "requestAutoNAT failed: " ++ err

    it "sends DIAL and receives error response" $ do
      (clientStream, serverStream) <- mkStreamPair
      let localAddrs = [publicAddr]
      let serverAction = do
            _ <- readAutoNATMessage serverStream maxAutoNATMessageSize
            let resp = AutoNATMessage
                  { anMsgType = Just DIAL_RESPONSE
                  , anMsgDial = Nothing
                  , anMsgDialResponse = Just AutoNATDialResponse
                      { anRespStatus = Just EDialError
                      , anRespStatusText = Just "timeout"
                      , anRespAddr = Nothing
                      }
                  }
            writeAutoNATMessage serverStream resp
      withAsync serverAction $ \_ -> do
        result <- requestAutoNAT clientStream testPeerId localAddrs
        case result of
          Right dr -> anRespStatus dr `shouldBe` Just EDialError
          Left err -> expectationFailure $ "requestAutoNAT failed: " ++ err

  describe "AutoNAT probeNATStatus" $ do
    it "all peers report OK → NATPublic" $ do
      let results = replicate 4 (Right AutoNATDialResponse
            { anRespStatus = Just StatusOK
            , anRespStatusText = Nothing
            , anRespAddr = Just (toBytes publicAddr)
            })
      probeNATStatusPure 3 results `shouldBe` NATPublic

    it "all peers report error → NATPrivate" $ do
      let results = replicate 4 (Right AutoNATDialResponse
            { anRespStatus = Just EDialError
            , anRespStatusText = Just "timeout"
            , anRespAddr = Nothing
            })
      probeNATStatusPure 3 results `shouldBe` NATPrivate

    it "mixed results below threshold → NATUnknown" $ do
      let okResult = Right AutoNATDialResponse
            { anRespStatus = Just StatusOK, anRespStatusText = Nothing
            , anRespAddr = Just (toBytes publicAddr) }
          errResult = Right AutoNATDialResponse
            { anRespStatus = Just EDialError, anRespStatusText = Just "fail"
            , anRespAddr = Nothing }
          results = [okResult, errResult, okResult, errResult]
      probeNATStatusPure 3 results `shouldBe` NATUnknown

    it "threshold=3 with 3 OK → NATPublic" $ do
      let okResult = Right AutoNATDialResponse
            { anRespStatus = Just StatusOK, anRespStatusText = Nothing
            , anRespAddr = Just (toBytes publicAddr) }
          errResult = Right AutoNATDialResponse
            { anRespStatus = Just EDialError, anRespStatusText = Just "fail"
            , anRespAddr = Nothing }
          results = [okResult, okResult, okResult, errResult]
      probeNATStatusPure 3 results `shouldBe` NATPublic

    it "transport errors count as failures" $ do
      let results = replicate 4 (Left "stream closed" :: Either String AutoNATDialResponse)
      probeNATStatusPure 3 results `shouldBe` NATPrivate

    it "no results → NATUnknown" $ do
      probeNATStatusPure 3 [] `shouldBe` NATUnknown
