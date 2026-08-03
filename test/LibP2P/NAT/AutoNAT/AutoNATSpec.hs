module LibP2P.NAT.AutoNAT.AutoNATSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (newTQueueIO, atomically, writeTQueue, readTQueue, TQueue)
import Data.Word (Word8)
import Data.IORef (newIORef, readIORef, modifyIORef')
import LibP2P.NAT.AutoNAT.Message
import LibP2P.NAT.AutoNAT
import LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import LibP2P.Multiaddr (Multiaddr (..))
import LibP2P.Multiaddr.Protocol (Protocol (..))
import LibP2P.Multiaddr (toBytes)
import LibP2P.Crypto.PeerId (PeerId (..))

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

-- | IPv6 host 2001:db8::1 (16 bytes).
ip6HostA :: BS.ByteString
ip6HostA = BS.pack [0x20, 0x01, 0x0d, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]

-- | IPv6 host 2001:db8::2 (16 bytes).
ip6HostB :: BS.ByteString
ip6HostB = BS.pack [0x20, 0x01, 0x0d, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2]

-- | Remote observed IPv6 address: /ip6/2001:db8::1/tcp/12345
ipv6ObservedAddr :: Multiaddr
ipv6ObservedAddr = Multiaddr [IP6 ip6HostA, TCP 12345]

-- | Candidate matching the observed IPv6 host: /ip6/2001:db8::1/tcp/4001
ipv6MatchAddr :: Multiaddr
ipv6MatchAddr = Multiaddr [IP6 ip6HostA, TCP 4001]

-- | Candidate with a different IPv6 host: /ip6/2001:db8::2/tcp/4001
ipv6OtherAddr :: Multiaddr
ipv6OtherAddr = Multiaddr [IP6 ip6HostB, TCP 4001]

-- | Observed address without any IP component: /dns/example.com/tcp/4001
dnsObservedAddr :: Multiaddr
dnsObservedAddr = Multiaddr [DNS (T.pack "example.com"), TCP 4001]

-- | Build a DIAL message claiming the given peer id and addresses.
mkDialMsg :: PeerId -> [Multiaddr] -> AutoNATMessage
mkDialMsg (PeerId pidBytes) addrs = AutoNATMessage
  { anMsgType = Just DIAL
  , anMsgDial = Just AutoNATDial
      { anDialPeer = Just AutoNATPeerInfo
          { anPeerId = pidBytes
          , anAddrs = map toBytes addrs
          }
      }
  , anMsgDialResponse = Nothing
  }

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
                    { anPeerId = let PeerId bs = remotePeerId in bs
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
                    { anPeerId = let PeerId bs = remotePeerId in bs
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
                    { anPeerId = let PeerId bs = remotePeerId in bs
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
                    { anPeerId = let PeerId bs = remotePeerId in bs
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

    it "dials back only candidates matching the observed IPv6 host" $ do
      dialedRef <- newIORef ([] :: [[Multiaddr]])
      (clientStream, serverStream) <- mkStreamPair
      let config = AutoNATConfig
            { natThreshold = 3
            , natDialBack = \_pid addrs -> do
                modifyIORef' dialedRef (addrs :)
                pure (Right ())
            }
          -- Same IPv6 host, different IPv6 host, and an IPv4 address
          dialMsg = mkDialMsg remotePeerId [ipv6MatchAddr, ipv6OtherAddr, publicAddr]
      writeAutoNATMessage clientStream dialMsg
      handleAutoNAT config serverStream remotePeerId ipv6ObservedAddr
      _ <- readAutoNATMessage clientStream maxAutoNATMessageSize
      dialed <- readIORef dialedRef
      case dialed of
        [addrs] -> addrs `shouldBe` [ipv6MatchAddr]
        _ -> expectationFailure $ "Expected 1 dial-back call, got " ++ show (length dialed)

    it "responds E_DIAL_REFUSED when no candidate matches the observed IP" $ do
      dialedRef <- newIORef ([] :: [[Multiaddr]])
      (clientStream, serverStream) <- mkStreamPair
      let config = AutoNATConfig
            { natThreshold = 3
            , natDialBack = \_pid addrs -> do
                modifyIORef' dialedRef (addrs :)
                pure (Right ())
            }
          -- Observed over IPv6, but only IPv4 candidates are offered
          dialMsg = mkDialMsg remotePeerId [publicAddr, privateAddr]
      writeAutoNATMessage clientStream dialMsg
      handleAutoNAT config serverStream remotePeerId ipv6ObservedAddr
      result <- readAutoNATMessage clientStream maxAutoNATMessageSize
      case result of
        Right resp ->
          case anMsgDialResponse resp of
            Just dr -> anRespStatus dr `shouldBe` Just EDialRefused
            Nothing -> expectationFailure "Expected DialResponse"
        Left err -> expectationFailure $ "Read failed: " ++ err
      dialed <- readIORef dialedRef
      dialed `shouldBe` []

    it "responds E_DIAL_REFUSED when the observed address has no IP component" $ do
      dialedRef <- newIORef ([] :: [[Multiaddr]])
      (clientStream, serverStream) <- mkStreamPair
      let config = AutoNATConfig
            { natThreshold = 3
            , natDialBack = \_pid addrs -> do
                modifyIORef' dialedRef (addrs :)
                pure (Right ())
            }
          dialMsg = mkDialMsg remotePeerId [publicAddr, ipv6MatchAddr]
      writeAutoNATMessage clientStream dialMsg
      -- Observed address carries no IP: the filter must drop everything,
      -- never fall through to dialling all requested addresses
      handleAutoNAT config serverStream remotePeerId dnsObservedAddr
      result <- readAutoNATMessage clientStream maxAutoNATMessageSize
      case result of
        Right resp ->
          case anMsgDialResponse resp of
            Just dr -> anRespStatus dr `shouldBe` Just EDialRefused
            Nothing -> expectationFailure "Expected DialResponse"
        Left err -> expectationFailure $ "Read failed: " ++ err
      dialed <- readIORef dialedRef
      dialed `shouldBe` []

    it "responds E_BAD_REQUEST when the claimed peer id differs from the connected peer" $ do
      dialedRef <- newIORef ([] :: [[Multiaddr]])
      (clientStream, serverStream) <- mkStreamPair
      let config = AutoNATConfig
            { natThreshold = 3
            , natDialBack = \_pid addrs -> do
                modifyIORef' dialedRef (addrs :)
                pure (Right ())
            }
          -- Message claims testPeerId, but the connection is authenticated as remotePeerId
          dialMsg = mkDialMsg testPeerId [publicAddr]
      writeAutoNATMessage clientStream dialMsg
      handleAutoNAT config serverStream remotePeerId remoteObservedAddr
      result <- readAutoNATMessage clientStream maxAutoNATMessageSize
      case result of
        Right resp ->
          case anMsgDialResponse resp of
            Just dr -> anRespStatus dr `shouldBe` Just EBadRequest
            Nothing -> expectationFailure "Expected DialResponse"
        Left err -> expectationFailure $ "Read failed: " ++ err
      dialed <- readIORef dialedRef
      dialed `shouldBe` []

    it "passes the authenticated peer id to the dial-back, not a value from the message body" $ do
      dialedPidsRef <- newIORef ([] :: [PeerId])
      (clientStream, serverStream) <- mkStreamPair
      let config = AutoNATConfig
            { natThreshold = 3
            , natDialBack = \pid _addrs -> do
                modifyIORef' dialedPidsRef (pid :)
                pure (Right ())
            }
          dialMsg = mkDialMsg remotePeerId [publicAddr]
      writeAutoNATMessage clientStream dialMsg
      handleAutoNAT config serverStream remotePeerId remoteObservedAddr
      _ <- readAutoNATMessage clientStream maxAutoNATMessageSize
      dialedPids <- readIORef dialedPidsRef
      -- The dial-back target identity must be the peer authenticated on the
      -- connection (which, after the mismatch check, is the only value that
      -- can reach this point).
      dialedPids `shouldBe` [remotePeerId]

    it "includes the successfully dialled address in the OK response" $ do
      (clientStream, serverStream) <- mkStreamPair
      let config = AutoNATConfig
            { natThreshold = 3
            , natDialBack = \_pid _addrs -> pure (Right ())
            }
          -- privateAddr fails the observed-IP filter; publicAddr passes
          dialMsg = mkDialMsg remotePeerId [privateAddr, publicAddr]
      writeAutoNATMessage clientStream dialMsg
      handleAutoNAT config serverStream remotePeerId remoteObservedAddr
      result <- readAutoNATMessage clientStream maxAutoNATMessageSize
      case result of
        Right resp ->
          case anMsgDialResponse resp of
            Just dr -> do
              anRespStatus dr `shouldBe` Just StatusOK
              -- Spec (autonat): on success the response SHOULD carry the
              -- address that was successfully dialled.
              anRespAddr dr `shouldBe` Just (toBytes publicAddr)
            Nothing -> expectationFailure "Expected DialResponse"
        Left err -> expectationFailure $ "Read failed: " ++ err

    it "rejects a relayed observed address with a trailing p2p-circuit component" $ do
      dialedRef <- newIORef ([] :: [[Multiaddr]])
      (clientStream, serverStream) <- mkStreamPair
      let config = AutoNATConfig
            { natThreshold = 3
            , natDialBack = \_pid addrs -> do
                modifyIORef' dialedRef (addrs :)
                pure (Right ())
            }
          -- P2PCircuit in the last position (the existing relayed-address
          -- test places it mid-list)
          trailingRelayed = Multiaddr [IP4 0xCB007101, TCP 4001, P2P (BS.pack [1,2,3]), P2PCircuit]
          dialMsg = mkDialMsg remotePeerId [publicAddr]
      writeAutoNATMessage clientStream dialMsg
      handleAutoNAT config serverStream remotePeerId trailingRelayed
      result <- readAutoNATMessage clientStream maxAutoNATMessageSize
      case result of
        Right resp ->
          case anMsgDialResponse resp of
            Just dr -> anRespStatus dr `shouldBe` Just EDialRefused
            Nothing -> expectationFailure "Expected DialResponse"
        Left err -> expectationFailure $ "Read failed: " ++ err
      dialed <- readIORef dialedRef
      dialed `shouldBe` []

    it "caps the number of dial-back addresses per request" $ do
      dialedRef <- newIORef ([] :: [[Multiaddr]])
      (clientStream, serverStream) <- mkStreamPair
      let config = AutoNATConfig
            { natThreshold = 3
            , natDialBack = \_pid addrs -> do
                modifyIORef' dialedRef (addrs :)
                pure (Right ())
            }
          -- Many same-IP addresses on different ports; all pass the IP filter
          manyAddrs = [Multiaddr [IP4 0xCB007105, TCP p] | p <- [4001 .. 4064]]
          dialMsg = mkDialMsg remotePeerId manyAddrs
      writeAutoNATMessage clientStream dialMsg
      handleAutoNAT config serverStream remotePeerId remoteObservedAddr
      _ <- readAutoNATMessage clientStream maxAutoNATMessageSize
      dialed <- readIORef dialedRef
      case dialed of
        [addrs] -> length addrs `shouldBe` maxDialBackAddrs
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
                    { anPeerId = let PeerId bs = remotePeerId in bs
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
