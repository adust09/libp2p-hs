-- | AutoNAT v1 service: detect NAT status by asking remote peers to dial back.
--
-- Protocol: /libp2p/autonat/1.0.0
-- Flow: Client sends DIAL with addresses, server dials back, responds with result.
--
-- Security rules (from docs/10-nat-traversal.md):
--   - Server MUST NOT dial addresses unless they match the requester's observed IP
--   - Server MUST NOT accept dial requests over relayed connections
module Network.LibP2P.NAT.AutoNAT.AutoNAT
  ( -- * Types
    NATStatus (..)
  , AutoNATConfig (..)
    -- * Server
  , handleAutoNAT
    -- * Client
  , requestAutoNAT
    -- * NAT status aggregation
  , probeNATStatusPure
  ) where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Word (Word32)
import Network.LibP2P.NAT.AutoNAT.Message
import Network.LibP2P.MultistreamSelect.Negotiation (StreamIO (..))
import Network.LibP2P.Multiaddr.Multiaddr (Multiaddr (..), toBytes, fromBytes)
import Network.LibP2P.Multiaddr.Protocol (Protocol (..))
import Network.LibP2P.Crypto.PeerId (PeerId (..))

-- | Detected NAT status.
data NATStatus = NATPublic | NATPrivate | NATUnknown
  deriving (Show, Eq)

-- | AutoNAT configuration.
data AutoNATConfig = AutoNATConfig
  { natThreshold :: !Int
    -- ^ Number of peers that must agree for a definitive result
  , natDialBack  :: !(PeerId -> [Multiaddr] -> IO (Either String ()))
    -- ^ Injectable dial-back function (for testing)
  }

-- | Server handler: receive DIAL, validate, dial back, respond.
--
-- Security:
--   - Rejects requests from relayed connections (P2PCircuit in observed addr)
--   - Filters dial-back addresses to match observed IP
handleAutoNAT :: AutoNATConfig -> StreamIO -> PeerId -> Multiaddr -> IO ()
handleAutoNAT config stream remotePeerId remoteObservedAddr = do
  result <- readAutoNATMessage stream maxAutoNATMessageSize
  case result of
    Left _err -> pure ()
    Right msg -> do
      resp <- processDialRequest config msg remotePeerId remoteObservedAddr
      writeAutoNATMessage stream resp

-- | Process a DIAL request and produce a response.
processDialRequest :: AutoNATConfig -> AutoNATMessage -> PeerId -> Multiaddr -> IO AutoNATMessage
processDialRequest config msg _remotePeerId remoteObservedAddr
  -- Reject requests from relayed connections
  | isRelayedAddr remoteObservedAddr = pure $ mkDialResponse EDialRefused (Just "relayed connection") Nothing
  | otherwise = case anMsgDial msg of
      Nothing -> pure $ mkDialResponse EBadRequest (Just "missing dial field") Nothing
      Just dial -> case anDialPeer dial of
        Nothing -> pure $ mkDialResponse EBadRequest (Just "missing peer info") Nothing
        Just peerInfo -> do
          let requestedAddrs = mapMaybe' fromBytes (anAddrs peerInfo)
              filteredAddrs = filterByObservedIP remoteObservedAddr requestedAddrs
          if null filteredAddrs
            then pure $ mkDialResponse EBadRequest (Just "no valid addresses") Nothing
            else do
              let peerId = PeerId (anPeerId peerInfo)
              dialResult <- natDialBack config peerId filteredAddrs
              case dialResult of
                Right () ->
                  let addrBytes = case filteredAddrs of
                        (a:_) -> Just (toBytes a)
                        []    -> Nothing
                  in pure $ mkDialResponse StatusOK Nothing addrBytes
                Left _err ->
                  pure $ mkDialResponse EDialError (Just "dial failed") Nothing

-- | Build a DIAL_RESPONSE message.
mkDialResponse :: ResponseStatus -> Maybe String -> Maybe ByteString -> AutoNATMessage
mkDialResponse status mText mAddr = AutoNATMessage
  { anMsgType = Just DIAL_RESPONSE
  , anMsgDial = Nothing
  , anMsgDialResponse = Just AutoNATDialResponse
      { anRespStatus = Just status
      , anRespStatusText = fmap T.pack mText
      , anRespAddr = mAddr
      }
  }

-- | Client: send DIAL with local addresses, receive response.
requestAutoNAT :: StreamIO -> PeerId -> [Multiaddr] -> IO (Either String AutoNATDialResponse)
requestAutoNAT stream localPeerId localAddrs = do
  let PeerId pidBytes = localPeerId
      dialMsg = AutoNATMessage
        { anMsgType = Just DIAL
        , anMsgDial = Just AutoNATDial
            { anDialPeer = Just AutoNATPeerInfo
                { anPeerId = pidBytes
                , anAddrs = map toBytes localAddrs
                }
            }
        , anMsgDialResponse = Nothing
        }
  writeAutoNATMessage stream dialMsg
  result <- readAutoNATMessage stream maxAutoNATMessageSize
  case result of
    Left err -> pure (Left err)
    Right resp -> case anMsgDialResponse resp of
      Nothing -> pure (Left "response missing dialResponse field")
      Just dr -> pure (Right dr)

-- | Pure aggregation of AutoNAT results into a NAT status.
-- Counts OK responses as "public" votes, all other results as "private" votes.
probeNATStatusPure :: Int -> [Either String AutoNATDialResponse] -> NATStatus
probeNATStatusPure _threshold [] = NATUnknown
probeNATStatusPure threshold results =
  let (okCount, failCount) = foldl' countResult (0 :: Int, 0 :: Int) results
  in if okCount >= threshold then NATPublic
     else if failCount >= threshold then NATPrivate
     else NATUnknown
  where
    countResult :: (Int, Int) -> Either String AutoNATDialResponse -> (Int, Int)
    countResult (ok, fail') (Left _) = (ok, fail' + 1)
    countResult (ok, fail') (Right dr) =
      case anRespStatus dr of
        Just StatusOK -> (ok + 1, fail')
        _             -> (ok, fail' + 1)

-- Helpers

-- | Check if a multiaddr contains P2PCircuit (indicating a relayed connection).
isRelayedAddr :: Multiaddr -> Bool
isRelayedAddr (Multiaddr ps) = any isCircuit ps
  where
    isCircuit P2PCircuit = True
    isCircuit _          = False

-- | Extract IP address (as Word32 for IPv4) from a multiaddr.
extractIP4 :: Multiaddr -> Maybe Word32
extractIP4 (Multiaddr ps) = go ps
  where
    go [] = Nothing
    go (IP4 addr : _) = Just addr
    go (_ : rest) = go rest

-- | Filter addresses to only those matching the observed IP.
filterByObservedIP :: Multiaddr -> [Multiaddr] -> [Multiaddr]
filterByObservedIP observed addrs =
  case extractIP4 observed of
    Nothing -> addrs  -- can't determine IP, pass all through
    Just obsIP -> filter (matchesIP obsIP) addrs
  where
    matchesIP :: Word32 -> Multiaddr -> Bool
    matchesIP obsIP addr =
      case extractIP4 addr of
        Just ip -> ip == obsIP
        Nothing -> False  -- non-IP4 addresses are filtered out

-- | mapMaybe for Either (keeping only Right values).
mapMaybe' :: (a -> Either e b) -> [a] -> [b]
mapMaybe' _ [] = []
mapMaybe' f (x:xs) = case f x of
  Right v -> v : mapMaybe' f xs
  Left _  -> mapMaybe' f xs
