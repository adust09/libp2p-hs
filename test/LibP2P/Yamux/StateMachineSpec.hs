-- | Yamux stream state-machine coverage (issue #171).
--
-- The HashiCorp yamux spec (spec.md, Flag Field) says SYN, ACK, FIN and
-- RST "may be sent with a data or window update message", so one state
-- transition table must hold for both frame types. These tests inject
-- raw frames through the hostile-peer harness to exercise the
-- (state x flag x frame-type) cells our own sender never produces.
module LibP2P.Yamux.StateMachineSpec (spec) where

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import Data.Word (Word32)
import LibP2P.Yamux.Frame
import LibP2P.Yamux.HostilePeer
import LibP2P.Yamux.Session
import LibP2P.Yamux.Stream (streamClose, streamRead, streamWrite)
import LibP2P.Yamux.Types
import System.Timeout (timeout)
import Test.Hspec

synF, ackF, finF, rstF, noF :: Flags
synF = defaultFlags {flagSYN = True}
ackF = defaultFlags {flagACK = True}
finF = defaultFlags {flagFIN = True}
rstF = defaultFlags {flagRST = True}
noF = defaultFlags

dataHdr, wuHdr :: Flags -> Word32 -> Word32 -> YamuxHeader
dataHdr = YamuxHeader 0 FrameData
wuHdr = YamuxHeader 0 FrameWindowUpdate

pingHdr :: Flags -> Word32 -> YamuxHeader
pingHdr f = YamuxHeader 0 FramePing f 0

-- | Round-trip a Ping through the session. Because recvLoop dispatches
-- frames serially, receiving the echo proves every frame injected
-- before the Ping has been fully processed.
pingFence :: HostilePeer -> Word32 -> Expectation
pingFence hp opaque = do
  injectFrame hp (pingHdr synF opaque) BS.empty
  (hdr, _) <- expectFrame hp
  yhType hdr `shouldBe` FramePing
  yhLength hdr `shouldBe` opaque

-- | Open an inbound stream on a server-role session via a Data SYN and
-- accept it, consuming the WindowUpdate ACK the session emits.
openAccepted :: HostilePeer -> IO YamuxStream
openAccepted hp = do
  injectFrame hp (dataHdr synF 1 0) BS.empty
  stream <- acceptWithin hp
  (ackHdr, _) <- expectFrame hp
  yhType ackHdr `shouldBe` FrameWindowUpdate
  flagACK (yhFlags ackHdr) `shouldBe` True
  pure stream

spec :: Spec
spec = do
  describe "SYNSent state (outbound stream, before ACK)" $ do
    it "establishes when ACK arrives on a Data frame" $
      withHostilePeer RoleClient $ \hp -> do
        Right stream <- openStream (hpSession hp)
        (synOut, _) <- expectFrame hp
        flagSYN (yhFlags synOut) `shouldBe` True
        injectFrame hp (dataHdr ackF 1 0) BS.empty
        awaitState stream StreamEstablished

    it "half-closes when FIN arrives on a Data frame" $
      withHostilePeer RoleClient $ \hp -> do
        Right stream <- openStream (hpSession hp)
        _ <- expectFrame hp -- Data SYN
        injectFrame hp (dataHdr finF 1 0) BS.empty
        awaitState stream StreamRemoteClose

    it "resets when RST arrives on a Data frame" $
      withHostilePeer RoleClient $ \hp -> do
        Right stream <- openStream (hpSession hp)
        _ <- expectFrame hp -- Data SYN
        injectFrame hp (dataHdr rstF 1 0) BS.empty
        awaitState stream StreamReset
        result <- streamWrite stream "data"
        result `shouldBe` Left YamuxStreamReset

    it "resets when RST arrives on a WindowUpdate frame" $
      withHostilePeer RoleClient $ \hp -> do
        Right stream <- openStream (hpSession hp)
        _ <- expectFrame hp -- Data SYN
        injectFrame hp (wuHdr rstF 1 0) BS.empty
        awaitState stream StreamReset

  describe "WindowUpdate SYN (go-libp2p stream open pattern)" $ do
    it "creates an inbound stream and applies the window delta from the SYN" $
      withHostilePeer RoleServer $ \hp -> do
        -- WindowUpdate+SYN with a delta announces a larger initial
        -- window as part of stream open (spec.md, Flow Control)
        injectFrame hp (wuHdr synF 1 initialWindowSize) BS.empty
        stream <- acceptWithin hp
        (ackHdr, _) <- expectFrame hp
        yhType ackHdr `shouldBe` FrameWindowUpdate
        flagACK (yhFlags ackHdr) `shouldBe` True
        yhStreamId ackHdr `shouldBe` 1
        result <- timeout 1000000 $ atomically $ do
          w <- readTVar (ysSendWindow stream)
          check (w == 2 * initialWindowSize)
        result `shouldBe` Just ()

    it "rejects a duplicate stream ID on a WindowUpdate SYN with GoAway(0x01) on the wire" $
      withHostilePeer RoleServer $ \hp -> do
        injectFrame hp (wuHdr synF 1 0) BS.empty
        _ <- acceptWithin hp
        _ <- expectFrame hp -- WindowUpdate ACK
        injectFrame hp (wuHdr synF 1 0) BS.empty
        (goAway, _) <- expectFrame hp
        yhType goAway `shouldBe` FrameGoAway
        yhStreamId goAway `shouldBe` 0
        yhLength goAway `shouldBe` 0x01 -- protocol error code
        awaitTrue (ysessShutdown (hpSession hp))

  describe "Established state, flags on WindowUpdate frames" $ do
    it "half-closes when FIN arrives on a WindowUpdate frame" $
      withHostilePeer RoleServer $ \hp -> do
        stream <- openAccepted hp
        injectFrame hp (wuHdr finF 1 0) BS.empty
        awaitState stream StreamRemoteClose
        eof <- streamRead stream
        eof `shouldBe` Left YamuxStreamClosed

    it "resets when RST arrives on a WindowUpdate frame" $
      withHostilePeer RoleServer $ \hp -> do
        stream <- openAccepted hp
        injectFrame hp (wuHdr rstF 1 0) BS.empty
        awaitState stream StreamReset
        result <- streamRead stream
        result `shouldBe` Left YamuxStreamReset

  describe "Half-closed and terminal states" $ do
    it "ignores a duplicate FIN in RemoteClose" $
      withHostilePeer RoleServer $ \hp -> do
        stream <- openAccepted hp
        injectFrame hp (dataHdr finF 1 0) BS.empty
        awaitState stream StreamRemoteClose
        injectFrame hp (dataHdr finF 1 0) BS.empty
        pingFence hp 7 -- guarantees the second FIN was dispatched
        st <- readTVarIO (ysState stream)
        st `shouldBe` StreamRemoteClose

    it "does not resurrect a Closed stream on a later FIN" $
      withHostilePeer RoleServer $ \hp -> do
        stream <- openAccepted hp
        Right () <- streamClose stream -- Established -> LocalClose
        (finOut, _) <- expectFrame hp
        flagFIN (yhFlags finOut) `shouldBe` True
        injectFrame hp (dataHdr finF 1 0) BS.empty -- LocalClose -> Closed
        awaitState stream StreamClosed
        injectFrame hp (dataHdr finF 1 0) BS.empty
        pingFence hp 8
        st <- readTVarIO (ysState stream)
        st `shouldBe` StreamClosed

  describe "Frames for unknown stream IDs" $ do
    it "keeps the session alive when RST arrives for an unknown stream" $
      withHostilePeer RoleServer $ \hp -> do
        injectFrame hp (dataHdr rstF 5 0) BS.empty
        pingFence hp 9
        shut <- readTVarIO (ysessShutdown (hpSession hp))
        shut `shouldBe` False

    it "discards a WindowUpdate for an unknown stream without terminating" $
      withHostilePeer RoleServer $ \hp -> do
        injectFrame hp (wuHdr noF 7 4096) BS.empty
        pingFence hp 10
        shut <- readTVarIO (ysessShutdown (hpSession hp))
        shut `shouldBe` False

  describe "Session termination on malformed input" $ do
    it "sends GoAway(0x01) before terminating on an unknown frame type" $
      withHostilePeer RoleServer $ \hp -> do
        hpInject hp (BS.pack [0x00, 0xFF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])
        (goAway, _) <- expectFrame hp
        yhType goAway `shouldBe` FrameGoAway
        yhLength goAway `shouldBe` 0x01
        awaitTrue (ysessShutdown (hpSession hp))

    it "sends GoAway(0x01) before terminating on a non-zero version" $
      withHostilePeer RoleServer $ \hp -> do
        injectFrame hp (YamuxHeader 1 FrameData noF 1 0) BS.empty
        (goAway, _) <- expectFrame hp
        yhType goAway `shouldBe` FrameGoAway
        yhLength goAway `shouldBe` 0x01
        awaitTrue (ysessShutdown (hpSession hp))

  describe "GoAway from the peer" $ do
    it "raw GoAway(0x01) prevents new outbound streams and surfaces the code" $
      withHostilePeer RoleClient $ \hp -> do
        injectFrame hp (YamuxHeader 0 FrameGoAway noF 0 0x01) BS.empty
        awaitRemoteGoAway (hpSession hp) GoAwayProtocol
        result <- openStream (hpSession hp)
        case result of
          Left err -> err `shouldBe` YamuxGoAway GoAwayProtocol
          Right _ -> expectationFailure "openStream succeeded after remote GoAway"

  describe "Window accounting" $ do
    it "replenishes exactly the consumed bytes after a partial read" $
      withHostilePeer RoleServer $ \hp -> do
        stream <- openAccepted hp
        injectFrame hp (dataHdr noF 1 60) (BS.replicate 60 0xAA)
        injectFrame hp (dataHdr noF 1 40) (BS.replicate 40 0xBB)
        -- Receive window shrinks by the declared lengths on arrival
        debit <- timeout 1000000 $ atomically $ do
          w <- readTVar (ysRecvWindow stream)
          check (w == initialWindowSize - 100)
        debit `shouldBe` Just ()
        -- Reading one chunk must credit back exactly those 60 bytes
        Right chunk <- streamRead stream
        chunk `shouldBe` BS.replicate 60 0xAA
        (wu, _) <- expectFrame hp
        yhType wu `shouldBe` FrameWindowUpdate
        yhStreamId wu `shouldBe` 1
        yhLength wu `shouldBe` 60
        credit <- timeout 1000000 $ atomically $ do
          w <- readTVar (ysRecvWindow stream)
          check (w == initialWindowSize - 40)
        credit `shouldBe` Just ()
