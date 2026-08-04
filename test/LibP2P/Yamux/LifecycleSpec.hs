-- | Yamux session lifecycle defects (issue #164).
--
-- Covers the accept-backlog bound, terminal-state preservation across
-- acceptStream, reclamation of closed/reset streams from the session
-- map, and preservation of the received GoAway error code.
module LibP2P.Yamux.LifecycleSpec (spec) where

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import LibP2P.Yamux.Frame
import LibP2P.Yamux.HostilePeer
import LibP2P.Yamux.Session
import LibP2P.Yamux.Stream (streamClose, streamRead, streamReset)
import LibP2P.Yamux.Types
import Test.Hspec

synF, finF, rstF :: Flags
synF = defaultFlags {flagSYN = True}
finF = defaultFlags {flagFIN = True}
rstF = defaultFlags {flagRST = True}

dataHdr :: Flags -> Word32 -> Word32 -> YamuxHeader
dataHdr = YamuxHeader 0 FrameData

pingHdr :: Word32 -> YamuxHeader
pingHdr = YamuxHeader 0 FramePing synF 0

-- | Round-trip a Ping through the session. recvLoop dispatches frames
-- serially, so receiving the echo proves every frame injected before
-- the Ping has been fully processed.
pingFence :: HostilePeer -> Word32 -> Expectation
pingFence hp opaque = do
  injectFrame hp (pingHdr opaque) BS.empty
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

streamMapKeys :: HostilePeer -> IO [Word32]
streamMapKeys hp = Map.keys <$> readTVarIO (ysessStreams (hpSession hp))

spec :: Spec
spec = do
  describe "Accept backlog bound (spec.md: buffer MUST be bounded)" $ do
    it "resets the 257th unaccepted inbound SYN and keeps the session alive" $
      withHostilePeer RoleServer $ \hp -> do
        -- 256 SYNs fill the backlog (client parity: odd IDs 1..511)
        mapM_
          (\sid -> injectFrame hp (dataHdr synF sid 0) BS.empty)
          [1, 3 .. 511]
        -- The 257th SYN must be rejected with RST, not buffered
        injectFrame hp (dataHdr synF 513 0) BS.empty
        (rstHdr, _) <- expectFrame hp
        flagRST (yhFlags rstHdr) `shouldBe` True
        yhStreamId rstHdr `shouldBe` 513
        -- Session survives: not a protocol error, no GoAway
        pingFence hp 1
        shut <- readTVarIO (ysessShutdown (hpSession hp))
        shut `shouldBe` False
        -- The rejected stream is not registered
        keys <- streamMapKeys hp
        length keys `shouldBe` 256
        keys `shouldNotContain` [513]

    it "frees a backlog slot once a stream is accepted" $
      withHostilePeer RoleServer $ \hp -> do
        mapM_
          (\sid -> injectFrame hp (dataHdr synF sid 0) BS.empty)
          [1, 3 .. 511]
        stream <- acceptWithin hp
        ysStreamId stream `shouldBe` 1
        (ackHdr, _) <- expectFrame hp
        flagACK (yhFlags ackHdr) `shouldBe` True
        -- A new SYN now fits again instead of being reset
        injectFrame hp (dataHdr synF 513 0) BS.empty
        pingFence hp 2
        keys <- streamMapKeys hp
        keys `shouldContain` [513]

  describe "Terminal states survive acceptStream (issue #164.2)" $ do
    it "returns a stream still in Reset when RST arrived before accept" $
      withHostilePeer RoleServer $ \hp -> do
        injectFrame hp (dataHdr synF 1 0) BS.empty
        injectFrame hp (dataHdr rstF 1 0) BS.empty
        pingFence hp 3 -- both frames dispatched before we accept
        stream <- acceptWithin hp
        st <- readTVarIO (ysState stream)
        st `shouldBe` StreamReset

  describe "Stream reclamation (issue #164.5)" $ do
    it "removes a stream from the session map on remote RST" $
      withHostilePeer RoleServer $ \hp -> do
        stream <- openAccepted hp
        injectFrame hp (dataHdr rstF 1 0) BS.empty
        awaitState stream StreamReset
        keys <- streamMapKeys hp
        keys `shouldNotContain` [1]

    it "removes a stream from the session map on local reset" $
      withHostilePeer RoleServer $ \hp -> do
        stream <- openAccepted hp
        streamReset stream
        keys <- streamMapKeys hp
        keys `shouldNotContain` [1]

    it "removes a stream once local FIN is followed by remote FIN" $
      withHostilePeer RoleServer $ \hp -> do
        stream <- openAccepted hp
        Right () <- streamClose stream -- Established -> LocalClose
        (finOut, _) <- expectFrame hp
        flagFIN (yhFlags finOut) `shouldBe` True
        injectFrame hp (dataHdr finF 1 0) BS.empty -- LocalClose -> Closed
        awaitState stream StreamClosed
        keys <- streamMapKeys hp
        keys `shouldNotContain` [1]

    it "removes a stream once remote FIN is followed by local close" $
      withHostilePeer RoleServer $ \hp -> do
        stream <- openAccepted hp
        injectFrame hp (dataHdr finF 1 0) BS.empty -- Established -> RemoteClose
        awaitState stream StreamRemoteClose
        Right () <- streamClose stream -- RemoteClose -> Closed
        st <- readTVarIO (ysState stream)
        st `shouldBe` StreamClosed
        keys <- streamMapKeys hp
        keys `shouldNotContain` [1]

    it "keeps a reclaimed stream's handle observable as Reset" $
      withHostilePeer RoleServer $ \hp -> do
        stream <- openAccepted hp
        injectFrame hp (dataHdr rstF 1 0) BS.empty
        awaitState stream StreamReset
        -- The map slot is gone, but the application-held handle still
        -- reports the reset instead of hanging
        result <- streamRead stream
        result `shouldBe` Left YamuxStreamReset

    it "keeps half-closed streams registered" $
      withHostilePeer RoleServer $ \hp -> do
        stream <- openAccepted hp
        injectFrame hp (dataHdr finF 1 0) BS.empty
        awaitState stream StreamRemoteClose
        keys <- streamMapKeys hp
        keys `shouldContain` [1]

  describe "GoAway code preservation (issue #164.3)" $ do
    it "records GoAwayInternal (0x02) and surfaces it from openStream" $
      withHostilePeer RoleClient $ \hp -> do
        injectFrame hp (YamuxHeader 0 FrameGoAway defaultFlags 0 0x02) BS.empty
        awaitRemoteGoAway (hpSession hp) GoAwayInternal
        result <- openStream (hpSession hp)
        case result of
          Left err -> err `shouldBe` YamuxGoAway GoAwayInternal
          Right _ -> expectationFailure "openStream succeeded after remote GoAway"

    it "records GoAwayNormal (0x00) so a clean shutdown is distinguishable" $
      withHostilePeer RoleClient $ \hp -> do
        injectFrame hp (YamuxHeader 0 FrameGoAway defaultFlags 0 0x00) BS.empty
        awaitRemoteGoAway (hpSession hp) GoAwayNormal

    it "records a spec-undefined code as GoAwayProtocol" $
      withHostilePeer RoleClient $ \hp -> do
        injectFrame hp (YamuxHeader 0 FrameGoAway defaultFlags 0 0x7F) BS.empty
        awaitRemoteGoAway (hpSession hp) GoAwayProtocol
