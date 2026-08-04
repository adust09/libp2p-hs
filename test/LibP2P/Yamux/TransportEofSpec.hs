-- | Transport EOF mid-frame (issue #171).
--
-- When the underlying transport hits EOF partway through a frame --
-- after a partial header, or after a header whose declared payload
-- never fully arrives -- the session must fail cleanly: the receive
-- loop terminates, blocked readers and pending pings are unblocked
-- with an error instead of hanging, and no new streams can be opened.
module LibP2P.Yamux.TransportEofSpec (spec) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM (readTVarIO)
import qualified Data.ByteString as BS
import Data.Word (Word32)
import LibP2P.Yamux.Frame
import LibP2P.Yamux.HostilePeer
import LibP2P.Yamux.Session
import LibP2P.Yamux.Stream (streamRead, streamWrite)
import LibP2P.Yamux.Types
import System.Timeout (timeout)
import Test.Hspec

synF :: Flags
synF = defaultFlags {flagSYN = True}

dataHdr :: Flags -> Word32 -> Word32 -> YamuxHeader
dataHdr = YamuxHeader 0 FrameData

-- | Establish inbound stream 1 on a server-role session, consuming the
-- WindowUpdate ACK.
openAccepted :: HostilePeer -> IO YamuxStream
openAccepted hp = do
  injectFrame hp (dataHdr synF 1 0) BS.empty
  stream <- acceptWithin hp
  (ackHdr, _) <- expectFrame hp
  flagACK (yhFlags ackHdr) `shouldBe` True
  pure stream

spec :: Spec
spec = do
  describe "Transport EOF after a partial header" $ do
    it "unblocks a blocked reader and a pending ping with an error, refuses new streams" $
      withHostilePeer RoleServer $ \hp -> do
        stream <- openAccepted hp
        withAsync (streamRead stream) $ \readerA ->
          withAsync (ping (hpSession hp)) $ \pingA -> do
            (pingSyn, _) <- expectFrame hp
            yhType pingSyn `shouldBe` FramePing
            -- 5 of the 12 header bytes arrive, then the transport dies
            hpInject hp (BS.pack [0x00, 0x00, 0x00, 0x00, 0x00])
            hpCloseInject hp
            readerRes <- timeout 1000000 (wait readerA)
            readerRes `shouldBe` Just (Left YamuxStreamReset)
            pingRes <- timeout 1000000 (wait pingA)
            pingRes `shouldBe` Just (Left YamuxSessionShutdown)
        openRes <- openStream (hpSession hp)
        case openRes of
          Left err -> err `shouldBe` YamuxSessionShutdown
          Right _ -> expectationFailure "openStream succeeded after transport EOF"

  describe "Transport EOF after a header but partial payload" $ do
    it "fails the session cleanly when a declared payload is cut short" $
      withHostilePeer RoleServer $ \hp -> do
        stream <- openAccepted hp
        withAsync (streamRead stream) $ \readerA -> do
          -- Header declares 100 payload bytes; only 40 arrive before EOF
          injectFrame hp (dataHdr defaultFlags 1 100) (BS.replicate 40 0x61)
          hpCloseInject hp
          readerRes <- timeout 1000000 (wait readerA)
          readerRes `shouldBe` Just (Left YamuxStreamReset)
        -- The truncated frame is never delivered as data, the session
        -- is shut down, and writes on the dead stream fail too
        awaitTrue (ysessShutdown (hpSession hp))
        writeRes <- streamWrite stream "late"
        writeRes `shouldBe` Left YamuxStreamReset
        st <- readTVarIO (ysState stream)
        st `shouldBe` StreamReset
