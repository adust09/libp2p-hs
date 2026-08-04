-- | Golden trace replay against go-yamux wire behaviour (issue #171).
--
-- Each test replays a hand-derived, byte-exact session trace through
-- the hostile-peer harness and asserts every frame on both directions
-- literally, byte for byte. The inject-side bytes mirror what go-yamux
-- (as used by go-libp2p) puts on the wire; the expect-side bytes are
-- what a conformant peer must observe from us.
--
-- Header layout (spec.md): Version(1) Type(1) Flags(2 BE)
-- StreamID(4 BE) Length(4 BE).
module LibP2P.Yamux.TraceReplaySpec (spec) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM (readTVarIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import LibP2P.Yamux.HostilePeer
import LibP2P.Yamux.Session (ping)
import LibP2P.Yamux.Stream (streamClose, streamRead)
import LibP2P.Yamux.Types
import Test.Hspec

bytes :: [Word8] -> ByteString
bytes = BS.pack

spec :: Spec
spec = do
  describe "go-yamux trace replay" $ do
    it "replays a single-shot RPC: WindowUpdate+SYN open, data, FIN, FIN teardown" $
      withHostilePeer RoleServer $ \hp -> do
        -- >>> peer opens stream 1 the go-yamux way: WindowUpdate|SYN,
        --     delta 0 (no extra window beyond the implicit 256 KiB)
        hpInject hp $
          bytes [0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00]
        stream <- acceptWithin hp
        -- <<< we acknowledge the stream: WindowUpdate|ACK, delta 0
        expectBytes hp $
          bytes [0x00, 0x01, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00]
        -- >>> peer sends the request: Data, 5 bytes, "hello"
        hpInject hp $
          bytes [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x05]
            <> "hello"
        received <- streamRead stream
        received `shouldBe` Right "hello"
        -- <<< consuming the request credits the window back: plain
        --     WindowUpdate, delta 5
        expectBytes hp $
          bytes [0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x05]
        -- >>> peer half-closes: Data|FIN, no payload
        hpInject hp $
          bytes [0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00]
        eof <- streamRead stream
        eof `shouldBe` Left YamuxStreamClosed
        -- local close completes the teardown
        closeRes <- streamClose stream
        closeRes `shouldBe` Right ()
        -- <<< our half-close: Data|FIN, no payload
        expectBytes hp $
          bytes [0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00]
        st <- readTVarIO (ysState stream)
        st `shouldBe` StreamClosed

    it "replays a ping round-trip in each direction with byte-exact opaque echo" $
      withHostilePeer RoleClient $ \hp -> do
        withAsync (ping (hpSession hp)) $ \pingA -> do
          -- <<< our ping: Ping|SYN, StreamID 0, opaque 1 (first ping id)
          expectBytes hp $
            bytes [0x00, 0x02, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]
          -- >>> peer echoes it: Ping|ACK with the same opaque value
          hpInject hp $
            bytes [0x00, 0x02, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]
          pingRes <- wait pingA
          pingRes `shouldBe` Right ()
        -- >>> peer pings us: Ping|SYN, opaque 0xDEADBEEF
        hpInject hp $
          bytes [0x00, 0x02, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF]
        -- <<< we echo the exact opaque value back: Ping|ACK
        expectBytes hp $
          bytes [0x00, 0x02, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF]
