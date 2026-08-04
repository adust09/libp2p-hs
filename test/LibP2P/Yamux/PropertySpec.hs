-- | QuickCheck properties for Yamux (issue #171).
--
-- Frame codec: encode/decode round-trips over arbitrary valid headers,
-- and rejection of arbitrary invalid type/version bytes (the type at
-- the codec level, the version at the session level, where the spec
-- places the check).
--
-- Window accounting: for any sequence of writes and peer window
-- grants, the session never puts more data bytes on the wire than the
-- advertised send window allows, converges to exactly the deliverable
-- amount, and keeps its bookkeeping consistent with the model.
module LibP2P.Yamux.PropertySpec (spec) where

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forM, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Word (Word8)
import LibP2P.Yamux.Frame
import LibP2P.Yamux.HostilePeer
import LibP2P.Yamux.Session
import LibP2P.Yamux.Stream (streamWrite)
import LibP2P.Yamux.Types
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck

genFrameType :: Gen FrameType
genFrameType = elements [FrameData, FrameWindowUpdate, FramePing, FrameGoAway]

genFlags :: Gen Flags
genFlags = Flags <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Any header the codec can produce: every type, every flag
-- combination, arbitrary version/stream-id/length.
genHeader :: Gen YamuxHeader
genHeader =
  YamuxHeader
    <$> arbitrary
    <*> genFrameType
    <*> genFlags
    <*> arbitrary
    <*> arbitrary

-- | A 12-byte block whose type byte (offset 1) is outside the four
-- spec-defined frame types; every other byte is arbitrary.
genInvalidTypeHeader :: Gen ByteString
genInvalidTypeHeader = do
  b0 <- arbitrary
  t <- fromIntegral <$> chooseInt (0x04, 0xFF)
  rest <- vectorOf 10 (arbitrary :: Gen Word8)
  pure (BS.pack (b0 : t : rest))

spec :: Spec
spec = do
  describe "Frame codec properties" $ do
    it "round-trips any valid header through encode/decode" $
      forAll genHeader $ \hdr ->
        decodeHeader (encodeHeader hdr) === Right hdr

    it "always encodes to exactly 12 bytes" $
      forAll genHeader $ \hdr ->
        BS.length (encodeHeader hdr) === headerSize

    it "rejects any 12-byte block with an unknown type byte" $
      forAll genInvalidTypeHeader $ \bs ->
        property (isLeft (decodeHeader bs))

    it "rejects any input shorter than 12 bytes" $
      forAll (chooseInt (0, 11) >>= \n -> vectorOf n (arbitrary :: Gen Word8)) $
        \ws -> property (isLeft (decodeHeader (BS.pack ws)))

  describe "Session-level rejection of invalid header bytes" $ do
    it "terminates with GoAway(0x01) on any nonzero version byte" $
      withMaxSuccess 25 $
        forAll (chooseInt (1, 255)) $ \v -> ioProperty $
          withHostilePeer RoleServer $ \hp -> do
            let hdr = YamuxHeader (fromIntegral v) FrameData defaultFlags 1 0
            injectFrame hp hdr BS.empty
            (goAway, _) <- expectFrame hp
            yhType goAway `shouldBe` FrameGoAway
            yhLength goAway `shouldBe` 0x01
            pure True

    it "terminates with GoAway(0x01) on any unknown type byte" $
      withMaxSuccess 25 $
        forAll genInvalidTypeHeader $ \bs -> ioProperty $
          withHostilePeer RoleServer $ \hp -> do
            hpInject hp bs
            (goAway, _) <- expectFrame hp
            yhType goAway `shouldBe` FrameGoAway
            yhLength goAway `shouldBe` 0x01
            pure True

  describe "Window accounting properties" $ do
    it "never sends more data than the advertised send window for any op sequence" $
      withMaxSuccess 30 $
        forAll genOps $ \ops -> ioProperty (runWindowOps ops)

-- | One step of the window-accounting scenario: a local write of n
-- bytes, or the peer granting a window update of the given delta.
data WindowOp = OpWrite Int | OpGrant Int
  deriving (Show)

genOps :: Gen [WindowOp]
genOps = do
  n <- chooseInt (1, 10)
  vectorOf n $
    frequency
      [ (3, OpWrite <$> chooseInt (1, 100000))
      , (2, OpGrant <$> chooseInt (0, 150000))
      ]

-- | Drive an outbound stream with the given op sequence and check that
-- the bytes observed on the wire never exceed the advertised window,
-- converge to exactly min(totalWritten, window), and that the stream's
-- remaining send window matches the model afterwards.
runWindowOps :: [WindowOp] -> IO Bool
runWindowOps ops =
  withHostilePeer RoleClient $ \hp -> do
    Right stream <- openStream (hpSession hp)
    (synHdr, _) <- expectFrame hp
    flagSYN (yhFlags synHdr) `shouldBe` True
    writers <- fmap concat . forM ops $ \op -> case op of
      OpWrite n -> do
        a <- async (void (streamWrite stream (BS.replicate n 0x61)))
        pure [a]
      OpGrant d -> do
        let hdr = YamuxHeader 0 FrameWindowUpdate defaultFlags 1 (fromIntegral d)
        injectFrame hp hdr BS.empty
        pure []
    let totalWritten = sum [n | OpWrite n <- ops]
        limit = fromIntegral initialWindowSize + sum [d | OpGrant d <- ops]
        expected = min totalWritten limit
        drain acc
          | acc == expected = pure ()
          | otherwise = do
              (hdr, payload) <- expectFrame hp
              yhType hdr `shouldBe` FrameData
              let acc' = acc + BS.length payload
              if acc' > limit
                then
                  expectationFailure $
                    "sent " <> show acc' <> " bytes, window allows " <> show limit
                else drain acc'
    flip finally (mapM_ cancel writers) $ do
      drain 0
      -- After convergence the sender must stay quiet: any further data
      -- frame would either overrun the window or duplicate data.
      extra <- timeout 50000 (hpNextFrame hp)
      case extra of
        Nothing -> pure ()
        Just (hdr, _) ->
          expectationFailure ("unexpected extra frame: " <> show hdr)
      -- Bookkeeping: the remaining send window equals the advertised
      -- window minus what was actually delivered.
      settled <- timeout 1000000 $ atomically $ do
        w <- readTVar (ysSendWindow stream)
        check (fromIntegral w == limit - expected)
      case settled of
        Just () -> pure True
        Nothing -> do
          w <- readTVarIO (ysSendWindow stream)
          expectationFailure $
            "send window settled at " <> show w
              <> ", model expects " <> show (limit - expected)
          pure False
