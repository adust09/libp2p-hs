module Test.Network.LibP2P.Protocol.GossipSub.TypesSpec (spec) where

import Test.Hspec

import qualified Data.ByteString as BS
import Network.LibP2P.Protocol.GossipSub.Types

spec :: Spec
spec = do
  describe "GossipSub.Types" $ do
    describe "defaultGossipSubParams" $ do
      it "has D=6, D_lo=4, D_hi=12" $ do
        paramD defaultGossipSubParams `shouldBe` 6
        paramDlo defaultGossipSubParams `shouldBe` 4
        paramDhi defaultGossipSubParams `shouldBe` 12

      it "has D_lazy=6, D_score=4, D_out=2" $ do
        paramDlazy defaultGossipSubParams `shouldBe` 6
        paramDscore defaultGossipSubParams `shouldBe` 4
        paramDout defaultGossipSubParams `shouldBe` 2

      it "has heartbeat=1s, fanoutTTL=60s, seenTTL=120s" $ do
        paramHeartbeatInterval defaultGossipSubParams `shouldBe` 1
        paramFanoutTTL defaultGossipSubParams `shouldBe` 60
        paramSeenTTL defaultGossipSubParams `shouldBe` 120

      it "has pruneBackoff=60s, unsubBackoff=10s" $ do
        paramPruneBackoff defaultGossipSubParams `shouldBe` 60
        paramUnsubBackoff defaultGossipSubParams `shouldBe` 10

      it "has gossipFactor=0.25, floodPublish=True, StrictSign" $ do
        paramGossipFactor defaultGossipSubParams `shouldBe` 0.25
        paramFloodPublish defaultGossipSubParams `shouldBe` True
        paramSignaturePolicy defaultGossipSubParams `shouldBe` StrictSign

      it "has mcacheLen=5, mcacheGossip=3" $ do
        paramMcacheLen defaultGossipSubParams `shouldBe` 5
        paramMcacheGossip defaultGossipSubParams `shouldBe` 3

    describe "defaultMessageId" $ do
      it "concatenates from and seqno" $ do
        let msg = PubSubMessage
              { msgFrom      = Just (BS.pack [1, 2, 3])
              , msgData      = BS.empty
              , msgSeqNo     = Just (BS.pack [0, 0, 0, 0, 0, 0, 0, 42])
              , msgTopic     = "test"
              , msgSignature = Nothing
              , msgKey       = Nothing
              }
        defaultMessageId msg `shouldBe` BS.pack [1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 42]

      it "returns empty when both from and seqno are absent" $ do
        let msg = PubSubMessage
              { msgFrom      = Nothing
              , msgData      = BS.empty
              , msgSeqNo     = Nothing
              , msgTopic     = "test"
              , msgSignature = Nothing
              , msgKey       = Nothing
              }
        defaultMessageId msg `shouldBe` BS.empty

    describe "emptyRPC" $ do
      it "has no subscriptions, messages, or control" $ do
        rpcSubscriptions emptyRPC `shouldBe` []
        rpcPublish emptyRPC `shouldBe` []
        rpcControl emptyRPC `shouldBe` Nothing

    describe "emptyControlMessage" $ do
      it "has empty IHAVE/IWANT/GRAFT/PRUNE lists" $ do
        ctrlIHave emptyControlMessage `shouldBe` []
        ctrlIWant emptyControlMessage `shouldBe` []
        ctrlGraft emptyControlMessage `shouldBe` []
        ctrlPrune emptyControlMessage `shouldBe` []
