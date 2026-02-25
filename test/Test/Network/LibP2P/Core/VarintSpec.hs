module Test.Network.LibP2P.Core.VarintSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "Varint" $ do
    it "placeholder passes" $ do
      True `shouldBe` True
