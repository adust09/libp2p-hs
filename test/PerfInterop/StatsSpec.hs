module PerfInterop.StatsSpec (spec) where

import PerfInterop.Stats
import Test.Hspec

spec :: Spec
spec = do
  describe "percentile" $ do
    it "should interpolate linearly between samples" $ do
      let xs = [1, 2, 3, 4]
      percentile xs 25 `shouldBe` 1.75
      percentile xs 50 `shouldBe` 2.5
      percentile xs 75 `shouldBe` 3.25

    it "should return the exact sample when the index is integral" $ do
      let xs = [10, 20, 30, 40, 50]
      percentile xs 0 `shouldBe` 10
      percentile xs 50 `shouldBe` 30
      percentile xs 100 `shouldBe` 50

    it "should return the single sample for a singleton list" $ do
      percentile [7] 25 `shouldBe` 7
      percentile [7] 75 `shouldBe` 7

  describe "computeStats" $ do
    it "should report min/max from the full set when there are no outliers" $ do
      let s = computeStats [3, 1, 2, 5, 4]
      statMin s `shouldBe` 1
      statMax s `shouldBe` 5
      statMedian s `shouldBe` 3
      statOutliers s `shouldBe` []
      statSamples s `shouldBe` [1, 2, 3, 4, 5]

    it "should flag values outside the 1.5*IQR fences as outliers" $ do
      -- sorted: [10,11,12,13,14,100]; q1=11.25, q3=13.75, iqr=2.5
      -- fences: [7.5, 17.5] -> 100 is an outlier
      let s = computeStats [12, 100, 10, 13, 11, 14]
      statOutliers s `shouldBe` [100]
      statMax s `shouldBe` 14
      statMin s `shouldBe` 10
      statSamples s `shouldBe` [10, 11, 12, 13, 14, 100]

    it "should keep quartiles computed over all samples including outliers" $ do
      let s = computeStats [12, 100, 10, 13, 11, 14]
      statQ1 s `shouldBe` 11.25
      statQ3 s `shouldBe` 13.75

  describe "renderStatsYaml" $ do
    it "should render a section with 2-decimal formatting" $ do
      let s = computeStats [2.04, 2.05, 2.06]
      renderStatsYaml "upload" 3 2 "Gbps" s `shouldBe` unlines
        [ "upload:"
        , "  iterations: 3"
        , "  min: 2.04"
        , "  q1: 2.04"
        , "  median: 2.05"
        , "  q3: 2.05"
        , "  max: 2.06"
        , "  outliers: []"
        , "  samples: [2.04, 2.05, 2.06]"
        , "  unit: Gbps"
        ]

    it "should render outliers as a flow list with the same precision" $ do
      let s = computeStats [12, 100, 10, 13, 11, 14]
      renderStatsYaml "latency" 6 3 "ms" s `shouldBe` unlines
        [ "latency:"
        , "  iterations: 6"
        , "  min: 10.000"
        , "  q1: 11.250"
        , "  median: 12.500"
        , "  q3: 13.750"
        , "  max: 14.000"
        , "  outliers: [100.000]"
        , "  samples: [10.000, 11.000, 12.000, 13.000, 14.000, 100.000]"
        , "  unit: ms"
        ]
