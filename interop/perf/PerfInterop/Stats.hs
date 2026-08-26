-- | Sample statistics and YAML rendering for the unified-testing perf
-- contract (docs/write-a-perf-test-app.md).
--
-- Quartiles use linear interpolation over all sorted samples; outliers
-- are values outside the [q1 - 1.5*IQR, q3 + 1.5*IQR] fences; min/max
-- are taken over the non-outlier samples (falling back to the full set
-- if everything is flagged). This mirrors the reference test apps so
-- results are comparable across implementations.
module PerfInterop.Stats
  ( Stats (..)
  , computeStats
  , percentile
  , renderStatsYaml
  ) where

import Data.List (intercalate, partition, sort)
import Text.Printf (printf)

-- | Summary statistics over one measurement group.
data Stats = Stats
  { statMin      :: !Double
  , statQ1       :: !Double
  , statMedian   :: !Double
  , statQ3       :: !Double
  , statMax      :: !Double
  , statOutliers :: ![Double]  -- ^ Values outside the IQR fences, sorted
  , statSamples  :: ![Double]  -- ^ All samples, sorted
  } deriving (Show, Eq)

-- | Interpolated percentile of a sorted, non-empty sample list.
percentile :: [Double] -> Double -> Double
percentile sorted p =
  let n = length sorted
      index = (p / 100) * fromIntegral (n - 1)
      lower = floor index :: Int
      upper = ceiling index :: Int
      weight = index - fromIntegral lower
  in if lower == upper
       then sorted !! lower
       else sorted !! lower * (1 - weight) + sorted !! upper * weight

-- | Compute summary statistics for a list of samples.
computeStats :: [Double] -> Stats
computeStats [] = Stats 0 0 0 0 0 [] []
computeStats values =
  let sorted = sort values
      q1 = percentile sorted 25
      median = percentile sorted 50
      q3 = percentile sorted 75
      iqr = q3 - q1
      lowerFence = q1 - 1.5 * iqr
      upperFence = q3 + 1.5 * iqr
      (outliers, kept) = partition (\v -> v < lowerFence || v > upperFence) sorted
      bounds = if null kept then sorted else kept
  in Stats
       { statMin = minimum bounds
       , statQ1 = q1
       , statMedian = median
       , statQ3 = q3
       , statMax = maximum bounds
       , statOutliers = outliers
       , statSamples = sorted
       }

-- | Render one YAML results section (contract Results Schema).
renderStatsYaml
  :: String  -- ^ Section name (upload | download | latency)
  -> Int     -- ^ Iteration count
  -> Int     -- ^ Decimal places for formatted values
  -> String  -- ^ Unit label (Gbps | ms)
  -> Stats
  -> String
renderStatsYaml section iterations decimals unit s = unlines
  [ section ++ ":"
  , "  iterations: " ++ show iterations
  , "  min: " ++ fmt (statMin s)
  , "  q1: " ++ fmt (statQ1 s)
  , "  median: " ++ fmt (statMedian s)
  , "  q3: " ++ fmt (statQ3 s)
  , "  max: " ++ fmt (statMax s)
  , "  outliers: " ++ fmtList (statOutliers s)
  , "  samples: " ++ fmtList (statSamples s)
  , "  unit: " ++ unit
  ]
  where
    fmt = printf ("%." ++ show decimals ++ "f")
    fmtList xs = "[" ++ intercalate ", " (map fmt xs) ++ "]"
