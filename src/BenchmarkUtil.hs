module BenchmarkUtil (
  breakIntoFolds,
  benchmarkLSH
) where

--------------------------------------------------------------------------------

import Control.Exception

import Base
import LSH

--------------------------------------------------------------------------------

breakIntoFolds :: Int -> [a] -> [([a], [a])]
breakIntoFolds foldDepth points =
  assert (foldDepth >= 1) $
  if foldDepth == 1
  then [(front, back), (back, front)]
  else (recursiveSplit front back) ++ (recursiveSplit back front)
  where
    (front, back) = splitAt ((length points) `div` 2) points
    recursiveSplit containsTest allTrain =
      let pairs = breakIntoFolds (foldDepth - 1) containsTest
      in [(test, train ++ allTrain) | (test, train) <- pairs]

benchmarkLSHSingle :: Metric a => ([a], [a]) -> Double
benchmarkLSHSingle (test, train) =
  (fromIntegral numCorrect) / (fromIntegral (length test))
  where
   golden =
     let brute = BruteNN train
     in map (findNearestBrute brute) test
   estimated =
     let hashDimension = 32
         numCharikarLists = 8
         lsh = mkRescoreNN train hashDimension numCharikarLists
         radius = 2
         numRescore = 8
     in map (nearestRescoreNN lsh radius numRescore) test
   numCorrect = length $ filter id $ zipWith (==) golden estimated

benchmarkLSH :: Metric a => [a] -> [Double]
benchmarkLSH points = map benchmarkLSHSingle splits
  where
    splits = breakIntoFolds 3 points

