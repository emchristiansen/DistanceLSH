module BenchmarkUtil (
  breakIntoFolds,
  benchmark
) where

--------------------------------------------------------------------------------

import Control.Exception

import Base
import Util

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

benchmarkSingle :: NNMethod a -> Distance a -> ([a], [a]) -> Double
benchmarkSingle method distance (test, train) =
  fractionTrue $ zipWith (==) golden estimated
 where
  golden =
   let brute = bruteNN distance train
   in map (brute . pair 1) test
  estimated =
   let method' = method distance train
   in map (method' . pair 1) test

benchmark :: NNMethod a -> Distance a -> [a] -> Double
benchmark method distance points = mean $ map method' splits
 where
  method' = benchmarkSingle method distance
  splits = breakIntoFolds 3 points

