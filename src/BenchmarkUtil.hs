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

benchmarkSingle :: NNConstructor a b -> NNRunner a b -> Distance a -> ([a], [a]) -> Double
benchmarkSingle mk use distance (test, train) =
  fractionTrue $ zipWith (==) golden estimated
 where
  golden =
   let brute = bruteNN distance train
   in map (brute . pair 1) test
  estimated =
   let matcher = mk distance train
   in map (use matcher . pair 1) test

benchmark :: NNConstructor a b -> NNRunner a b -> Distance a -> [a] -> Double
benchmark mk use distance points = mean $ map method' splits
 where
  method' = benchmarkSingle mk use distance
  splits = breakIntoFolds 3 points

