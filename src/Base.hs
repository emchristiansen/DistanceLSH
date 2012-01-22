module Base (
  BruteNN(BruteNN),
  findNearestBrute,
  Metric(distance),
  L2Vector(L2Vector)
) where

--------------------------------------------------------------------------------

import Control.Exception
import Data.List
import Data.Ord

--------------------------------------------------------------------------------

class Metric a where
  distance :: a -> a -> Double

data L2Vector = L2Vector [Double] deriving Show

instance Metric L2Vector where
  distance (L2Vector xs) (L2Vector ys) =
    assert ((length xs) == (length ys)) $
    sqrt $ sum squaredDifferences
    where
      squaredDifferences = map (^ 2) $ zipWith (-) xs ys

data Metric a => BruteNN a = BruteNN [a]

findNearestBrute :: Metric a => BruteNN a -> a -> (Double, Int)
findNearestBrute (BruteNN xs) query =
  minimumBy (comparing fst) distancesWithIndex
  where
    distancesWithIndex = zip (map (distance query) xs) [0 ..]

data Metric a => LazyNN a = LazyNN [a]

findNearestLazy :: Metric a => LazyNN a -> a -> (Double, Int)
findNearestLazy _ _ = (0, 0)

--data Metric m => NNFinderStruct m = BruteNNStruct (BruteNN m)
--                                  | LazyNNStruct (LazyNN m)
--
--findNearest brute@(BruteNN _) = findNearestBrute brute
--findNearest lazy@(LazyNN _) = findNearestLazy lazy

--findNearest :: Metric m => NNFinderStruct m -> m -> (Double, Int)
--findNearest (BruteNNStruct x) = findNearestBrute x
--findNearest (LazyNNStruct x) = findNearestLazy x

--class Metric m => NNFinder (f, m) where
--  findNearest :: f -> m -> (Double, Int)

--instance NNFinder (BruteNN m) where
--  findNearest struct query = findNearestBrute struct query

--class Metric m => NNFinder m where
--  findNearest :: NNFinderStruct m -> m -> (Double, Int)
--
--instance NNFinder L2Vector where
--  findNearest (BruteNNStruct bruteNN) query = findNearestBrute bruteNN query
--  findNearest (LazyNNStruct lazyNN) query = findNearestLazy lazyNN query

--data Metric m => NN m = NNFinder {
--  findNearest :: m -> (Double, Int)
--}
--
--mkFinder :: Metric m => Int -> Int -> NN m
--mkFinder x privateData = NNFinder {
--  findNearest = \_ -> (0, 0)
--  }



--class NNFinderStruct a
--
--instance Metric a => NNFinderStruct (BruteNN a)
--
--instance Metric a => NNFinderStruct (LazyNN a)

--class Metric b => NNFinder (NNFinder)  where
--  findNearest :: NNFinderStruct b => b -> (Double, Int)

--instance Metric a =>

--indexData = mkL2Vectors
--query = mkL2Vector
--
--bruteMatcher = BruteNN indexData
--bruteNearest = findNearest bruteMatcher query
--
--lshMatcher = LSHNN indexData
--lshNearest = findNearest lshMatcher query









-- TODO: Use typeclasses somehow to finish the below.
--class Metric a => NNFinder a
--class Metric a => NNFinderOps a where
--  findNearests :: NNFinder a => a -> Int -> a -> [(Int, Double)]
----  findNearest :: a -> (Int, Double)
----  findNearest = head . (findNearests 1)
