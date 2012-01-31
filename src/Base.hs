module Base where

--------------------------------------------------------------------------------

import Control.Exception
import Data.List
import qualified Data.Map as M
import Data.Ord
import qualified Data.Vector.Unboxed as V

--------------------------------------------------------------------------------

type Distance a = a -> a -> Double
type NNQuery a = (Int, a)
type NNResponse = [(Double, Int)]

type NNConstructor a b = Distance a -> [a] -> b
type NNRunner a b = b -> NNQuery a -> NNResponse
type NNMethod a = Distance a -> [a] -> NNQuery a -> NNResponse

type BruteNNStruct a = (Distance a, [a])

mkBruteNN :: Distance a -> [a] -> BruteNNStruct a
mkBruteNN distance points = (distance, points)

useBruteNN :: BruteNNStruct a -> NNQuery a -> NNResponse
useBruteNN (distance, points) (num, query) = take num sorted
 where queryDistance = distance query
       distances = map queryDistance points
       distanceWithPoints = zip distances [0 ..]
       sorted = sort distanceWithPoints

bruteNN :: Distance a -> [a] -> NNQuery a -> NNResponse
bruteNN distance points = useBruteNN (mkBruteNN distance points)

--------------------------------------------------------------------------------

l2Distance :: Floating a => [a] -> [a] -> a
l2Distance xs ys = sqrt $ sum squaredDifferences
 where squaredDifferences = map (^ 2) $ zipWith (-) xs ys

l2DistanceUnboxed :: V.Vector Double -> V.Vector Double -> Double
l2DistanceUnboxed xs ys = sqrt $ V.sum squaredDifferences
 where squaredDifferences = V.map (^ 2) $ V.zipWith (-) xs ys

--------------------------------------------------------------------------------

type DistanceMap a = M.Map (a, a) Double

distanceToMap :: Ord a => Distance a -> [a] -> DistanceMap a
distanceToMap distance train = M.fromList pairs
 where pairs = [((x, y), distance x y) | x <- train, y <- train, y > x]

mapToDistance :: Ord a => DistanceMap a -> Distance a
mapToDistance distanceMap x y
 -- Distances must obey the identity of indiscernibles.
 | x == y = 0
 -- Distances must by symmetric.
 | x > y = mapToDistance distanceMap y x
 | otherwise = case M.lookup (x, y) distanceMap of
                Just distance -> distance
                Nothing -> error ""

