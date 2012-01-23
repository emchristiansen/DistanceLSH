module Base where

--------------------------------------------------------------------------------

import Control.Exception
import Data.List
import Data.Ord

--------------------------------------------------------------------------------

type Distance a = a -> a -> Double

type NNQuery a = (Int, a)

type NNResponse = [(Double, Int)]

type NNMethod a = Distance a -> [a] -> NNQuery a -> NNResponse

bruteNN :: Distance a -> [a] -> NNQuery a -> NNResponse
bruteNN distance points (num, query) = take num sorted
  where queryDistance = distance query
        distances = map queryDistance points
        distanceWithPoints = zip distances [0 ..]
        sorted = sort distanceWithPoints

l2Distance :: Floating a => [a] -> [a] -> a
l2Distance xs ys = sqrt $ sum squaredDifferences
  where squaredDifferences = map (^ 2) $ zipWith (-) xs ys

