module RandomUtil where

--------------------------------------------------------------------------------

import Control.Exception
import Data.List
import System.Random
import System.Random.Shuffle

--------------------------------------------------------------------------------

-- An infinite list of random generators.
randList :: StdGen -> [StdGen]
randList gen = gen : (randList $ snd $ next gen)

-- Private constructor.
data Permutation = Permutation [Int]

-- Verifies is actually a permutation.
mkPermutation :: [Int] -> Permutation
mkPermutation permutation =
 assert((sort permutation) == [0 .. (length permutation) - 1]) $
 Permutation permutation

mkPermutationWithGen :: StdGen -> Int -> Permutation
mkPermutationWithGen rand n = mkPermutation shuffled
 where shuffled = shuffle' [0 .. n - 1]  n rand

randomSubset :: StdGen -> Int -> [a] -> [a]
randomSubset gen n xs = assert (length shuffled >= n) $ take n shuffled
 where shuffled = shuffle' xs (length xs) gen
