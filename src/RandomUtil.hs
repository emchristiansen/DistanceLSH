module RandomUtil where

--------------------------------------------------------------------------------

import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.List
import System.Random

--------------------------------------------------------------------------------

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
 where shuffled = fst $ shuffle [0 .. n - 1] rand

randomSubset :: StdGen -> Int -> [a] -> ([a], StdGen)
randomSubset gen n xs = (take n shuffled, gen')
 where (shuffled, gen') = shuffle xs gen

-- Copy-pasted from http://www.haskell.org/haskellwiki/Random_shuffle
shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs




