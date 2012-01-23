module Util (
  dataToVectors,
  regexFindAllIn,
  slice,
  binarySearch,
  shuffle,
  randomSubset,
  truncateSlice,
  mean,
  fractionTrue,
  BitVector,
  mkBitVector,
  Permutation,
  mkPermutation,
  mkPermutationWithGen,
  numBits,
  permute,
  pair,
  randList,
  hammingDistance
) where

--------------------------------------------------------------------------------

import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.List
import qualified Data.Vector as V
import System.Random
import Text.Regex

import Base

--------------------------------------------------------------------------------

randList :: StdGen -> [StdGen]
randList gen = gen : (randList $ snd $ next gen)

pair :: a -> b -> (a, b)
pair x y = (x, y)

data BitVector = BitVector (V.Vector Bool) deriving (Eq, Ord, Show)

mkBitVector :: [Bool] -> BitVector
mkBitVector = BitVector . V.fromList

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

hammingDistance :: BitVector -> BitVector -> Int
hammingDistance (BitVector xs) (BitVector ys) = V.sum disagreements
 where disagreements = V.map boolToInt $ V.zipWith (/=) xs ys

-- Private constructor.
data Permutation = Permutation [Int]

-- Verifies is actually a permutation.
mkPermutation :: [Int] -> Permutation
mkPermutation permutation =
  assert(sorted == [0 .. (length permutation) - 1]) $
  Permutation permutation
  where
    sorted = sort permutation

mkPermutationWithGen :: StdGen -> Int -> Permutation
mkPermutationWithGen rand n = mkPermutation shuffled
 where shuffled = fst $ shuffle [0 .. n - 1] rand

permute :: Permutation -> BitVector -> BitVector
permute (Permutation permutation) (BitVector bits) =
  mkBitVector [bits V.! i | i <- permutation]

numBits :: BitVector -> Int
numBits (BitVector bits) = V.length bits


mean :: Fractional a => [a] -> a
mean xs = (sum xs) / (fromIntegral $ length xs)

fractionTrue :: [Bool] -> Double
fractionTrue bs = numTrue / numTotal
  where numTrue = fromIntegral $ length $ filter id bs
        numTotal = fromIntegral $ length bs

truncateSlice :: Int -> Int -> V.Vector a -> V.Vector a
truncateSlice start end v = V.slice start' extent v
  where
    start' = max 0 start
    end' = min (V.length v) end
    extent = end' - start'


randomSubset :: StdGen -> Int -> [a] -> ([a], StdGen)
randomSubset gen n xs = (take n shuffled, gen')
  where
    (shuffled, gen') = shuffle xs gen

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

slice :: [a] -> Int -> Int -> [a]
slice xs from to = take (to - from) (drop from xs)

dataToVectors :: String -> [[Double]]
dataToVectors string =
    doubleLists
  where
    regex = "(^|[ ,\t\n]+)([0-9\\.]+)($|[ ,\t\n]+)"
    stringsToDoubles = map (\x -> read x :: Double)
    lineToDoubleStrings = regexFindAllIn regex 1
    lineToDoubles = stringsToDoubles . lineToDoubleStrings
    nonemptyLines = filter ((> 0) . length) $ lines string
    doubleLists = map lineToDoubles nonemptyLines

-- Returns all occurrences of the given regular expression in the given string.
-- regular expression -> capture group -> string to be parsed -> result
compiledRegexFindAllIn :: Regex -> Int -> String -> [String]
compiledRegexFindAllIn regex group string =
  case matchRegexAll regex string of
    Just (_, _, remainder, matches) ->
      (matches !! group) : compiledRegexFindAllIn regex group remainder
    Nothing -> []

regexFindAllIn :: String -> Int -> String -> [String]
regexFindAllIn regex group string =
  compiledRegexFindAllIn (mkRegex regex) group string

binarySearch :: Ord a => V.Vector a -> a -> Int
binarySearch sorted query = helper 0 (V.length sorted)
  where
    -- Invariant: The insert-sort index is always included
    -- in [min, max] (inclusive).
    helper min max =
      if max == min + 1
      then if query < sorted V.! min then min else max
      else
        let mid = (max + min) `div` 2 in
        if query < sorted V.! mid then helper min mid
        else helper mid max

