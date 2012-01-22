module LSH (
  BitVector,
  mkBitVector,
  CharikarList,
  mkCharikarList,
  mkRescoreNN,
  nearestRescoreNN,
  nearestRescoreNNAuto
) where

--------------------------------------------------------------------------------

import Control.Exception
import Data.List
import Data.Ord
import qualified Data.Vector as V
import System.Random

import Base
import Util

--------------------------------------------------------------------------------

-- The default ordering should be lexicographic.
data BitVector = BitVector (V.Vector Bool) deriving (Eq, Ord, Show)

mkBitVector :: [Bool] -> BitVector
mkBitVector = BitVector . V.fromList

-- Private constructor.
data Permutation = Permutation [Int]

-- Verifies is actually a permutation.
mkPermutation :: [Int] -> Permutation
mkPermutation permutation =
  assert(sorted == [0 .. length permutation]) $
  Permutation permutation
  where
    sorted = sort permutation

instance Metric BitVector where
  distance (BitVector xs) (BitVector ys) =
    assert ((V.length xs) == (V.length ys)) $
    V.foldl incrementWhenDifferent 0 differ
    where
      incrementWhenDifferent count different =
        count + if different then 1 else 0
      differ = V.zipWith (/=) xs ys

permute :: Permutation -> BitVector -> BitVector
permute (Permutation permutation) (BitVector bits) =
  mkBitVector [bits V.! i | i <- permutation]

numBits :: BitVector -> Int
numBits (BitVector bits) = V.length bits

data Metric m => Hasher m = Hasher [(m, m)] deriving Show

mkHasherRandom :: Metric m => [m] -> Int -> (Hasher m)
mkHasherRandom points dimension = Hasher (zip lefts rights)
  where
    randGen = mkStdGen 0
    (subset, _) = randomSubset randGen (2 * dimension) points
    lefts = take dimension subset
    rights = drop dimension subset

hash :: Metric m => Hasher m -> m -> BitVector
hash (Hasher pairs) query =
  mkBitVector [queryDistance left < queryDistance right | (left, right) <- pairs]
  where
    queryDistance = distance query

-- The constructor should be private.
data CharikarList = CharikarList (V.Vector (BitVector, Int)) Permutation

mkCharikarList :: [BitVector] -> Permutation -> CharikarList
mkCharikarList bitVectors permutation = CharikarList sorted permutation
  where
    permuted = map (permute permutation) bitVectors
    permutedWithIndex = zip permuted [0 ..]
    sorted = V.fromList $ sortBy (comparing fst) permutedWithIndex

nearestCharikarList :: CharikarList -> Int -> BitVector -> [Int]
nearestCharikarList (CharikarList sorted permutation) radius query =
  V.toList $ V.map snd $ truncateSlice (midpoint - radius) (midpoint + radius + 1) sorted
  where
    permuted = permute permutation query
    midpoint = binarySearch sorted (permuted, -1)

data CharikarLists = CharikarLists [CharikarList]

mkCharikarLists :: [BitVector] -> Int -> CharikarLists
mkCharikarLists xs numPermutations =
  CharikarLists $ map (mkCharikarList xs) permutations
  where
    randGen = mkStdGen 0
    numShuffle = numBits . head $ xs
    permutationGen gen =
      let (permutation, gen') = shuffle [0 .. numShuffle - 1] randGen
      in (Permutation permutation) : (permutationGen gen')
    permutations = take numPermutations $ permutationGen randGen

nearestCharikarLists :: CharikarLists -> Int -> BitVector -> [Int]
nearestCharikarLists (CharikarLists lists) radius query =
  nub $ concatMap (\c -> nearestCharikarList c radius query) lists

data Metric m => CharikarNN m = CharikarNN (V.Vector BitVector) (Hasher m) CharikarLists

mkCharikarNN :: Metric m => [m] -> Int -> Int -> CharikarNN m
mkCharikarNN points hashDimension numCharikarLists =
  CharikarNN (V.fromList hashedPoints) hasher charikarLists
  where
    hasher = mkHasherRandom points hashDimension
    hashedPoints = map (hash hasher) points
    charikarLists = mkCharikarLists hashedPoints numCharikarLists

nearestCharikarNN :: Metric m => CharikarNN m -> Int -> Int -> m -> [Int]
nearestCharikarNN (CharikarNN points hasher lists) radius numNeighbors query =
  take numNeighbors $ map snd $ sort $ zip distances candidates
    where
      hashedQuery = hash hasher query
      candidates = nearestCharikarLists lists radius hashedQuery
      distances = map (distance hashedQuery) $ map (points V.!) candidates

data Metric m => RescoreNN m = RescoreNN (V.Vector m) (CharikarNN m)

mkRescoreNN :: Metric m => [m] -> Int -> Int -> RescoreNN m
mkRescoreNN points hashDimension numCharikarLists =
  RescoreNN (V.fromList points) (mkCharikarNN points hashDimension numCharikarLists)

nearestRescoreNN :: Metric m => RescoreNN m -> Int -> Int -> m -> (Double, Int)
nearestRescoreNN (RescoreNN points matcher) radius numRescore query =
  minimum $ zip distances candidates
  where
    candidates = nearestCharikarNN matcher radius numRescore query
    distances = map (distance query) $ map (points V.!) candidates

nearestRescoreNNAuto :: Metric m => RescoreNN m -> m -> (Double, Int)
nearestRescoreNNAuto matcher = nearestRescoreNN matcher radius numRescore
  where
    numRescore = 64
    radius = 8
