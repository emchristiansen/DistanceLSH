module LSH where

--------------------------------------------------------------------------------

import Control.Exception
import Data.List
import Data.Ord
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
import System.Random

import Base
import RandomUtil
import Util

--------------------------------------------------------------------------------

type CharikarListParamsStatic = StdGen
type CharikarListStruct = (Permutation, B.Vector (BitVector, Int))
type Radius = Int
type CharikarParamsQuery = Radius
type HNNResponse = [(Int, Int)]

mkCharikarList :: CharikarListParamsStatic -> [BitVector] -> CharikarListStruct
mkCharikarList rand train = (permutation, B.fromList $ sort $ zip permuted [0 ..])
 where permutation = mkPermutationWithGen rand $ numBits $ head train
       permuted = map (permute permutation) train

useCharikarList :: CharikarListStruct -> CharikarParamsQuery ->
                   NNQuery BitVector -> HNNResponse
useCharikarList (permutation, sorted) radius (num, query) =
  take num $ sort $ zip distances indices
 where
  midpoint = binarySearch sorted (permute permutation query, -1)
  neighbors = truncateSlice (midpoint - radius) (midpoint + radius + 1) sorted
  (bitvectors, indices) = unzip $ B.toList $ neighbors
  distances = map (hammingDistance query) bitvectors

charikarList :: CharikarListParamsStatic -> [BitVector] -> CharikarParamsQuery ->
                NNQuery BitVector -> HNNResponse
charikarList rand train = useCharikarList (mkCharikarList rand train)

--------------------------------------------------------------------------------

type NumPermutations = Int
type CharikarListsParamsStatic = (StdGen, NumPermutations)
type CharikarListsStruct = [CharikarListStruct]

mkCharikarLists :: CharikarListsParamsStatic -> [BitVector] -> CharikarListsStruct
mkCharikarLists (rand, numPermutations) train =
  map ($ train) $ map mkCharikarList rands
 where rands = take numPermutations $ randList rand

useCharikarLists :: CharikarListsStruct -> CharikarParamsQuery ->
                    NNQuery BitVector -> HNNResponse
useCharikarLists lists radius (num, query) = take num $ nub $ sort candidates
 where needsList list = useCharikarList list radius (2 * radius + 1, query)
       candidates = concatMap needsList lists

charikarLists :: CharikarListsParamsStatic -> [BitVector] -> CharikarParamsQuery ->
                 NNQuery BitVector -> HNNResponse
charikarLists params train = useCharikarLists (mkCharikarLists params train)

--------------------------------------------------------------------------------

type Dimension = Int
type Projection a = [(a, a)]
type CharikarParamsStatic = (StdGen, NumPermutations, Dimension)
type LSHNNStruct a = (Distance a, Projection a, CharikarListsStruct)

project :: Distance a -> Projection a -> a -> BitVector
project distance projection point =
  mkBitVector [distance point left < distance point right | (left, right) <- projection]

mkLSHNN :: CharikarParamsStatic -> Distance a -> [a] -> LSHNNStruct a
mkLSHNN (rand, numPermutations, dimension) distance train =
  (distance, projection, lists)
 where
  projection =
   let randomSubset' = randomSubset rand (2 * dimension) train
       (lefts, rights) = splitAt dimension randomSubset'
   in zip lefts rights
  projectedTrain = map (project distance projection) train
  lists = mkCharikarLists (rand, numPermutations) projectedTrain

useLSHNN :: LSHNNStruct a -> CharikarParamsQuery -> NNQuery a -> HNNResponse
useLSHNN (distance, projection, lists) radius (num, query) =
  take num $ nub $ sort candidates
 where projectedQuery = project distance projection query
       takeAll = (length lists) * (2 * radius + 1)
       candidates = useCharikarLists lists radius (takeAll, projectedQuery)

lshNN :: CharikarParamsStatic -> Distance a -> [a] -> CharikarParamsQuery ->
         NNQuery a -> HNNResponse
lshNN params distance train = useLSHNN (mkLSHNN params distance train)

--------------------------------------------------------------------------------

type NumRescore = Int
type RLSHNNStruct a = (LSHNNStruct a, B.Vector a)
type RLSHParamsQuery = (Radius, NumRescore)

mkRLSHNN :: CharikarParamsStatic -> Distance a -> [a] -> RLSHNNStruct a
mkRLSHNN params distance train = (mkLSHNN params distance train, B.fromList train)

useRLSHNN :: RLSHNNStruct a -> RLSHParamsQuery -> NNQuery a -> NNResponse
useRLSHNN (lsh@(distance, _, _), train) (radius, numRescore) (num, query) =
  take num $ nub $ sort $ zip distances candidates
 where candidates = map snd $ useLSHNN lsh radius (numRescore, query)
       distances = map (distance query) $ map ((B.!) train) candidates

rlshNN :: CharikarParamsStatic -> Distance a -> [a] -> RLSHParamsQuery ->
          NNQuery a -> NNResponse
rlshNN params distance train = useRLSHNN (mkRLSHNN params distance train)

mkRLSHNNAuto :: Distance a -> [a] -> RLSHNNStruct a
mkRLSHNNAuto = mkRLSHNN params
 where rand = mkStdGen 0
       dimension = 8
       numPermutations = 16
--       dimension = 64
--       numPermutations = 16
       params = (rand, numPermutations, dimension)

useRLSHNNAuto :: RLSHNNStruct a -> NNQuery a -> NNResponse
useRLSHNNAuto struct@((_, projection, lists), _) = useRLSHNN struct params
 where dimension = length projection
       numPermutations = length lists
       numRescore = 2 * dimension
       radius = ceiling $ (fromIntegral numRescore) / (fromIntegral numPermutations)
       params = (radius, numRescore)

rlshNNAuto :: Distance a -> [a] -> NNQuery a -> NNResponse
rlshNNAuto distance train = useRLSHNNAuto (mkRLSHNNAuto distance train)

