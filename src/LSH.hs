module LSH where

--------------------------------------------------------------------------------

import Control.Exception
import Data.List
import Data.Ord
import qualified Data.Vector as V
import System.Random

import Base
import Util

--------------------------------------------------------------------------------

type CharikarListParamsStatic = StdGen
type CharikarListsParamsStatic = (StdGen, NumPermutations)
type CharikarParamsStatic = (StdGen, NumPermutations, Dimension)
type LSHParamsQuery = Radius
type RLSHParamsQuery = (Radius, NumRescore)

type HNNResponse = [(Int, Int)]

type Dimension = Int
type NumPermutations = Int
type Radius = Int
type NumRescore = Int

charikarList :: CharikarListParamsStatic -> [BitVector] -> LSHParamsQuery ->
                NNQuery BitVector -> HNNResponse
charikarList rand train radius (num, query) = sort $ zip distances indices
 where
  numBits' = numBits $ head train
  permute' = permute (mkPermutationWithGen rand numBits')
  sorted = V.fromList $ sort $ zip (map permute' train) [0 ..]
  midpoint = binarySearch sorted (permute' query, -1)
  neighbors = truncateSlice (midpoint - radius) (midpoint + radius + 1) sorted
  (bitvectors, indices) = unzip $ V.toList $ neighbors
  distances = map (hammingDistance query) bitvectors

charikarLists :: CharikarListsParamsStatic -> [BitVector] -> LSHParamsQuery ->
                 NNQuery BitVector -> HNNResponse
charikarLists (rand, numPermutations) train radius (num, query) =
 take num $ nub $ sort $ candidates
 where rands = take numPermutations $ randList rand
       lists = map (\r -> charikarList r train) rands
       keepAll = 2 * radius + 1
       query' = (keepAll, query)
       candidates = concatMap (\l -> l radius query') lists

lshNN :: CharikarParamsStatic -> Distance a -> [a] -> LSHParamsQuery ->
         NNQuery a -> HNNResponse
lshNN (rand, numPermutations, dimension) distance train radius (num, query) =
 charikar
 where
  project point =
   let randomSubset' = fst $ randomSubset rand (2 * dimension) train
       (lefts, rights) = splitAt dimension randomSubset'
       projection = zip lefts rights
   in mkBitVector [distance point left < distance point right | (left, right) <- projection]
  charikar =
   let projected = map project train
       projectedQuery = project query
   in charikarLists (rand, numPermutations) projected radius (num, projectedQuery)

rlshNN :: CharikarParamsStatic -> Distance a -> [a] -> RLSHParamsQuery ->
          NNQuery a -> NNResponse
rlshNN trainParams distance train (radius, numRescore) (num, query) =
  take num $ sort $ zip distances candidates
 where lsh = lshNN trainParams distance train radius (numRescore, query)
       candidates = map snd lsh
       train' = V.fromList train
       distances = map (distance query) $ map ((V.!) train') candidates

rlshNNAuto :: Distance a -> [a] -> NNQuery a -> NNResponse
rlshNNAuto distance train =
 let
  rand = mkStdGen 0
  dimension = 32
  numPermutations = 8
  numRescore = 2 * dimension
  radius = ceiling $ (fromIntegral numRescore) / (fromIntegral numPermutations)
 in
  rlshNN (rand, numPermutations, dimension) distance train (radius, numRescore)

