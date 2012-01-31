module Util where

--------------------------------------------------------------------------------

import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V

import Base
import IOUtil
import RandomUtil

--------------------------------------------------------------------------------

pair :: a -> b -> (a, b)
pair x y = (x, y)

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

mean :: Fractional a => [a] -> a
mean xs = (sum xs) / (fromIntegral $ length xs)

fractionTrue :: [Bool] -> Double
fractionTrue bs = numTrue / numTotal
 where numTrue = fromIntegral $ length $ filter id bs
       numTotal = fromIntegral $ length bs

truncateSlice :: Int -> Int -> B.Vector a -> B.Vector a
truncateSlice start end v = B.slice start' extent v
 where start' = max 0 start
       end' = min (B.length v) end
       extent = end' - start'

data BitVector = BitVector (V.Vector Bool) deriving (Eq, Ord, Show)

mkBitVector :: [Bool] -> BitVector
mkBitVector = BitVector . V.fromList

hammingDistance :: BitVector -> BitVector -> Int
hammingDistance (BitVector xs) (BitVector ys) = V.sum disagreements
 where disagreements = V.map boolToInt $ V.zipWith (/=) xs ys

permute :: Permutation -> BitVector -> BitVector
permute (Permutation permutation) (BitVector bits) =
  mkBitVector [bits V.! i | i <- permutation]

numBits :: BitVector -> Int
numBits (BitVector bits) = V.length bits

binarySearch :: Ord a => B.Vector a -> a -> Int
binarySearch sorted query = helper 0 (B.length sorted)
 where
   -- Invariant: The insert-sort index is always included
   -- in [min, max] (inclusive).
  helper min max =
   if max == min + 1
   then if query < sorted B.! min then min else max
   else
    let mid = (max + min) `div` 2 in
    if query < sorted B.! mid then helper min mid
    else helper mid max

