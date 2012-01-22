module Main (
  main
) where

--------------------------------------------------------------------------------

import Base
import LSH
import Util

import BenchmarkUtil

--------------------------------------------------------------------------------

main = do
--  let b1 = mkBitVector [False, False, True]
--  let b2 = mkBitVector [False, True, False]
--  putStrLn $ show $ b1 < b2

  text <- readFile "/home/eric/Dropbox/haskell/mlsh/data/iris.data"
  let vectors = dataToVectors text

  let results = benchmarkLSH vectors

  putStrLn $ show results

--  let brute = BruteNN vectors
--  let lsh = mkRescoreNN vectors 32 8
--
--  let v10 = vectors !! 10
--
--  let radius = 2
--  let numRescore = 8
--  let (dist, ind) = nearestRescoreNN lsh radius numRescore v10
--  putStrLn $ show ind
--  putStrLn $ show dist

--  let asLines = lines text
--  let line0 = asLines !! 0
--  let padded = " " ++ line0 ++ " "
--  let nums = regexFindAllIn "(^|[ ,\t\n]+)([0-9\\.]+)($|[ ,\t\n]+)" 1 padded
--  sequence_ $ map putStrLn nums
--  sequence_ $ map (putStrLn . show) (dataToVectors text)

