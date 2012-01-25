module Main (
  main
) where

--------------------------------------------------------------------------------

import Base
import LSH
import IOUtil
import Util

import BenchmarkUtil

--------------------------------------------------------------------------------

main = do
--  let train = [mkBitVector [False, False], mkBitVector [True, False]]
--  let test = mkBitVector [True, True]
--  let rand = mkStdGen 0
--  let ret = charikarLists (rand, 10) train 2 (1, test)
--
--  putStrLn $ show ret

  text <- readFile "/home/eric/Dropbox/haskell/mlsh/data/breast_cancer.data"
  let vectors = dataToDoubleLists text

--  let test = head vectors
--  let train = tail vectors
--
--  let ret = rlshNNAuto l2Distance train (1, test)

  let results = benchmark mkRLSHNNAuto useRLSHNNAuto l2Distance vectors

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

