module Main (
  main
) where

--------------------------------------------------------------------------------

import qualified Data.Vector.Unboxed as V
import System.Environment

import Base
import Control.Exception
import LSH
import IOUtil
--import Util

import BenchmarkUtil

import qualified Data.Map as M


--------------------------------------------------------------------------------


main = do let pat = "*_000*.png"
          let distanceExecutable = "/home/eric/Dropbox/python/imagedistance/src/l2distance.py"
--          let dir = "/home/eric/macroot/Volumes/H1/MultiPIE/data/session01/multiview"
--          let dir = "/home/eric/Dropbox/python/extract_image_patches/data"
          let dir = "/home/eric/delme"
          files <- findGlobRecursive pat dir

          distanceMap <- externalDistanceMap distanceExecutable files True
          let distance = mapToDistance distanceMap

--          let f0 = files !! 0
--          let f1 = files !! 1
--
--          putStrLn $ show $ distance f0 f1
--          putStrLn $ "here"
--          putStrLn $ show $ distance f0 f1
--          putStrLn $ show $ distance f0 f0

--          let results = benchmark mkBruteNN useBruteNN distance files
          let results = benchmark mkRLSHNNAuto useRLSHNNAuto distance files

          putStrLn $ show results

--main :: IO ()
--main = do
--
--  text <- readFile "/home/eric/Dropbox/haskell/mlsh/data/letter-recognition.data"
--  let vectors = map V.fromList $ take 500 $ dataToDoubleLists text
--
--  let distanceMap = distanceToMap l2DistanceUnboxed vectors
--  let distance = mapToDistance distanceMap
--
--
--  let results = benchmark mkRLSHNNAuto useRLSHNNAuto distance vectors
--
--  putStrLn $ show results

