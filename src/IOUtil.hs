module IOUtil where

--------------------------------------------------------------------------------

import qualified Data.Map as M
import System.FilePath.Find
import System.IO.Unsafe
import System.Process
import Text.Printf
import Text.Regex

import Base

--------------------------------------------------------------------------------

-- A facility for relying on externally implemented distance functions,
-- such as a python script. The first argument is the path to the executable
-- which computes the distance, and the following two arguments will be passed
-- verbatim into the stdin of the executable. The last argument is True for
-- verbose.
-- The executable should calculate
-- a distance between its two arguments, and the last thing it prints must be
-- the distance on its own line, followed by the newline character.
externalDistance :: String -> String -> String -> Bool -> IO Double
externalDistance distanceExecutable point0 point1 verbose =
 do let message = printf "calling: %s\narg0: %s\narg1: %s\n" distanceExecutable point0 point1
    let message' = if verbose then message else ""
    putStr message'
    result <- readProcess distanceExecutable [point0, point1] []
    let splitResult = splitRegex (mkRegex "\n") result
    let double = read $ splitResult !! ((length splitResult) - 2) :: Double
    let message = if verbose then printf "distance: %f\n" double else ""
    putStr message
    return $ double

externalDistanceMap :: String -> [String] -> Bool -> IO (DistanceMap String)
externalDistanceMap distanceExecutable points verbose =
 do let pairs = [(x, y) | x <- points, y <- points, y > x]
    distances <- sequence $ [unsafeInterleaveIO (externalDistance distanceExecutable x y verbose) | (x, y) <- pairs]
    return $ M.fromList $ zip pairs distances

-- Returns all occurrences of the given regular expression in the given string.
compiledRegexFindAllIn :: Regex -> Int -> String -> [String]
compiledRegexFindAllIn regex group string =
 case matchRegexAll regex string of
  Just (_, _, remainder, matches) ->
   (matches !! group) : compiledRegexFindAllIn regex group remainder
  Nothing -> []

regexFindAllIn :: String -> Int -> String -> [String]
regexFindAllIn regex group string =
 compiledRegexFindAllIn (mkRegex regex) group string

dataToDoubleLists :: String -> [[Double]]
dataToDoubleLists string = map lineToDoubles nonemptyLines
 where regex = "(^|[ ,\t\n]+)([0-9\\.]+)($|[ ,\t\n]+)"
       stringsToDoubles = map (\x -> read x :: Double)
       lineToDoubleStrings = regexFindAllIn regex 1
       lineToDoubles = stringsToDoubles . lineToDoubleStrings
       nonemptyLines = filter ((> 0) . length) $ lines string

-- Search a folder directory tree recursively for the given glob pattern.
findGlobRecursive glob directory = find always (fileName ~~? glob) directory



