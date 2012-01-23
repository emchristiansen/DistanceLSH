module IOUtil where

--------------------------------------------------------------------------------

import Text.Regex

--------------------------------------------------------------------------------

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



