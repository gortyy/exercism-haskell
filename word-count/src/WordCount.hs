module WordCount (wordCount) where

import Data.Char (toLower, isLetter, isNumber)
import Data.Set (fromList)

wordCount :: String -> [(String, Int)]
wordCount xs = foldr (\word acc -> (word, count word wds) : acc) [] uniqWds
 where
  uniqWds = fromList wds
  wds     = map normalizeWord $ words $ map processChar xs

count :: String -> [String] -> Int
count _    []     = 0
count word (x:xs) = count word xs + if x == word then 1 else 0

processChar :: Char -> Char
processChar x =
  if isLetter x || isNumber x || x == '\'' then toLower x else ' '

normalizeWord :: String -> String
normalizeWord ""     = ""
normalizeWord (x:xs) = if x == '\'' && last xs == '\'' then init xs else x : xs
