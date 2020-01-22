module Acronym (abbreviate) where

import Data.Char (toUpper, toLower, isUpper)

abbreviate :: String -> String
abbreviate xs = filter isUpper $ toUpperFirst $ replaceChar '-' xs

toUpperFirst :: String -> String
toUpperFirst xs =
  concatMap (\x -> processAllCaps (toUpper (head x) : tail x)) $ words xs

processAllCaps :: String -> String
processAllCaps xs =
  if all isUpper xs then head xs : map toLower (tail xs) else xs

replaceChar :: Char -> String -> String
replaceChar c = map (\x -> if x == c then ' ' else x)
