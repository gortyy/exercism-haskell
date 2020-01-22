module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss =
  [ x
  | x <- xss
  , sort [ toLower c | c <- x ] == sort lxs
  , [ toLower c | c <- x ] /= lxs
  ]
  where lxs = [ toLower c | c <- xs ]
