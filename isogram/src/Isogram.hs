module Isogram (isIsogram) where

import Data.Char (isLetter, toLower)
import Data.Set (fromList)

isIsogram :: String -> Bool
isIsogram xs = length filtered == length (fromList filtered)
  where filtered = map toLower $ filter isLetter xs
