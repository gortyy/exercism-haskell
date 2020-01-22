module Pangram (isPangram) where

import Data.Set (member, fromList)
import Data.Char (toLower)

isPangram :: String -> Bool
isPangram xs = all (`member` fromList (map toLower xs)) ['a' .. 'z']
