module RunLength (decode, encode) where

import Data.Char (isNumber)

decode :: String -> String
decode "" = ""
decode (x:xs) | isNumber x = replicate num (head rest) ++ decode (tail rest)
              | otherwise  = x : decode xs
 where
  num  = read (takeWhile isNumber (x : xs)) :: Int
  rest = dropWhile isNumber (x : xs)

encode :: String -> String
encode ""   = ""
encode text = concat $ foldr
  ( \x acc ->
    if length x > 1 then show (length x) : [head x] : acc else [head x] : acc
  )
  []
  splitted
  where splitted = splitString text

splitString :: String -> [String]
splitString "" = []
splitString (x:xs) =
  takeWhile (== x) (x : xs) : splitString (dropWhile (== x) (x : xs))
