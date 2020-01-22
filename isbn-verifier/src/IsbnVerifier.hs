module IsbnVerifier (isbn) where

import Data.Char (isNumber, digitToInt)

isbn :: String -> Bool
isbn xs | validLength && validLast = formula i
        | otherwise                = False
 where
  i           = clean xs
  validLength = length i == validIsbnLength
  validLast   = isNumber (last xs) || last xs == 'X'

validIsbnLength :: Int
validIsbnLength = 10

clean :: String -> String
clean "" = ""
clean xs = filter isNumber (init xs) ++ [last xs]

calculate :: String -> Int -> Int
calculate "" _ = 0
calculate (x:xs) index
  | x == 'X'  = 10
  | otherwise = digitToInt x * index + calculate xs (index - 1)

formula :: String -> Bool
formula x = calculate x validIsbnLength `mod` 11 == 0
