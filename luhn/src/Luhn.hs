module Luhn (isValid) where

isValid :: String -> Bool
isValid n
  | length nums <= 1 = False
  | otherwise = rem (sum . doubleEverySecondDigit . reverse $ nums) 10 == 0
  where nums = toNums n

toNums :: String -> [Int]
toNums xs = [ read [x] :: Int | x <- xs, x /= ' ' ]

doubleEverySecondDigit :: (Ord a, Num a) => [a] -> [a]
doubleEverySecondDigit (x:x':xs) = x : doubled : doubleEverySecondDigit xs
 where
  doubled = if d > 9 then d - 9 else d
  d       = 2 * x'
doubleEverySecondDigit x = x
