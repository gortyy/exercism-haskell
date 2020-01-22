module Yacht (yacht, Category(..)) where

import Data.Set (fromList, toList)
import Data.List (sort)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht
              deriving (Eq, Enum)


unique :: Ord a => [a] -> [a]
unique = toList . fromList

howMany :: Int -> [Int] -> Int
howMany x xs = length (filter (== x) xs)

onesToSixes :: Category -> [Int] -> Int
onesToSixes category xs = ord * length (filter (== ord) xs)
  where ord = fromEnum category + 1

yacht :: Category -> [Int] -> Int
yacht category xs
  | category `elem` [Ones, Twos, Threes, Fours, Fives, Sixes] = onesToSixes
    category
    xs
  | category == Yacht = if all (== head xs) xs then 50 else 0
  | category == Choice = sum xs
  | category == BigStraight = if sorted == [2, 3, 4, 5, 6] then 30 else 0
  | category == LittleStraight = if sorted == [1, 2, 3, 4, 5] then 30 else 0
  | otherwise = 0
  where sorted = sort xs

yacht FullHouse xs
  | length u > 2
  = 0
  | (fstC == 2 && sndC == 3) || (fstC == 3 && sndC == 2)
  = fstC * first + sndC * second
  | otherwise
  = 0
 where
  u      = unique xs
  first  = head u
  second = head (tail u)
  fstC   = howMany first xs
  sndC   = howMany second xs
yacht FourOfAKind xs
  | length u > 2 = 0
  | length u == 1 = 4 * first
  | (fstC == 1 && sndC == 4) || (fstC == 4 && sndC == 1) = if fstC > sndC
    then 4 * first
    else 4 * second
  | otherwise = 0
 where
  u      = unique xs
  first  = head u
  second = head (tail u)
  fstC   = howMany first xs
  sndC   = howMany second xs
