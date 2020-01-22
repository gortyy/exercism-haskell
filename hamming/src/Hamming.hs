module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs == length ys = Just $ sum $ zipWith
    (\x y -> if x == y then 0 else 1)
    xs
    ys
  | otherwise = Nothing
