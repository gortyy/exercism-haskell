module CryptoSquare (encode) where

import Data.Char (toLower, isAlphaNum)

encode :: String -> String
encode xs | null xs   = ""
          | otherwise = unwords $ split len (addSpaces $ chunks normalized)
  where normalized = [ toLower c | c <- xs, isAlphaNum c ]
        len = chunkLen normalized


chunkLen :: String -> Int
chunkLen = round . sqrt . fromIntegral . length

every :: Int -> String -> String
every _ "" = ""
every n (x:xs) = x: every n (drop (n - 1) xs)

chunksHelper :: Int -> String -> String
chunksHelper _ "" = ""
chunksHelper n (x:xs) = every n (x:xs) ++ chunksHelper n xs

chunks :: String -> String
chunks xs = take (length xs) $ chunksHelper (chunkLen xs) xs

addSpaces :: String -> String
addSpaces "" = ""
addSpaces xs = xs ++ concat (replicate lenDiff " ")
  where lenDiff = chunkLen xs ^ 2 - length xs

split :: Int -> String -> [String]
split _ "" = []
split n xs = take n xs : split n (drop n xs)
