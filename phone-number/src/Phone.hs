module Phone (number) where

import Data.Char (isNumber)

number :: String -> Maybe String
number xs | valid     = Just processed
          | otherwise = Nothing
 where
  filtered  = filter isNumber xs
  processed = if head filtered == '1' then tail filtered else filtered
  valid =
    length processed
      == properLength
      && areExchangeCodesValid processed exchangeCodeOffsets


properLength :: Int
properLength = 10

exchangeCodeOffsets :: (Int, Int)
exchangeCodeOffsets = (0, 3)

areExchangeCodesValid :: String -> (Int, Int) -> Bool
areExchangeCodesValid phoneNumber offsets =
  phoneNumber !! fst offsets > '1' && phoneNumber !! snd offsets > '1'
