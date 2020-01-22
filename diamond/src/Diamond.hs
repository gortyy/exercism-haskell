module Diamond (diamond) where

import Data.Char (isLetter, ord)

diamond :: Char -> Maybe [String]
diamond x | isLetter x = Just (top ++ genLine x x : reverse top)
          | otherwise  = Nothing
  where top = map (`genLine` x) ['A' .. (pred x)]

genLine :: Char -> Char -> String
genLine from to | from == 'A' = wrap [from] spacesTo
                | otherwise   = wrap (wrap spaceFromA [from]) spacesTo
 where
  spacesTo   = replicateSpaces (ord to - ord from)
  spaceFromA = replicateSpaces (2 * (ord from - ord 'A') - 1)

wrap :: String -> String -> String
wrap inside coating = coating ++ inside ++ coating

replicateSpaces :: Int -> String
replicateSpaces = concat . flip replicate " "
