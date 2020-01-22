module Proverb(recite) where


recite :: [String] -> String
recite []     = ""
recite [x   ] = "And all for the want of a " ++ x ++ "."
recite (x:xs) = go (x : xs) x
 where
  go (y:ys) first
    | null ys
    = recite [first]
    | otherwise
    = "For want of a "
      ++ y
      ++ " the "
      ++ head ys
      ++ " was lost.\n"
      ++ go ys first
