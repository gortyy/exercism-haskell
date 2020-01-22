module Bob (responseFor) where

import Data.Char (isAlphaNum, isUpper, isLetter)

responseFor :: String -> String
responseFor xs | isSilence                = "Fine. Be that way!"
               | isAllUpper && isQuestion = "Calm down, I know what I'm doing!"
               | isAllUpper               = "Whoa, chill out!"
               | isQuestion               = "Sure."
               | otherwise                = "Whatever."
 where
  trimmed    = filter (/= ' ') xs
  isAllUpper = any isLetter trimmed && all isUpper (filter isLetter trimmed)
  isQuestion = last trimmed == '?'
  isSilence  = null trimmed || (not (any isAlphaNum trimmed) && not isQuestion)
