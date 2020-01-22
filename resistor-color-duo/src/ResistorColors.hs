module ResistorColors (Color(..), value) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Enum)

value :: (Color, Color) -> Int
value (a, b) = 10 * fromColor a + fromColor b

fromColor :: Color -> Int
fromColor Black = 0
fromColor c     = 1 + fromColor (pred c)
