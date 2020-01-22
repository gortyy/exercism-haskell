module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Enum, Eq, Show)

left :: Bearing -> Bearing
left North = West
left b     = pred b

right :: Bearing -> Bearing
right West = North
right b    = succ b

advance :: Robot -> Robot
advance (Robot North (x, y)) = Robot North (x, y + 1)
advance (Robot East  (x, y)) = Robot East (x + 1, y)
advance (Robot South (x, y)) = Robot South (x, y - 1)
advance (Robot West  (x, y)) = Robot West (x - 1, y)

data Robot = Robot Bearing (Integer, Integer) deriving Show

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ c) = c

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move robot [] = robot
move (Robot b (m, n)) (x:xs) | x == 'R' = move (Robot (right b) (m, n)) xs
                             | x == 'L' = move (Robot (left b) (m, n)) xs
                             | x == 'A' = move (advance (Robot b (m, n))) xs
