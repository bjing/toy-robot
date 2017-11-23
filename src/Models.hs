module Models where

type Coord = Integer

data Position = Position {
    x :: Coord
  , y :: Coord
} deriving (Show)

data Direction = NORTH
  | SOUTH
  | WEST
  | EAST
  deriving (Show)

data Robot =
  Robot {
      position :: Position
    , facing :: Direction
  }
  deriving (Show)

data Command = PLACE Position Direction
  | MOVE
  | LEFT
  | RIGHT
  | REPORT
  deriving (Show)
