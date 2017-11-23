module Models where

type Coord = Integer

data Position = Position {
    x :: Coord
  , y :: Coord
} deriving (Show, Eq)

data Direction = NORTH
  | SOUTH
  | WEST
  | EAST
  deriving (Show, Eq)

data Robot =
  Robot {
      position :: Position
    , facing :: Direction
  }
  deriving (Show, Eq)

data Command = PLACE Position Direction
  | MOVE
  | LEFT
  | RIGHT
  | REPORT
  deriving (Show, Eq)
