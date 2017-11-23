module Models where

import Control.Lens
import Data.Validation

type Coord = Integer
type Validated = AccValidation [Error]

data Error = CoordOutOfRangeError
  | OtherError
  deriving (Show)

data Position = Position {
    x :: Coord
  , y :: Coord
} deriving (Show, Eq, Ord)

data Direction = NORTH
  | SOUTH
  | WEST
  | EAST
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

-- Smart constructors

minCoord = 0
maxCoord = 5

mkCoord :: Integer -> Validated Coord
mkCoord z = if z >= minCoord && z < maxCoord
            then _Success # z
            else _Failure # [ CoordOutOfRangeError ]

mkPositionFromCoords :: Integer -> Integer -> Validated Position
mkPositionFromCoords x y =
  Position
  <$> mkCoord x
  <*> mkCoord y

mkPosition :: Position -> Validated Position
mkPosition position =
  Position
  <$> mkCoord (x position)
  <*> mkCoord (y position)

mkDirection :: Direction -> Validated Direction
mkDirection direction = if True
                        then _Success # direction
                        else _Failure # [ OtherError ]

mkRobotFromDetails :: Coord -> Coord -> Direction -> Validated Robot
mkRobotFromDetails x y direction =
  Robot
  <$> mkPositionFromCoords x y
  <*> mkDirection direction

mkRobot :: Position -> Direction -> Validated Robot
mkRobot position direction =
  Robot
  <$> mkPosition position
  <*> mkDirection direction
