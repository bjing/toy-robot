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
} deriving (Eq, Ord)

instance Show Position where
  show position =
    show (x position) ++ "," ++ show (y position)

data Direction = NORTH
  | SOUTH
  | WEST
  | EAST
  deriving (Show, Eq, Ord)

data Robot =
  ValidRobot {
      position :: Position
    , facing :: Direction
  } | InvalidRobot
  deriving (Eq)

instance Show Robot where
    show (ValidRobot pos dir) = show pos ++ "," ++ show dir

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
mkDirection direction = _Success # direction

mkRobotFromDetails :: Coord -> Coord -> Direction -> Validated Robot
mkRobotFromDetails x y direction =
  ValidRobot
  <$> mkPositionFromCoords x y
  <*> mkDirection direction

mkRobot :: Position -> Direction -> Validated Robot
mkRobot position direction =
  ValidRobot
  <$> mkPosition position
  <*> mkDirection direction

initRobotState :: Robot
initRobotState = InvalidRobot
