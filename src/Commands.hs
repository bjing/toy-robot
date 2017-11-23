
module Commands where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Models

type RobotState = StateT Robot (MaybeT IO) ()

runCommand :: Command -> RobotState
runCommand LEFT = turnLeft
runCommand RIGHT = turnRight
runCommand MOVE = move
runCommand REPORT = do
  robot <- get
  liftIO $ print robot

move :: RobotState
move = do
  robot <- get
  let currentFacing = facing robot
  let currentX = x $ position robot
  let currentY = y $ position robot
  put $ move' currentX currentY currentFacing

move' :: Coord -> Coord -> Direction -> Robot
move' x y NORTH = Robot (Position x (y+1)) NORTH
move' x y SOUTH = Robot (Position x (y-1)) SOUTH
move' x y EAST  = Robot (Position (x+1) y) EAST
move' x y WEST  = Robot (Position (x-1) y) WEST

turnLeft :: RobotState
turnLeft = do
  robot <- get
  let currentFacing = facing robot
  let currentPostion = position robot
  let newFacing = case currentFacing of
                    NORTH -> WEST
                    WEST -> SOUTH
                    SOUTH -> EAST
                    EAST -> NORTH
  put $ Robot currentPostion newFacing

turnRight :: RobotState
turnRight = do
  robot <- get
  let currentFacing = facing robot
  let currentPostion = position robot
  let newFacing = case currentFacing of
                    NORTH -> EAST
                    EAST -> SOUTH
                    SOUTH -> WEST
                    WEST -> NORTH
  put $ Robot currentPostion newFacing

initialState :: Robot
initialState = Robot (Position 0 0) NORTH
