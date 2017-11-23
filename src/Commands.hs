
module Commands where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Models

type RobotState = StateT Robot (MaybeT IO) ()

runCommand :: Command -> RobotState
runCommand LEFT = turnLeft
runCommand REPORT = do
  robot <- get
  liftIO $ print robot

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

initialState :: Robot
initialState = Robot (Position 0 0) NORTH
