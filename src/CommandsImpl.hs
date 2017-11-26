module CommandsImpl where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Map as M
import Control.Lens
import Data.Validation

import Models
import Common

-- Type RobotState is defined in Common.hs

moveRobot :: RobotState
moveRobot = do
  robot <- get
  let currentX = x $ position robot
  let currentY = y $ position robot
  let currentFacing = facing robot
  updateRobotStateIfNecessary $ moveRobot' currentX currentY currentFacing

moveRobot' :: Coord -> Coord -> Direction -> Maybe Robot
moveRobot' x y NORTH = mkRobotFromDetails x (y+1) NORTH ^? _Success
moveRobot' x y SOUTH = mkRobotFromDetails x (y-1) SOUTH ^? _Success
moveRobot' x y EAST  = mkRobotFromDetails (x+1) y EAST  ^? _Success
moveRobot' x y WEST  = mkRobotFromDetails (x-1) y WEST  ^? _Success


placeRobot :: Position -> Direction -> RobotState
placeRobot pos dir = do
  robot <- get
  updateRobotStateIfNecessary $ mkRobot pos dir ^? _Success

turnLeftLookup = M.fromList[(NORTH, WEST), (WEST, SOUTH), (SOUTH, EAST), (EAST, NORTH)]
turnRightLookup = M.fromList[(NORTH, EAST), (WEST, NORTH), (SOUTH, WEST), (EAST, SOUTH)]

turn :: Command -> RobotState
turn command = do
  robot <- get
  let currentFacing = facing robot
  let pos = position robot
  let turnMap = case command of
                       LEFT -> turnLeftLookup
                       RIGHT -> turnRightLookup
  newDirection <- liftMaybe $ M.lookup currentFacing turnMap
  updateRobotStateIfNecessary $ mkRobot pos newDirection ^? _Success

-- The report function directly gets the robot state and prints it.
-- I don't like having I/O in this layer of the app, but I can't come up with a better solution
report :: RobotState
report = do
  robotState <- get
  liftIO $ print robotState

-- This function is called when receiving an invalid command
stayIdle :: RobotState
stayIdle = return ()

updateRobotStateIfNecessary :: Maybe Robot -> RobotState
updateRobotStateIfNecessary (Just robot) = put robot
updateRobotStateIfNecessary Nothing = return ()

isNotPlaceCmd :: Maybe Command -> Bool
isNotPlaceCmd (Just (PLACE _ _)) = False
isNotPlaceCmd _ = True
