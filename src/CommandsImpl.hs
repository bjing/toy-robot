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
import qualified Helpers as H

-- A StateT stack is used so that we can easily update and get robot state without
-- explicitly passing it around
type RobotState = StateT Robot (MaybeT IO) ()

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


turnLeftMap = M.fromList[(NORTH, WEST), (WEST, SOUTH), (SOUTH, EAST), (EAST, NORTH)]
turnRightMap = M.fromList[(NORTH, EAST), (WEST, NORTH), (SOUTH, WEST), (EAST, SOUTH)]

turn :: Command -> RobotState
turn command = do
  robot <- get
  let currentFacing = facing robot
  let pos = position robot
  let turnMap = case command of
                       LEFT -> turnLeftMap
                       RIGHT -> turnRightMap
  newDirection <- H.liftMaybe $ M.lookup currentFacing turnMap
  updateRobotStateIfNecessary $ mkRobot pos newDirection ^? _Success

report :: RobotState
report = do
  robotState <- get
  liftIO $ print robotState

stayIdle :: RobotState
stayIdle = return ()

updateRobotStateIfNecessary :: Maybe Robot -> RobotState
updateRobotStateIfNecessary (Just robot) = put robot
updateRobotStateIfNecessary Nothing = return ()

isNotPlaceCmd :: Maybe Command -> Bool
isNotPlaceCmd (Just (PLACE _ _)) = False
isNotPlaceCmd _ = True
