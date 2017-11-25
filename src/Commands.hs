
module Commands where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad
import Control.Lens
import Data.Validation
import qualified Data.Map as M

import Models
import qualified Helpers as H

type RobotState = StateT Robot (MaybeT IO) ()

runCommands :: [Maybe Command] -> RobotState
runCommands cmds = do
  let filteredCmds = dropWhile isNotPlaceCmd cmds
  mapM_ runCommand filteredCmds

runCommand :: Maybe Command -> RobotState
runCommand (Just (PLACE pos dir)) = place pos dir
runCommand (Just command) = do
  robotState <- get
  guard (robotState /= InvalidRobot)
  case command of
    MOVE -> move
    LEFT -> turn LEFT
    RIGHT -> turn RIGHT
    REPORT -> report
runCommand Nothing = idle

move :: RobotState
move = do
  robot <- get
  let currentFacing = facing robot
  let currentX = x $ position robot
  let currentY = y $ position robot
  updateRobotStateIfNecessary $ move' currentX currentY currentFacing

move' :: Coord -> Coord -> Direction -> Maybe Robot
move' x y NORTH = mkRobotFromDetails x (y+1) NORTH ^? _Success
move' x y SOUTH = mkRobotFromDetails x (y-1) SOUTH ^? _Success
move' x y EAST  = mkRobotFromDetails (x+1) y EAST ^? _Success
move' x y WEST  = mkRobotFromDetails (x-1) y WEST ^? _Success

place :: Position -> Direction -> RobotState
place pos dir =
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
  robot <- get
  liftIO $ print robot

idle :: RobotState
idle = return ()

updateRobotStateIfNecessary :: Maybe Robot -> RobotState
updateRobotStateIfNecessary (Just robot) = put robot
updateRobotStateIfNecessary Nothing = return ()

isNotPlaceCmd :: Maybe Command -> Bool
isNotPlaceCmd (Just (PLACE _ _)) = False
isNotPlaceCmd _ = True
