
module Commands where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Lens
import Data.Validation
import qualified Data.Map as M

import Models
import qualified Helpers as H

type RobotState = StateT Robot (MaybeT IO) ()

runCommand :: Command -> RobotState
runCommand LEFT = turn LEFT
runCommand RIGHT = turn RIGHT
runCommand MOVE = move
runCommand REPORT = report
runCommand (PLACE pos dir) = place pos dir

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

updateRobotStateIfNecessary :: Maybe Robot -> RobotState
updateRobotStateIfNecessary (Just robot) = put robot
updateRobotStateIfNecessary Nothing = return ()

initialState :: Robot
initialState = InvalidRobot
