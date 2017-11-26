module Commands
  ( runCommands
  , runCommand
  ) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad

import Models
import CommandsImpl
import Common

-- Type RobotState is defined in Common.hs

runCommands :: [Maybe Command] -> RobotState
runCommands cmds = do
  -- Drop commands from list until we see a PLACE command
  let filteredCmds = dropWhile isNotPlaceCmd cmds
  mapM_ runCommand filteredCmds

runCommand :: Maybe Command -> RobotState
runCommand (Just (PLACE position direction)) = place position direction
runCommand (Just command) = do
  robotState <- get
  guard (robotState /= InvalidRobot)
  case command of
    MOVE -> move
    LEFT -> turn LEFT
    RIGHT -> turn RIGHT
    REPORT -> report
runCommand Nothing = stayIdle
