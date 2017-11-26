module Simulation where

import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Text as T

import Commands
import Parsers
import IOReaders
import Common

-- Type RobotState is defined in Common.hs

simulateFromStdin :: StdinReader -> RobotState
simulateFromStdin stdinReader = forever $ do
  cmdStr <- liftIO stdinReader
  let cmd = parseCommand (T.pack cmdStr)
  runCommand cmd

simulateFromFile :: FileReader -> FilePath -> RobotState
simulateFromFile fileReader filePath = do
  cmdStrs <- liftIO $ fileReader filePath
  let cmds = fmap (parseCommand . T.pack) cmdStrs
  runCommands cmds
