module Simulation where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Text as T

import Commands
import Parsers
import Models
import Lib

type FileReader = FilePath -> IO [String]
type StdinReader = IO String

simulateFromStdin :: StdinReader -> StateT Robot (MaybeT IO) ()
simulateFromStdin stdinReader = forever $ do
  cmdStr <- liftIO stdinReader
  let cmd = parseCommand (T.pack cmdStr)
  runCommand cmd

simulateFromFile :: FileReader -> FilePath -> StateT Robot (MaybeT IO) ()
simulateFromFile reader filePath = do
  cmdStrs <- liftIO $ reader filePath
  let cmds = fmap (parseCommand . T.pack) cmdStrs
  runCommands cmds
