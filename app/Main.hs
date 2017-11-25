module Main where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad
import System.Environment
import qualified Data.Text as T

import Lib
import Parsers
import Commands
import Models
import qualified Helpers as H

main :: IO ()
main = do
  args <- getArgs
  case args of
    (inputFilePath:_) -> do
      _ <- runMaybeT $ runStateT (simulateFromFile inputFilePath) initRobotState
      return ()
    [] -> do
      _ <- runMaybeT $ runStateT simulateFromStdin initRobotState
      return ()

simulateFromStdin :: RobotState
simulateFromStdin = forever $ do
  liftIO $ putStrLn "Please start typing commands"
  cmdStr <- liftIO getLine
  let cmd = parseCommand (T.pack cmdStr)
  runCommand cmd

simulateFromFile :: FilePath -> RobotState
simulateFromFile filePath = do
  content <- liftIO $ readFile filePath
  let cmdStrs = lines content
  let cmds = fmap (parseCommand . T.pack) cmdStrs
  mapM_ runCommand cmds
