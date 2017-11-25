module Main where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Text as T

import Lib
import Parsers
import Commands
import Models
import qualified Helpers as H


main :: IO ()
main = forever $ do
  putStrLn "Please start typing commands"
  runMaybeT $ runStateT simulate initialState

simulate :: RobotState
simulate = forever $ do
  cmdStr <- liftIO getLine
  cmd <- H.liftMaybe $ parseCommand (T.pack cmdStr)
  runCommand cmd
