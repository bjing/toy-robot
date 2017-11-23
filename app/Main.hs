module Main where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad

import Lib
import Parsers
import Commands
import Models


main :: IO ()
main = forever $ do
  putStrLn "Please start typing commands"
  cmdStr <- getLine
  cmd <- liftMaybe $ parseCommand cmdStr
  runMaybeT $ runStateT (runCommand cmd) initialState

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return
