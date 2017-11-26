{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad
import System.Environment
import qualified Data.Text as T

import Models
import Simulation
import IOReaders

main :: IO ()
main = do
  args <- getArgs
  case args of
    (inputFilePath:_) -> runFromFile inputFilePath
    [] -> runRepl

runFromFile :: String -> IO ()
runFromFile inputFilePath = do
  _ <- runMaybeT $ runStateT (simulateFromFile getCmdsFromFile inputFilePath) initRobotState
  return ()

runRepl :: IO ()
runRepl = forever $ do
  putStrLn "Please start typing commands"
  _ <- runMaybeT $ runStateT (simulateFromStdin getCmdsFromStdin) initRobotState
  return ()
