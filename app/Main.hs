{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad
import System.Environment
import qualified Data.Text as T

import Lib
import Commands
import Parsers
import Models
import Simulation

main :: IO ()
main = do
  args <- getArgs
  case args of
    (inputFilePath:_) -> runStandard inputFilePath
    [] -> runRepl

runStandard :: String -> IO ()
runStandard inputFilePath = do
  _ <- runMaybeT $ runStateT (simulateFromFile getCmdFromFile inputFilePath) initRobotState
  return ()

runRepl :: IO ()
runRepl = forever $ do
  putStrLn "Please start typing commands"
  runMaybeT $ runStateT (simulateFromStdin getCmdFromStdin) initRobotState

getCmdFromStdin :: IO String
getCmdFromStdin = getLine

getCmdFromFile :: FilePath -> IO [String]
getCmdFromFile filePath = do
  content <- readFile filePath
  let cmdStrs = lines content
  return cmdStrs
