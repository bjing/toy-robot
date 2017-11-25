{-# LANGUAGE OverloadedStrings #-}

module Parsers ( parseCommand ) where

import qualified Data.Text as T

import Models
import ParsersImpl

parseCommand :: T.Text -> Maybe Command
parseCommand = parseCommand' . T.toUpper . T.strip

parseCommand' :: T.Text -> Maybe Command
parseCommand' c
  | c == "LEFT" = Just LEFT
  | c == "RIGHT" = Just RIGHT
  | c == "MOVE" = Just MOVE
  | c == "REPORT" = Just REPORT
  | "PLACE " `T.isPrefixOf` c = parsePlaceCommand c
  | otherwise = Nothing
