{-# LANGUAGE OverloadedStrings #-}
module Parsers where

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AT
import Data.Either.Combinators
import Control.Monad
import Control.Lens
import Data.Validation

import qualified Helpers as H
import Models

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

parsePlaceCommand :: T.Text -> Maybe Command
parsePlaceCommand cmd = do
  let details = T.intercalate " " $ tail $ T.splitOn (T.singleton ' ') cmd
  let itemList = T.strip <$> T.splitOn (T.singleton ',') details

  position <- parsePosition $ take 2 itemList
  directionStr <- H.headMaybe $ reverse itemList
  direction <- parseDirection directionStr
  return $ PLACE position direction

parseDirection :: T.Text -> Maybe Direction
parseDirection "NORTH" = Just NORTH
parseDirection "SOUTH" = Just SOUTH
parseDirection "EAST" = Just EAST
parseDirection "WEST" = Just WEST
parseDirection _ = Nothing

parsePosition :: [T.Text] -> Maybe Position
parsePosition (x:y:[]) = parsePosition' x y
parsePosition _ = Nothing

parsePosition' :: T.Text -> T.Text -> Maybe Position
parsePosition' x y = do
  xCoord <- parseInt x
  yCoord <- parseInt y
  mkPositionFromCoords xCoord yCoord ^? _Success
  where
    parseInt :: T.Text -> Maybe Integer
    parseInt textDigit = rightToMaybe $ AT.parseOnly (AT.signed AT.decimal) textDigit
