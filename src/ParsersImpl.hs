{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

module ParsersImpl where

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AT
import Data.Either.Combinators
import Control.Monad
import Control.Lens
import Data.Validation

import Models
import Common

parsePlaceCommand :: T.Text -> Maybe Command
parsePlaceCommand cmd = do
  let details = T.intercalate " " $ tail $ T.splitOn (T.singleton ' ') cmd
  let itemList = T.strip <$> T.splitOn (T.singleton ',') details

  position <- parsePosition $ take 2 itemList
  directionStr <- headMaybe $ reverse itemList
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
