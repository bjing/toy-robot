module Parsers where

import Models

parseCommand :: String -> Maybe Command
parseCommand "MOVE" = Just MOVE
parseCommand "LEFT" = Just LEFT
parseCommand "RIGHT" = Just RIGHT
parseCommand "REPORT" = Just REPORT
parseCommand "PLACE 3,3,EAST" = Just (PLACE (Position 3 3) EAST)
