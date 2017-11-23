module Parsers where

import Models

parseCommand :: String -> Maybe Command
parseCommand "LEFT" = Just LEFT
parseCommand "REPORT" = Just REPORT
