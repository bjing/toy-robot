module TestHelpers where

import qualified Data.Text as T

createPositionInput :: String -> [T.Text]
createPositionInput i = T.splitOn (T.singleton ',') (T.pack i)
