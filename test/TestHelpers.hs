module TestHelpers where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import qualified Data.Text as T
import Test.Hspec

import Models

-- A helper function to help us do assertions
assertEquals command initRobotState expected = do
  maybeRobot <- runMaybeT $ execStateT command initRobotState
  case maybeRobot of
    Just actual -> actual `shouldBe` expected
    Nothing -> expected `shouldBe` InvalidRobot

createPositionInput :: String -> [T.Text]
createPositionInput i = T.splitOn (T.singleton ',') (T.pack i)
