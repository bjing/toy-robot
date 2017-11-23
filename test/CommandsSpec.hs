module CommandsSpec where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Test.Hspec

import Models
import Commands

-- A helper function to help us do assertions
assertEquals command initRobotState expected = do
  maybeRobot <- runMaybeT $ execStateT command initRobotState
  case maybeRobot of
    Just actual -> actual `shouldBe` expected
    Nothing -> InvalidRobot `shouldBe` expected

spec :: Spec
spec = do
  describe "runCommand" $ do
    let validRobotState = ValidRobot (Position 2 3) NORTH
    it "should move given a move command" $
      assertEquals (runCommand MOVE) validRobotState (ValidRobot (Position 2 4) NORTH)
    it "should turn left given a LEFT command" $
      assertEquals (runCommand LEFT) validRobotState (ValidRobot (Position 2 3) WEST)
    it "should turn left given a RIGHT command" $
      assertEquals (runCommand RIGHT) validRobotState (ValidRobot (Position 2 3) EAST)
    it "should PLACE robot given a PLACE command" $
      assertEquals (runCommand $ PLACE (Position 0 0) EAST) validRobotState (ValidRobot (Position 0 0) EAST)

  describe "Move command" $ do
    it "should move one step forward when able" $ do
      assertEquals move (ValidRobot (Position 2 3) NORTH) (ValidRobot (Position 2 4) NORTH)
      assertEquals move (ValidRobot (Position 2 3) SOUTH) (ValidRobot (Position 2 2) SOUTH)
      assertEquals move (ValidRobot (Position 2 3) EAST) (ValidRobot (Position 3 3) EAST)
      assertEquals move (ValidRobot (Position 2 3) WEST) (ValidRobot (Position 1 3) WEST)
    it "should not move robot off board" $ do
      let robot1 = ValidRobot (Position 2 4) NORTH
      assertEquals move robot1 robot1
      let robot2 = ValidRobot (Position 2 0) SOUTH
      assertEquals move robot2 robot2
      let robot3 = ValidRobot (Position 4 0) EAST
      assertEquals move robot3 robot3
      let robot4 = ValidRobot (Position 0 4) WEST
      assertEquals move robot4 robot4

  describe "Place command" $ do
    it "should place robot on board" $ do
      let initState = ValidRobot (Position 0 0) EAST
      assertEquals (place (Position 1 2) NORTH) initState (ValidRobot (Position 1 2) NORTH)
    it "should leave robot alone if attempting to put it off board" $ do
      let initState = ValidRobot (Position 0 0) EAST
      assertEquals (place (Position 1 (-1)) NORTH) initState initState
      assertEquals (place (Position (-1) 1) NORTH) initState initState
      assertEquals (place (Position 5 1) NORTH) initState initState
      assertEquals (place (Position 1 5) NORTH) initState initState

  describe "Left command" $
    it "should turn robot left" $ do
      let pos = Position 2 4
      assertEquals (turn LEFT) (ValidRobot pos NORTH) (ValidRobot pos WEST)
      assertEquals (turn LEFT) (ValidRobot pos SOUTH) (ValidRobot pos EAST)
      assertEquals (turn LEFT) (ValidRobot pos EAST) (ValidRobot pos NORTH)
      assertEquals (turn LEFT) (ValidRobot pos WEST) (ValidRobot pos SOUTH)

  describe "Right command" $
    it "should turn robot right" $ do
      let pos = Position 2 4
      assertEquals (turn RIGHT) (ValidRobot pos NORTH) (ValidRobot pos EAST)
      assertEquals (turn RIGHT) (ValidRobot pos SOUTH) (ValidRobot pos WEST)
      assertEquals (turn RIGHT) (ValidRobot pos EAST) (ValidRobot pos SOUTH)
      assertEquals (turn RIGHT) (ValidRobot pos WEST) (ValidRobot pos NORTH)
