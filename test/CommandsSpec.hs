module CommandsSpec where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Test.Hspec

import Models
import Commands

-- A helper function to help us do assertions
assertEquals command initRobotState expected = do
  maybeRobot <- runMaybeT $ execStateT command initRobotState
  case maybeRobot of
    Just actual -> actual `shouldBe` expected
    Nothing -> expected `shouldBe` initialState

spec :: Spec
spec = do
  describe "runCommand" $
    it "pending"
      pending

  describe "turnLeft" $
    it "should turn robot left" $ do
      let initState = Robot (Position 0 0) NORTH
      assertEquals turnLeft initState (Robot (Position 0 0) WEST)
      let initState = Robot (Position 0 0) WEST
      assertEquals turnLeft initState (Robot (Position 0 0) SOUTH)
      let initState = Robot (Position 0 0) SOUTH
      assertEquals turnLeft initState (Robot (Position 0 0) EAST)
      let initState = Robot (Position 0 0) EAST
      assertEquals turnLeft initState (Robot (Position 0 0) NORTH)

  describe "turnRight" $
      it "should turn robot right" $ do
        let initState = Robot (Position 0 0) NORTH
        assertEquals turnRight initState (Robot (Position 0 0) EAST)
        let initState = Robot (Position 0 0) EAST
        assertEquals turnRight initState (Robot (Position 0 0) SOUTH)
        let initState = Robot (Position 0 0) SOUTH
        assertEquals turnRight initState (Robot (Position 0 0) WEST)
        let initState = Robot (Position 0 0) WEST
        assertEquals turnRight initState (Robot (Position 0 0) NORTH)

  describe "move" $
    it "should move robot one step forward" $ do
      let initState = Robot (Position 0 0) NORTH
      assertEquals move initState (Robot (Position 0 1) NORTH)
      let initState = Robot (Position 0 0) EAST
      assertEquals move initState (Robot (Position 1 0) EAST)

  describe "Initial state" $
    it "should be initial state" $
      initialState `shouldBe` Robot (Position 0 0) NORTH
