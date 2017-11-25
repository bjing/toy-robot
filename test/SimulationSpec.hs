module SimulationSpec where

import Test.Hspec

import Simulation
import TestHelpers
import Models

testCommandsA = ["PLACE 0,0,NORTH", "MOVE"]
expectedResultA = ValidRobot (Position 0 1) NORTH
testCommandsB = ["PLACE 0,0,NORTH", "LEFT"]
expectedResultB = ValidRobot (Position 0 0) WEST
testCommandsC = ["PLACE 1,2,EAST", "MOVE", "MOVE", "LEFT", "MOVE"]
expectedResultC = ValidRobot (Position 3 3) NORTH
testCommandsThatDontStartWithPlace = ["MOVE", "DIG", "PLACE 1,2,SOUTH", "MOVE"]
expectedResultNonPlaceStart = ValidRobot (Position 1 1) SOUTH
testCommandsEmpty = []
expectedResultEmpty = InvalidRobot

spec :: Spec
spec = do
  let initialRobotState = InvalidRobot

  describe "Simulate from file" $ do
    it "should read file content and simulate" $ do
      let testFileReaderA _ = return testCommandsA
      assertEquals (simulateFromFile testFileReaderA "file") initialRobotState expectedResultA
      let testFileReaderB _ = return testCommandsB
      assertEquals (simulateFromFile testFileReaderB "file") initialRobotState expectedResultB
      let testFileReaderC _ = return testCommandsC
      assertEquals (simulateFromFile testFileReaderC "file") initialRobotState expectedResultC

    it "should ignore commands from start of the file until it sees PLACE" $ do
      let testFileReaderManyMore _ = return testCommandsThatDontStartWithPlace
      assertEquals (simulateFromFile testFileReaderManyMore "file") initialRobotState expectedResultNonPlaceStart

    it "should slack off if file is empty" $ do
      let testFileReaderEmpty _ = return testCommandsEmpty
      assertEquals (simulateFromFile testFileReaderEmpty "file") initialRobotState expectedResultEmpty