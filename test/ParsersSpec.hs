{-# LANGUAGE OverloadedStrings #-}

module ParsersSpec (spec) where

import Parsers
import ParsersImpl
import Models
import TestHelpers
import Test.Hspec

spec :: Spec
spec = do
  describe "All command parsers" $ do
    it "should ignore character cases" $ do
      parseCommand "place 4,3,noRth" `shouldBe` (Just $ PLACE (Position 4 3) NORTH)
      parseCommand "mOve" `shouldBe` Just MOVE
      parseCommand "lefT" `shouldBe` Just LEFT
      parseCommand "Right" `shouldBe` Just RIGHT
      parseCommand "RePoRt" `shouldBe` Just REPORT
    it "should not accept invalid commands" $ do
      parseCommand "ATTACK 1,2,EAST" `shouldBe` Nothing
      parseCommand "FLY 1,2,EAST" `shouldBe` Nothing
      parseCommand "PLACEON 1,2,EAST" `shouldBe` Nothing

  describe "Place command parser" $ do
    it "should accept correct commands" $ do
      parsePlaceCommand "PLACE 4,3,NORTH" `shouldBe` (Just $ PLACE (Position 4 3) NORTH)
      parsePlaceCommand "PLACE 0,0,EAST" `shouldBe` (Just $ PLACE (Position 0 0) EAST)
    it "should ignore whitespaces around commas" $
      parsePlaceCommand "PLACE 4 , 3 , NORTH" `shouldBe` (Just $ PLACE (Position 4 3) NORTH)

  describe "Direction command parser" $ do
    it "should parse NORTH" $
      parseDirection "NORTH" `shouldBe` Just NORTH
    it "should parse SOUTH" $
      parseDirection "SOUTH" `shouldBe` Just SOUTH
    it "should parse WEST" $
      parseDirection "WEST" `shouldBe` Just WEST
    it "should parse EAST" $
      parseDirection "EAST" `shouldBe` Just EAST
    it "should not accept invalid direction strings" $ do
      parseDirection "UP" `shouldBe` Nothing
      parseDirection "" `shouldBe` Nothing

  describe "Position command parser" $ do
    it "should parse valid positions" $ do
      parsePosition (createPositionInput "3,4") `shouldBe` Just (Position 3 4)
      parsePosition (createPositionInput "0,0") `shouldBe` Just (Position 0 0)
      parsePosition (createPositionInput "4,4") `shouldBe` Just (Position 4 4)
    it "should not accept invalid coordinates" $ do
      parsePosition (createPositionInput "-1,3") `shouldBe` Nothing
      parsePosition (createPositionInput "1,-3") `shouldBe` Nothing
      parsePosition (createPositionInput "-1,-3") `shouldBe` Nothing
      parsePosition (createPositionInput "1,5") `shouldBe` Nothing
      parsePosition (createPositionInput "") `shouldBe` Nothing

  describe "Other command parsers" $ do
    it "should parse MOVE command" $
      parseCommand "MOVE" `shouldBe` Just MOVE
    it "should parse LEFT command" $
      parseCommand "LEFT" `shouldBe` Just LEFT
    it "should parse RIGHT command" $
      parseCommand "RIGHT" `shouldBe` Just RIGHT
    it "should parse REPORT command" $
      parseCommand "REPORT" `shouldBe` Just REPORT
