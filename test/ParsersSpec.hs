{-# LANGUAGE OverloadedStrings #-}

module ParsersSpec where

import Test.Hspec

import Parsers
import Models
import qualified TestHelpers as TH

spec :: Spec
spec = do
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
      parsePosition (TH.createPositionInput "3,4") `shouldBe` Just (Position 3 4)
      parsePosition (TH.createPositionInput "0,0") `shouldBe` Just (Position 0 0)
      parsePosition (TH.createPositionInput "4,4") `shouldBe` Just (Position 4 4)
    it "should not accept invalid coordinates" $ do
      parsePosition (TH.createPositionInput "-1,3") `shouldBe` Nothing
      parsePosition (TH.createPositionInput "1,-3") `shouldBe` Nothing
      parsePosition (TH.createPositionInput "-1,-3") `shouldBe` Nothing
      parsePosition (TH.createPositionInput "1,5") `shouldBe` Nothing
      parsePosition (TH.createPositionInput "") `shouldBe` Nothing

  describe "Other command parsers" $ do
    it "should parse MOVE command" $
      parseCommand "MOVE" `shouldBe` Just MOVE
    it "should parse LEFT command" $
      parseCommand "LEFT" `shouldBe` Just LEFT
    it "should parse RIGHT command" $
      parseCommand "RIGHT" `shouldBe` Just RIGHT
    it "should parse REPORT command" $
      parseCommand "REPORT" `shouldBe` Just REPORT