module ParsersSpec where

import Test.Hspec

import Parsers
import Models

spec :: Spec
spec =
  describe "Command parser" $
    it "should parse all commands" $ do
      parseCommand "MOVE" `shouldBe` Just MOVE
      parseCommand "LEFT" `shouldBe` Just LEFT
      parseCommand "RIGHT" `shouldBe` Just RIGHT
      parseCommand "REPORT" `shouldBe` Just REPORT
      parseCommand "PLACE" `shouldBe` Just (PLACE (Position 3 3) EAST)
