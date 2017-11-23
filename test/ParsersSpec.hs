module ParsersSpec where

import Test.Hspec

import Parsers
import Models

spec :: Spec
spec =
  describe "Command parser" $
    it "should return LEFT" $ do
      parseCommand "LEFT" `shouldBe` Just LEFT
      parseCommand "REPORT" `shouldBe` Just REPORT
