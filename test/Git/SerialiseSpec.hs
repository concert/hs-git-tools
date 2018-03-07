module Git.SerialiseSpec where

import Test.Hspec

import Data.Attoparsec.ByteString (parseOnly, string, endOfInput)

import Git.Serialise (tellParsePos)

spec :: Spec
spec = describe "Store" $ do
  describe "tellParsePos" $ do
    it "should not consume input" $
      parseOnly ((tellParsePos >> string "hello") <* endOfInput) "hello"
      `shouldBe` Right "hello"
    it "should return the correct parse position" $
      parseOnly (string "he" >> tellParsePos) "hello"
      `shouldBe` Right 2
