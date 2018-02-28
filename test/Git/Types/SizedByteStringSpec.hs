module Git.Types.SizedByteStringSpec where

import Test.Hspec

import Data.Knob (newKnob, withFileHandle)
import System.IO (IOMode(ReadMode), Handle)

import qualified Git.Types.SizedByteString as SBS

spec :: Spec
spec = do
  describe "SizedByteString" $
    describeWithVirtFile "fromFilePath" $ do
      it "should report the correct ByteString length" $ \h -> do
        sbs <- SBS.fromHandle h
        SBS.length sbs `shouldBe` 11

      it "should provide the correct content" $ \h -> do
        sbs <- SBS.fromHandle h
        SBS.toLazyByteString sbs `shouldBe` "hello world"

describeWithVirtFile :: String -> SpecWith Handle -> Spec
describeWithVirtFile description = around (\spec' -> do
  k <- newKnob "hello world"
  withFileHandle k description ReadMode $ spec'
  ) . describe description
