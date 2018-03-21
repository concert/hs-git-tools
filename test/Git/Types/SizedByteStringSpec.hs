module Git.Types.SizedByteStringSpec where

import Test.Hspec

import Data.Knob (newKnob, withFileHandle)
import System.IO (IOMode(ReadMode), Handle)

import qualified Git.Types.SizedByteString as SBS

spec :: Spec
spec = do
  describe "SizedByteString" $ do
    describeWithVirtFile "fromFilePath" $ do
      it "should report the correct ByteString length" $ \h -> do
        sbs <- SBS.fromHandle h
        SBS.length sbs `shouldBe` 11

      it "should provide the correct content" $ \h -> do
        sbs <- SBS.fromHandle h
        SBS.toLazyByteString sbs `shouldBe` "hello world"

    describe "take" $ let sbs = SBS.fromStrictByteString "hello" in do
      it "should report the correct length when i < len" $
        SBS.take 3 sbs `shouldBe` SBS.fromStrictByteString "hel"
      it "should report the correct length when i = len" $
        SBS.take (fromIntegral $ SBS.length sbs) sbs `shouldBe` sbs
      it "should report the correct length when i > len" $
        SBS.take (fromIntegral $ SBS.length sbs + 1) sbs `shouldBe` sbs

    describe "drop" $ let sbs = SBS.fromStrictByteString "world" in do
      it "should report the correct length when i < len" $
        SBS.drop 3 sbs `shouldBe` SBS.fromStrictByteString "ld"
      it "should report the correct length when i = len" $
        SBS.drop (fromIntegral $ SBS.length sbs) sbs `shouldBe` mempty
      it "should report the correct length when i > len" $
        SBS.drop (fromIntegral $ SBS.length sbs + 1) sbs `shouldBe` mempty

describeWithVirtFile :: String -> SpecWith Handle -> Spec
describeWithVirtFile description = around (\spec' -> do
  k <- newKnob "hello world"
  withFileHandle k description ReadMode $ spec'
  ) . describe description
