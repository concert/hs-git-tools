{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BinaryLiterals #-}

module Git.InternalSpec where

import Test.Hspec

import Data.Bits (Bits)
import Data.Typeable (Typeable, typeOf, Proxy(..))
import Data.Word
import qualified System.Path as Path

import Git.Internal (lowMask, assembleBits, splitOn)

spec :: Spec
spec = describe "Internal" $ do
  describe "lowMask" $ do
    lowMaskSpec $ Proxy @Int
    lowMaskSpec $ Proxy @Word16
    it "should simply truncate a negative value" $ do
      lowMask (-4) 3 `shouldBe` (4 :: Int)
      lowMask (-3) 2 `shouldBe` (1 :: Int)

  describe "assembleBits" $ do
    it "should truncate single values" $
      assembleBits [(2, 7 :: Word8)] `shouldBe` 3
    it "should pack multiple values together" $ do
      assembleBits [(1, 1 :: Word8), (1, 0), (1, 1), (1, 0)] `shouldBe` 10
      assembleBits [(2, 7 :: Word8), (1, 0), (5, 0b101010)]
        `shouldBe` 0b11001010

  describe "splitOn" $ do
    it "should handle empty list" $
      splitOn 'l' "" `shouldBe` [""]
    it "should handle immediate match" $
      splitOn 'l' "lo" `shouldBe` ["o"]
    it "should handle trailing match" $
      -- Question: should we produce the trailing ""?
      splitOn 'l' "ol" `shouldBe` ["o", ""]
    it "should condense consecutive matches" $
      splitOn 'l' "hello world" `shouldBe` ["he", "o wor", "d"]

lowMaskSpec
  :: forall a. (Typeable a, Num a, Show a, Bits a, Bounded a) => Proxy a -> Spec
lowMaskSpec _ = let name = show $ typeOf $ minBound @a in do
  it ("should leave a small " ++ name ++ " untouched") $
    mapM_ (shouldBe (3 :: a) . lowMask 3) [2, 3, 4]
  it ("should trunctate a large " ++ name) $
    lowMask 15 3 `shouldBe` (7 :: a)
