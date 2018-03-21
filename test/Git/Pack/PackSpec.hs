{-# LANGUAGE BinaryLiterals #-}

module Git.Pack.PackSpec where

import Test.Hspec
import Test.QuickCheck (forAll, choose)

import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as BS
import Data.Word

import Git.Pack.Pack (chunkNumLeP', objHeadP)

spec :: Spec
spec = describe "Pack.Pack" $ do
  describe "Length parser" $ do
    it "should parse a single byte" $ forAll (choose (minBound, 127)) $
      \(w :: Word8) ->
        either error (== fromIntegral w) $
        parseOnly (chunkNumLeP' 0 0) $ BS.singleton w
    it "should parse multiple bytes until MSB unset" $
      let
        --                 4 --+     12 --+  24 -+            25 --+
        --                     v          v      v                 v
        bs = BS.pack [0b10000001, 0b10000010, 0b11000000, 0b00000001]
        parsed = either error id $ parseOnly (chunkNumLeP' 4 0b00000100) bs
        --                                                          ^
        --                                                      2 --+
      in
        parsed `shouldBe` sum (fmap (2^) [25, 24, 12, 4, 2 :: Int])
