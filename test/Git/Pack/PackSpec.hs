{-# LANGUAGE BinaryLiterals #-}

module Git.Pack.PackSpec where

import Test.Hspec
import Test.QuickCheck (forAll, choose)

import Data.Attoparsec.ByteString (parseOnly)
import Data.Bits (shiftL)
import qualified Data.ByteString as BS
import Data.Word

import Git.Pack.Pack (chunkNumLeP', chunkNumBeP)

spec :: Spec
spec = describe "Pack.Pack" $ do
  describe "Little-endian chunked number parser" $ do
    it "should parse a single byte" $ forAll (choose (0 :: Word8, 127)) $
      \w ->
        either error (== fromIntegral w) $
        parseOnly (chunkNumLeP' 0 0) $ BS.singleton w
    it "should parse multiple bytes until MSB unset" $
      let
        --                     4         12      24 21             25 +- Ignore
        --                     v          v      v  v              v  v
        bs = BS.pack [0b10000001, 0b10000010, 0b11001000, 0b00000001, 255]
        parsed = either error id $ parseOnly (chunkNumLeP' 4 0b00000100) bs
        --                                                          ^
        --                                                      2 --+
      in
        parsed `shouldBe` sum (fmap (2^) [25, 24, 21, 12, 4, 2 :: Int])

  describe "Big-endian chunked number parser (with addition)" $ do
    it "should parse a single byte as-is" $ forAll (choose (0 :: Word8, 127)) $
      \w ->
        either error (== fromIntegral w) $
        parseOnly (chunkNumBeP) $ BS.singleton w
    it "should parse multiple bytes until MSB unset" $
      let
        --               14    8     7     0    Ignore
        --               v     v     v     v    |------->
        bs = BS.pack [0b10010010, 0b01110101, 0b011111000]
        parsed = either error id $ parseOnly chunkNumBeP bs
      in
        --    This +1 is slightly strange ---+
        --                                   v
        parsed `shouldBe` (shiftL (0b10010 + 1) 7) + 0b1110101
