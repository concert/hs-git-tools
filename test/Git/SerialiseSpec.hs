{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module Git.SerialiseSpec where

import Test.Hspec

import Data.Attoparsec.ByteString (parseOnly, string, endOfInput)
import qualified Data.ByteString as BS
import Data.Time
  ( ZonedTime(..), TimeOfDay(..), LocalTime(..), TimeZone(..)
  , Day(ModifiedJulianDay))

import Git.Serialise (tellParsePos, decodeObject, encodeObject)
import Git.Types (Commit(..))
import Git.Types.Internal ()
import qualified Git.Types.Sha1 as Sha1
import qualified Git.Types.SizedByteString as SBS

deriving instance Eq ZonedTime
deriving instance Eq Commit

spec :: Spec
spec = describe "Serialise" $ do
  describe "tellParsePos" $ do
    it "should not consume input" $
      parseOnly ((tellParsePos >> string "hello") <* endOfInput) "hello"
      `shouldBe` Right "hello"
    it "should return the correct parse position" $
      parseOnly (string "he" >> tellParsePos) "hello"
      `shouldBe` Right 2

  describe "commit encoding" $ do
    it "should decode correctly" $
      let
        decoded = decodeObject (SBS.fromStrictByteString fa7a2abb_uncompBytes)
          :: Either String Commit
      in
        decoded `shouldBe` Right fa7a2abb_commit

    it "should encode correctly" $
      encodeObject fa7a2abb_commit
      `shouldBe`
      SBS.fromStrictByteString fa7a2abb_uncompBytes


-- | This is raw decompressed commit data from this very git repo (trying to
--   organise it as a data file for the test via cabal was hard).
fa7a2abb_uncompBytes :: BS.ByteString
fa7a2abb_uncompBytes = "tree 56558e3275b57381cd04d6cb604dde2f7e773166\n\
    \parent 3b2f2c262fe95a693b3cda2c18c2c4b16d29dc5e\n\
    \author Paul Weaver <paul@concertdaw.co.uk> 1522328367 +0100\n\
    \committer Paul Weaver <paul@concertdaw.co.uk> 1522328367 +0100\n\
    \\n\
    \Fix module name in test\n"

fa7a2abb_commit :: Commit
fa7a2abb_commit = Commit
    (sha1 "56558e3275b57381cd04d6cb604dde2f7e773166")
    [sha1 "3b2f2c262fe95a693b3cda2c18c2c4b16d29dc5e"]
    "Paul Weaver" "paul@concertdaw.co.uk" (t 58206 13 59 27)
    "Paul Weaver" "paul@concertdaw.co.uk" (t 58206 13 59 27)
    "Fix module name in test\n"
  where
    sha1 = either error id . Sha1.fromHexString
    t d h m s = ZonedTime
      (LocalTime (ModifiedJulianDay d) (TimeOfDay h m s))
      (TimeZone 60 False "")
