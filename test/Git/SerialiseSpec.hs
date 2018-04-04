{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module Git.SerialiseSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances

import Data.Attoparsec.ByteString (parseOnly, string, endOfInput)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Data.Time
  ( ZonedTime(..), TimeOfDay(..), LocalTime(..), TimeZone(..)
  , Day(ModifiedJulianDay), utcToZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Git.Serialise (tellParsePos, decodeObject, encodeObject)
import Git.Types (Commit(..))
import Git.Types.Internal ()
import qualified Git.Types.Sha1 as Sha1
import Git.Types.SizedByteString (SizedByteString)
import qualified Git.Types.SizedByteString as SBS

import Git.Types.Sha1Spec ()

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
    it "should decode a real commit correctly" $
      let
        decoded = decodeObject (SBS.fromStrictByteString fa7a2abb_uncompBytes)
          :: Either String Commit
      in
        decoded `shouldBe` Right fa7a2abb_commit

    it "should re-encode a real commit correctly" $
      encodeObject fa7a2abb_commit
      `shouldBe`
      SBS.fromStrictByteString fa7a2abb_uncompBytes

    it "should roundtrip" $ property $ \commit ->
      let
        encoded = encodeObject @Commit commit
        roundtripped = decodeObject encoded :: Either String Commit
      in
        counterexample (show $ SBS.toLazyByteString encoded) $
        counterexample (show roundtripped) $
        roundtripped == Right commit


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

instance Arbitrary Commit where
  arbitrary = Commit <$> arbitrary <*> listOf1 arbitrary
      <*> name <*> email <*> time
      <*> name <*> email <*> time
      <*> arbitrary
    where
      -- FIXME: filtering out pointy brackets makes the tests go, but we really
      -- should decide what the rules are about encoding/rejecting names/email
      -- addresses containing pointy arrows:
      notPointy = Text.filter (\c -> c /= '<' && c /= '>')
      name = notPointy . Text.pack <$> listOf1 arbitrary
      email = notPointy . Text.pack . mconcat <$>
        sequence [listOf1 arbitrary, pure "@", listOf1 arbitrary]
      time = do
        tz <- TimeZone <$> arbitrary <*> pure False <*> pure ""
        utcToZonedTime tz . posixSecondsToUTCTime . fromRational . toRational
          <$> choose (0, 10000000 :: Int)

instance Arbitrary SizedByteString where
  arbitrary = SBS.fromStrictByteString <$> arbitrary
