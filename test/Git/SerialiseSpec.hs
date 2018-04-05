{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module Git.SerialiseSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Attoparsec.ByteString (parseOnly, string, endOfInput)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy
import qualified Data.Text as Text
import Data.Tagged (Tagged(..))
import Data.Time
  ( ZonedTime(..), TimeOfDay(..), LocalTime(..), TimeZone(..)
  , Day(ModifiedJulianDay), utcToZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Git.Serialise
  ( tellParsePos
  , decodeObject, encodeObject
  , decodeLooseObject, encodeLooseObject
  , GitObject(objectType, unwrap), encodeObjectType)
import Git.Types (Commit(..), Blob(..), Tree(..), TreeRow(..), FileMode(..))
import Git.Types.Internal ()
import Git.Types.Sha1 (Sha1)
import qualified Git.Types.Sha1 as Sha1

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

    checkEncoding commit_fa7a2abb
    checkEncoding tree_56558e32
    checkEncoding blob_527d8d43
  where
    checkEncoding
      :: forall a. (GitObject a, Show a, Eq a, Arbitrary a)
      => TestObject a -> Spec
    checkEncoding (TestObject objSha1 looseHeader bytes obj) =
      let tyName = encodeObjectType $ objectType $ Proxy @a in
      describe (tyName ++ " encoding") $ do
        it ("should decode a real " ++ tyName) $
          (decodeObject bytes :: Either String a) `shouldBe` Right obj

        it ("should encode a real " ++ tyName) $
          encodeObject obj `shouldBe` bytes

        it ("should decode a real loose " ++ tyName) $
          let
            decoded = decodeLooseObject (LBS.fromStrict $ looseHeader <> bytes)
              >>= unwrap :: Either String a
          in
            decoded `shouldBe` Right obj

        it ("should encode a real loose " ++ tyName) $
          let (Tagged sha, encoded) = encodeLooseObject obj in do
            encoded `shouldBe` (looseHeader <> bytes)
            sha `shouldBe` objSha1

        it ("arbitrary " ++ tyName ++ "s should roundtrip") $ property $ \o ->
          let
            encoded = encodeObject @a o
            roundtripped = decodeObject encoded :: Either String a
          in
            counterexample (show encoded) $
            counterexample (show roundtripped) $
            roundtripped == Right o


sha1 :: String -> Sha1
sha1 = either error id . Sha1.fromHexString

data TestObject objTy
  = TestObject
  { toSha1 :: Sha1, toLoseHeader :: BS.ByteString, toBody :: BS.ByteString
  , toObject :: objTy}

-- | This is raw decompressed commit data from this very git repo (trying to
--   organise it as a data file for the test via cabal was hard).
commit_fa7a2abb :: TestObject Commit
commit_fa7a2abb = TestObject
    (sha1 "fa7a2abbf5e2457197ba973140fdbba3ad7b47ca")
    "commit 242\NUL"
    "tree 56558e3275b57381cd04d6cb604dde2f7e773166\n\
      \parent 3b2f2c262fe95a693b3cda2c18c2c4b16d29dc5e\n\
      \author Paul Weaver <paul@concertdaw.co.uk> 1522328367 +0100\n\
      \committer Paul Weaver <paul@concertdaw.co.uk> 1522328367 +0100\n\
      \\n\
      \Fix module name in test\n" $
    Commit
      (sha1 "56558e3275b57381cd04d6cb604dde2f7e773166")
      [sha1 "3b2f2c262fe95a693b3cda2c18c2c4b16d29dc5e"]
      "Paul Weaver" "paul@concertdaw.co.uk" (t 58206 13 59 27)
      "Paul Weaver" "paul@concertdaw.co.uk" (t 58206 13 59 27)
      "Fix module name in test\n"
  where
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

tree_56558e32 :: TestObject Tree
tree_56558e32 = TestObject
    (sha1 "56558e3275b57381cd04d6cb604dde2f7e773166")
    "tree 400\NUL"
    "100644 .gitignore\NUL\
      \\ENQX\EM\202\EOT+5n\ACKR\137\140\239\154P~Yb\SUB\239\&\
      \100644 .travis.yml\NUL\
      \\209\154\192\GS\143H\255\185\n\DEL\228Q\DEL\216\141\206\152\134\136^\
      \100644 ChangeLog.md\NUL\
      \R}\141C\151\133\234\157\r\213c\172\192\205\182P\208\168Vn\
      \100644 LICENSE\NUL\
      \\224\&7\199)\165,\138n\229@X\t\162\214\186\178\v\194yT\
      \100644 README.md\NUL\
      \\175\224/\EOT\244\147\163\156\181\153_\162\143\164\207\206\NAKm\185\ENQ\
      \100644 Setup.hs\NUL\
      \\154\153J\246w\176\223\212\ESCN;v\179\231\230\EOT\NUL=d\225\&\
      \40000 app\NUL\148\199\227\162\180s\214r\177n\173\DC3>\240\ETX\RS\t\246jZ\
      \100644 hs-git-tools.cabal\NUL\
      \\164p\DC4\137\224\136\188\200\255 \a)-\170\229\211\EM9\248\196\&\
      \40000 src\NUL\
      \\197\188\152 \204\169ytp\US}\247b\173P8\255\173\247\213\&\
      \100644 stack.yaml\NUL\
      \\162\234\160*\172<\169^\227\FSo\151O\128\US)0eu\159\&\
      \40000 test\NUL\
      \d\247\\L\248 \SO\129\f\ETXG\GSH0\160-\177\147#{" $
    Tree $ Map.fromList $
      [ r NonExecFile ".gitignore" "055819ca042b356e0652898cef9a507e59621aef"
      , r NonExecFile ".travis.yml" "d19ac01d8f48ffb90a7fe4517fd88dce9886885e"
      , r NonExecFile "ChangeLog.md" "527d8d439785ea9d0dd563acc0cdb650d0a8566e"
      , r NonExecFile "LICENSE" "e037c729a52c8a6ee5405809a2d6bab20bc27954"
      , r NonExecFile "README.md" "afe02f04f493a39cb5995fa28fa4cfce156db905"
      , r NonExecFile "Setup.hs" "9a994af677b0dfd41b4e3b76b3e7e604003d64e1"
      , r Directory "app" "94c7e3a2b473d672b16ead133ef0031e09f66a5a"
      , r NonExecFile "hs-git-tools.cabal"
            "a4701489e088bcc8ff2007292daae5d31939f8c4"
      , r Directory "src" "c5bc9820cca97974701f7df762ad5038ffadf7d5"
      , r NonExecFile "stack.yaml" "a2eaa02aac3ca95ee31c6f974f801f293065759f"
      , r Directory "test" "64f75c4cf8200e810c03471d4830a02db193237b"
      ]
  where
    r mode name sha = (name, TreeRow mode (sha1 sha))

instance Arbitrary Tree where
  arbitrary = Tree . Map.fromList <$> listOf rowPair
    where
      rowPair = (,) <$> nonNull <*> arbitrary
      nonNull = Text.filter (/= '\NUL') <$> arbitrary

instance Arbitrary TreeRow where
  arbitrary = TreeRow <$> arbitraryBoundedEnum <*> arbitrary

blob_527d8d43 :: TestObject Blob
blob_527d8d43 = TestObject
    (sha1 "527d8d439785ea9d0dd563acc0cdb650d0a8566e")
    "blob 46\NUL"
    "# Changelog for hs-git\n\n## Unreleased changes\n" $
    Blob $ "# Changelog for hs-git\n\n## Unreleased changes\n"

instance Arbitrary Blob where
  arbitrary = Blob <$> arbitrary
