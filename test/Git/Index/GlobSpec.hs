{-# LANGUAGE
    LambdaCase #-}

module Git.Index.GlobSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Attoparsec.ByteString.Char8 (parseOnly, char)
import qualified Data.ByteString.Char8 as Char8
import Data.Either (isLeft, isRight)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))

import Git.Index.Glob
  ( GlobPart(..), condenseConsecutive, star, matchGlob, escape, specialChars
  , checkPathComponents, glob)

import Git.Objects.SerialiseSpec (listOf')


spec :: Spec
spec = do
  describe "condenseConsecutive" $ do
    it "should eliminate required consecutive elements" $
      condenseConsecutive 'a' "aabbababbaa" `shouldBe` "abbababba"
    it "should leave a 1 element list as is" $ do
      condenseConsecutive 'a' "a" `shouldBe` "a"

  describe "star Parser" $ let p = star $ char 'z' in do
    it "should parse terminated empty string" $
      parseOnly p "z" `shouldSatisfy` isRight
    it "should parse terminated non-empty string" $
      parseOnly p "aaz" `shouldSatisfy` isRight
    it "should not parse empty string" $
      parseOnly p "" `shouldSatisfy` isLeft
    it "should not parse non-terminated non-empty" $
      parseOnly p "aa" `shouldSatisfy` isLeft

  describe "single glob part" $ do
    it "should match correct strings" $ forAll (genDepPair arbitrary genGood) $
      \(gp, goodBs) -> matchGlob [gp] goodBs
    it "should reject incorrect strings" $
      forAll (genMaybeDepPair arbitrary genBad) $ \case
        Nothing -> True
        Just (gp, badBs) -> matchGlob [gp] badBs == False
      -- FIXME: star is awkward for ^^ because it has no failing instances!

  describe "glob parts" $ do
    it "should match correct strings" $
      forAll (genDepPair (listOf' 0 5 arbitrary) genGood') $ \(gps, goodBs) ->
       matchGlob gps goodBs == True
    -- FIXME: not sure this is possible in general:
    -- it "should reject incorrect strings" $
    --   forAll (genDepList (listOf' 0 5 arbitrary) genBad) $ \l -> (l /= []) ==>
    --     let (gps, bss) = unzip l in matchGlob gps (mconcat bss) == False
    it "should reject some known incorrect strings" $ do
      matchGlob [QuestionMark, QuestionMark] "" `shouldBe` False
      matchGlob [QuestionMark, QuestionMark] "abc" `shouldBe` False
      matchGlob [Name "hi", QuestionMark] "hilo" `shouldBe` False
      matchGlob [Name "foo", QuestionMark, Name "bar"] "foobar" `shouldBe` False
      matchGlob [Name "foo", QuestionMark, Name "bar"] "fooaabar"
        `shouldBe` False
      matchGlob [Star, QuestionMark] "" `shouldBe` False
      matchGlob [Star, QuestionMark, QuestionMark] "a" `shouldBe` False
      matchGlob [QuestionMark, Star] "" `shouldBe` False

  describe "checkPathComponents" $
    let f = fmap $ Right . glob . pure . Name in do
    it "should match a simple name" $
      checkPathComponents (f ["foo"]) ["foo"] `shouldBe` True
    it "should match a name series" $
      checkPathComponents (f ["foo", "bar", "baz"]) ["foo", "bar", "baz"]
        `shouldBe` True

shouldBes :: (Show a, Eq a) => [(a, a)] -> Expectation
shouldBes = mapM_ $ uncurry shouldBe

instance Arbitrary GlobPart where
  arbitrary = oneof
    [ return Star
    , return QuestionMark
    , uncurry CharRange <$> (order <$> genChar <*> genChar)
    , Name <$> genBs]

genGood :: GlobPart -> Gen Char8.ByteString
genGood gp = case gp of
  Star -> arbitrary
  QuestionMark -> Char8.singleton <$> genChar
  CharRange c1 c2 -> Char8.singleton <$> choose (c1, c2)
  Name n -> return n

genGood' :: [GlobPart] -> Gen Char8.ByteString
genGood' = fmap mconcat . mapM genGood

genBad :: GlobPart -> Maybe (Gen Char8.ByteString)
genBad gp = case gp of
  Star -> Nothing
  QuestionMark -> Just $ oneof $ [return "", atLeastTwo]
  CharRange c1 c2 ->
    let badChars = esc <$> ['!'..pred c1] ++ [succ c2..'~'] in
    Just $ oneof [return "", atLeastTwo, elements badChars]
  Name n -> Just $ foldMap esc <$> notThis (Char8.unpack n)

genDepPair :: Gen a -> (a -> Gen b) -> Gen (a, b)
genDepPair g f = do
  a <- g
  b <- f a
  return (a, b)

genMaybeDepPair :: Gen a -> (a -> Maybe (Gen b)) -> Gen (Maybe (a, b))
genMaybeDepPair g f = do
  a <- g
  case f a of
    Nothing -> return Nothing
    Just g' -> Just . (a,) <$> g'

genDepList :: Gen [a] -> (a -> Maybe (Gen b)) -> Gen [(a, b)]
genDepList g f = do
  as <- g
  sequence $ fmap sequence $ mapMaybe (\a -> sequence (a, f a)) as

atLeastTwo :: Gen Char8.ByteString
atLeastTwo = do
  a1 <- esc <$> genChar
  a2 <- esc <$> genChar
  as <- foldMap esc <$> arbitrary @String
  return $ a1 <> a2 <> as

esc :: Char -> Char8.ByteString
esc = Char8.pack . escape

genChar :: Gen Char
genChar = choose ('!', '~')

genBs :: Gen Char8.ByteString
genBs = Char8.pack <$> listOf' 0 100 genChar

order :: Ord a => a -> a -> (a, a)
order a1 a2 = if a2 > a1 then (a1, a2) else (a2, a1)

notThis :: (Eq a, Arbitrary a) => a -> Gen a
notThis a = do
  a' <- arbitrary
  if a' == a then notThis a else return a'
