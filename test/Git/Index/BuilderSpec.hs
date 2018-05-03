{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    BinaryLiterals
#-}

module Git.Index.BuilderSpec where

import Test.Hspec

import qualified Data.Map as Map
import qualified System.Path as Path

import Git.Index.Builder (indexLbs, encodePathV4)
import Git.Index.Index (Index(..), index)
import Git.Index.Types
  (IndexVersion(..), IndexEntry(..), gitFileStat, Stages(..))

import Git.Index.ParserSpec (v2ConflictIndex, sha1V2Cona, sha1V2Conb)


spec :: Spec
spec = do
  it "should encode a real conflicting v2 index" $
    let
      bs = indexLbs $ (index Version2)
        { indexEntries = Map.singleton (Path.rel "f.txt") $
            EditedRm
              (IndexEntry gitFileStat sha1V2Cona mempty)
              (IndexEntry gitFileStat sha1V2Conb mempty)
        }
    in
      bs `shouldBe` v2ConflictIndex

  describe "encodePathV4" $ do
    it "should return the path if given no previous path" $
      encodePathV4 "" "foo/bar" `shouldBe` "\NULfoo/bar\NUL"
    it "should remove a common substring of the previous path" $
      encodePathV4 "foo/bar" "foo/baz" `shouldBe` "\SOHz\NUL"
    it "should cope with adding onto the path" $
      encodePathV4 "foo/" "foo/bar" `shouldBe` "\NULbar\NUL"
