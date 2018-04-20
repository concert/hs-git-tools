{-# OPTIONS_GHC -Wno-orphans #-}

module Git.Index.BuilderSpec where

import Test.Hspec

import qualified Data.Map as Map
import qualified System.Path as Path

import Git.Index.Builder (indexLbs)
import Git.Index.Index (Index(..), index)
import Git.Index.Types
  (IndexVersion(..), IndexEntry(..), gitFileStat, Stages(..))

import Git.Index.ParserSpec (v2ConflictIndex, sha1a, sha1b)


spec :: Spec
spec = do
  it "should encode a real conflicting v2 index" $
    let
      bs = indexLbs $ (index Version2)
        { indexEntries = Map.singleton (Path.rel "f.txt") $
            EditedRm
              (IndexEntry gitFileStat sha1a mempty)
              (IndexEntry gitFileStat sha1b mempty)
        }
    in
      bs `shouldBe` v2ConflictIndex
