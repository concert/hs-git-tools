{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module Git.Index.ParserSpec where

import Test.Hspec

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified System.Path as Path

import Git.Internal (lazyParseOnly)
import Git.Sha1 (Sha1)
import qualified Git.Sha1 as Sha1

import Git.Index.Index (Index(..), index)
import Git.Index.Extensions (CachedTree(..), CachedTreeRow(..), ResolveUndo(..))
import Git.Index.Types
  (Stages(..), IndexVersion(..), IndexEntry(..), gitFileStat)
import Git.Index.Parser (indexP)

deriving instance Eq a => Eq (Stages a)
deriving instance Eq CachedTreeRow
deriving instance Eq CachedTree
deriving instance Eq ResolveUndo
deriving instance Eq Index

spec :: Spec
spec = describe "Parser" $ do
  it "should decode a real normal v2 tree" $ pending
  it "should decode a real normal v3 tree" $ pending
  it "should decode a real normal v4 tree" $ pending
  it "should decode a real conflicting v2 tree" $
     (lazyParseOnly indexP v2ConflictIndex :: Either String Index)
     `shouldBe`
     Right (index Version2)
       { indexEntries = Map.singleton (Path.rel "f.txt") $
           EditedRm
             (IndexEntry gitFileStat sha1a mempty)
             (IndexEntry gitFileStat sha1b mempty)
       }

-- NB: the \NUL padding on the end of the first file name is hard to distinguish
-- from the \NULs that git fills in for the stat information on conflicting
-- files:
v2ConflictIndex :: BS.ByteString
v2ConflictIndex = "DIRC\NUL\NUL\NUL\STX\NUL\NUL\NUL\STX\
    \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\
    \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\129\164\NUL\NUL\NUL\NUL\
    \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\
    \;\CAN\229\DC2\219\167\158L\131\NUL\221\b\174\179\DEL\142r\139\141\173\
    \\DLE\ENQ\
    \f.txt\NUL\NUL\NUL\NUL\NUL\
    \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\
    \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\129\164\NUL\NUL\NUL\NUL\
    \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\
    \oLp\209\203\ENQ\154\247\ETX\162\206\&6\ENQ\209d\b\232\207\SYN\207\
    \ \ENQ\
    \f.txt\NUL\NUL\NUL\NUL\NUL\
    \\216\214\142\221\224)\227\&3\219|\195-~\233\170D7\152\208l"

sha1a, sha1b :: Sha1
sha1a = Sha1.unsafeSha1 ";\CAN\229\DC2\219\167\158L\131\NUL\221\b\174\179\DEL\142r\139\141\173"
sha1b = Sha1.unsafeSha1 "oLp\209\203\ENQ\154\247\ETX\162\206\&6\ENQ\209d\b\232\207\SYN\207"
