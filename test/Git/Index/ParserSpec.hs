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
  ( Stages(..), IndexVersion(..), IndexEntry(..), GitFileStat(..), gitFileStat)
import Git.Index.Parser (indexP)
import Git.Types (FileMode(..))

deriving instance Eq a => Eq (Stages a)
deriving instance Eq CachedTreeRow
deriving instance Eq CachedTree
deriving instance Eq ResolveUndo
deriving instance Eq Index

spec :: Spec
spec = describe "Parser" $ do
  it "should decode a real normal v2 tree" $
     (lazyParseOnly indexP v2NormalIndex :: Either String Index)
     `shouldBe`
     Right (index Version2)
       { indexEntries = Map.singleton (Path.rel "bar.txt") $
           Normal $ IndexEntry
             ( GitFileStat
                 1523885035.011432596 1523885035.011432596
                 2052 5508930
                 NonExecFile
                 1000 1000 0
             ) shaV2Nor mempty
       }
  it "should decode a real normal v3 tree" $ pending
  it "should decode a real normal v4 tree" $ pending
  it "should decode a real conflicting v2 tree" $
     (lazyParseOnly indexP v2ConflictIndex :: Either String Index)
     `shouldBe`
     Right (index Version2)
       { indexEntries = Map.singleton (Path.rel "f.txt") $
           EditedRm
             (IndexEntry gitFileStat sha1V2Cona mempty)
             (IndexEntry gitFileStat sha1V2Conb mempty)
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

sha1V2Cona, sha1V2Conb :: Sha1
sha1V2Cona = Sha1.unsafeSha1 ";\CAN\229\DC2\219\167\158L\131\NUL\221\b\174\179\DEL\142r\139\141\173"
sha1V2Conb = Sha1.unsafeSha1 "oLp\209\203\ENQ\154\247\ETX\162\206\&6\ENQ\209d\b\232\207\SYN\207"

v2NormalIndex :: BS.ByteString
v2NormalIndex = "DIRC\NUL\NUL\NUL\STX\NUL\NUL\NUL\SOHZ\
    \\212\163\235\NUL\174r\148Z\212\163\235\NUL\174r\148\NUL\NUL\b\EOT\
    \\NULT\SIB\NUL\NUL\129\164\NUL\NUL\ETX\232\NUL\NUL\ETX\232\NUL\NUL\
    \\NUL\NUL\
    \\230\157\226\155\178\209\214CK\139)\174wZ\216\194\228\140S\145\
    \\NUL\a\
    \bar.txt\NUL\NUL\NUL\
    \\135\173\157o\178S\v\vf\140\130N\236&\130\128ys\240I"

shaV2Nor :: Sha1
shaV2Nor = Sha1.unsafeSha1 "\230\157\226\155\178\209\214CK\139)\174wZ\216\194\228\140S\145"
