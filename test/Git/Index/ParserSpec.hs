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
             ) shaNor mempty
       }
  it "should decode a real normal v3 tree" $
     (lazyParseOnly indexP v3NormalIndex :: Either String Index)
     `shouldBe`
     Right (index Version3)
       { indexEntries = Map.fromList [
            (
               Path.rel "bar.txt",
               Normal $ IndexEntry
               ( GitFileStat
                  1523885035.011432596 1523885035.011432596
                  2052 5508930
                  NonExecFile
                  1000 1000 0
               ) shaNor mempty
            ),
            (
               Path.rel "foo.a",
               Normal (IndexEntry gitFileStat shaNor mempty)
            )
        ]
       }
  it "should decode a real normal v4 tree" $
     (lazyParseOnly indexP v4NormalIndex :: Either String Index)
     `shouldBe`
     Right (index Version4)
       { indexEntries = Map.singleton (Path.rel "bar.txt") $
           Normal $ IndexEntry
             ( GitFileStat
                 1524152171.351220571 1524152171.351220571
                 2052 5508930
                 NonExecFile
                 1000 1000 0
             ) shaNor mempty
       }
  it "should decode a real conflicting v2 tree" $
     (lazyParseOnly indexP v2ConflictIndex :: Either String Index)
     `shouldBe`
     Right (index Version2)
       { indexEntries = Map.singleton (Path.rel "f.txt") $
           EditedRm
             (IndexEntry gitFileStat shaV2Cona mempty)
             (IndexEntry gitFileStat shaV2Conb mempty)
       }

-- NB: the \NUL padding on the end of the first file name is hard to distinguish
-- from the \NULs that git fills in for the stat information on conflicting
-- files:
v2ConflictIndex :: BS.ByteString
v2ConflictIndex = "DIRC\NUL\NUL\NUL\STX\NUL\NUL\NUL\STX\
    \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\
    \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\129\164\NUL\NUL\NUL\NUL\
    \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\
    \XR\244F9\245-\182}0\173\145C\184j\251\DC4=A_\
    \\DLE\ACK\
    \f.txt\NUL\NUL\NUL\NUL\NUL\
    \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\
    \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\129\164\NUL\NUL\NUL\NUL\
    \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\
    \\ESC-\EOT\135\178\130\210\243\n\160\DLE\EOT6\144\232R\STXl\NAKD\
    \ \ACK\
    \f.txt\NUL\NUL\NUL\NUL\NUL\
    \H\FS\n\147\DC4:7\\X\161\165\232\215B\134\250Yh\168\163"

shaV2Cona, shaV2Conb :: Sha1
shaV2Cona = Sha1.unsafeSha1 "XR\244F9\245-\182}0\173\145C\184j\251\DC4=A_"
shaV2Conb = Sha1.unsafeSha1 "\ESC-\EOT\135\178\130\210\243\n\160\DLE\EOT6\144\232R\STXl\NAKD"

v2NormalIndex :: BS.ByteString
v2NormalIndex = "DIRC\NUL\NUL\NUL\STX\NUL\NUL\NUL\SOH\
    \Z\212\163\235\NUL\174r\148Z\212\163\235\
    \\NUL\174r\148\NUL\NUL\b\EOT\NULT\SIB\
    \\NUL\NUL\129\164\NUL\NUL\ETX\232\NUL\NUL\ETX\232\
    \\NUL\NUL\NUL\NUL\
    \\230\157\226\155\178\209\214CK\139)\174wZ\216\194\228\140S\145\
    \\NUL\a\
    \bar.txt\NUL\NUL\NUL\
    \\135\173\157o\178S\v\vf\140\130N\236&\130\128ys\240I"

v3NormalIndex :: BS.ByteString
v3NormalIndex = "DIRC\NUL\NUL\NUL\ETX\NUL\NUL\NUL\STX\
    \Z\216\183k\DC4\239\&3[Z\216\183\
    \k\DC4\239\&3[\NUL\NUL\b\EOT\NULT\
    \\SIB\NUL\NUL\129\164\NUL\NUL\ETX\232\NUL\NUL\
    \\ETX\232\NUL\NUL\NUL\NUL\
    \\230\157\226\155\178\209\214CK\139)\174wZ\216\194\228\140S\145\
    \\NUL\a\
    \bar.txt\NUL\NUL\NUL\
    \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\
    \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\
    \\NUL\NUL\129\164\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\
    \\NUL\NUL\NUL\NUL\
    \\230\157\226\155\178\209\214CK\139)\174wZ\216\194\228\140S\145\
    \@\ENQ \NUL\
    \foo.a\NUL\NUL\NUL\
    \\194\192 1\187\232\f\171\146\228#\182\203=\170\v\227\USB!"

v4NormalIndex :: BS.ByteString
v4NormalIndex = "DIRC\NUL\NUL\NUL\EOT\NUL\NUL\NUL\SOH\
    \Z\216\183k\DC4\239\&3[Z\216\183\
    \k\DC4\239\&3[\NUL\NUL\b\EOT\NULT\
    \\SIB\NUL\NUL\129\164\NUL\NUL\ETX\232\NUL\NUL\
    \\ETX\232\NUL\NUL\NUL\NUL\
    \\230\157\226\155\178\209\214CK\139)\174wZ\216\194\228\140S\145\
    \\NUL\a\
    \\NULbar.txt\NUL\
    \\139 \162FI\DC1\229\226\190\\\223\229\247 y\159\209\145B7"

shaNor :: Sha1
shaNor = Sha1.unsafeSha1 "\230\157\226\155\178\209\214CK\139)\174wZ\216\194\228\140S\145"
