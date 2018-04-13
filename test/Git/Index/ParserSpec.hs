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

import Git.Index.Types
  (Stages(..), Index(..), IndexVersion(..), IndexEntry(..), gitFileStat)
import Git.Index.Parser (indexP)

deriving instance Eq a => Eq (Stages a)
deriving instance Eq Index

spec :: Spec
spec = describe "Parser" $ do
  it "should decode a real normal v2 tree" $ pending
  it "should decode a real normal v3 tree" $ pending
  it "should decode a real normal v4 tree" $ pending
  it "should decode a real conflicting v2 tree" $
     (lazyParseOnly indexP v2ConflictIndex :: Either String Index)
     `shouldBe`
     Right (Index Version2 $ Map.singleton (Path.rel "f.txt")
            (EditedRm
             (IndexEntry gitFileStat sha1a mempty)
             (IndexEntry gitFileStat sha1b mempty)
            )
           )

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

sha1a, sha1b :: Sha1
sha1a = Sha1.unsafeSha1 "XR\244F9\245-\182}0\173\145C\184j\251\DC4=A_"
sha1b = Sha1.unsafeSha1 "\ESC-\EOT\135\178\130\210\243\n\160\DLE\EOT6\144\232R\STXl\NAKD"
