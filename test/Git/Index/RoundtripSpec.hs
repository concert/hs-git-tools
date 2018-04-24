{-# OPTIONS_GHC -Wno-orphans #-}

module Git.Index.RoundtripSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen (chooseAny)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified System.Path as Path
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word
import System.Posix.Types (CDev(..), CIno(..), CUid(..), CGid(..))

import Git.Index.Builder (indexLbs)
import Git.Index.Extensions
  (CachedTree(..), CachedTreeRow(..), ResolveUndo(..), ResolveUndoStages(..))
import Git.Index.Index (Index(..))
import Git.Index.Parser (parseIndex, mkPosixTime)
import Git.Index.Types
  (IndexEntry(..), iesMinVersion, Stages(..), Flag(..)
  , GitFileStat(..), gitFileStat)
import Git.Types (FileMode(..))

import Git.Sha1Spec ()
import Git.Index.ParserSpec ()
import Git.Objects.SerialiseSpec (arbitraryGitPath)


instance Arbitrary Index where
  arbitrary = do
      entries <- pathyMapGen
      version <- oneof $ return <$> [iesMinVersion entries..]
      ct <- arbitrary
      ru <- arbitrary
      return $ Index version entries ct ru
  shrink (Index version entries ct ru) =
    [Index version es' ct' ru' | (es', ct', ru') <- shrink (entries, ct, ru)]

pathyMapGen :: Arbitrary a => Gen (Map Path.RelFileDir a)
pathyMapGen = Map.fromList <$> listOf ((,) <$> arbitraryGitPath <*> arbitrary)

instance Arbitrary a => Arbitrary (Stages a) where
  arbitrary = oneof
    [ Normal <$> arbitrary
    , BothAdded <$> arbitrary <*> arbitrary
    , BothEdited <$> arbitrary <*> arbitrary <*> arbitrary
    , RmEdited <$> arbitrary <*> arbitrary
    , EditedRm <$> arbitrary <*> arbitrary
    ]
  shrink s = case s of
    Normal _ -> []
    BothAdded hd inc -> Normal <$> [hd, inc]
    BothEdited base hd inc -> Normal <$> [base, hd, inc]
    RmEdited base inc -> Normal <$> [base, inc]
    EditedRm base hd -> Normal <$> [base, hd]

instance Arbitrary IndexEntry where
  arbitrary = IndexEntry <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary GitFileStat where
  arbitrary = GitFileStat <$> arbitraryTime <*> arbitraryTime
      <*> did <*> fid <*> arbitrary <*> uid <*> gid
      <*> arbitrary
    where
      did = CDev . fromIntegral <$> arbitrary @Word32
      fid = CIno . fromIntegral <$> arbitrary @Word32
      uid = CUid <$> arbitrary
      gid = CGid <$> arbitrary
  shrink gfs =
    if gfs == gitFileStat then [] else
      [ gfs {gfsMetaDataChangedAt = 0}
      , gfs {gfsChangedAt = 0}
      , gfs {gfsDevId = 0}
      , gfs {gfsInodeNo = 0}
      , gfs {gfsMode = NonExecFile}
      , gfs {gfsUid = 0}
      , gfs {gfsGid = 0}
      , gfs {gfsSize = 0}
      ]

arbitraryTime :: Gen POSIXTime
arbitraryTime = mkPosixTime <$> chooseAny <*> chooseAny

instance Arbitrary Flag where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CachedTree where
  arbitrary = CachedTree <$> pathyMapGen
  shrink (CachedTree ct) = CachedTree <$> shrink ct

instance Arbitrary CachedTreeRow where
  -- NB: this is nonsense: the cached tree data depends quite heavily on the
  -- index entries in real life!
  arbitrary = CachedTreeRow <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ResolveUndo where
  arbitrary = ResolveUndo . Map.fromList <$> listOf narg
    where
      narg = (,) <$> arbitraryGitPath <*> arbitrary
  shrink (ResolveUndo ru) = ResolveUndo <$> shrink ru

instance Arbitrary a => Arbitrary (ResolveUndoStages a) where
  arbitrary = oneof
    [ RuBothAdded <$> arbitrary <*> arbitrary
    , RuBothEdited <$> arbitrary <*> arbitrary <*> arbitrary
    , RuRmEdited <$> arbitrary <*> arbitrary
    , RuEditedRm <$> arbitrary <*> arbitrary
    ]

instance Arbitrary FileMode where
  arbitrary = arbitraryBoundedEnum

spec :: Spec
spec = do
  it "should consistently serialise and deserialise" $ property $ \idx ->
    let
      encoded = indexLbs idx
      roundtripped = parseIndex encoded :: Either String Index
    in
      counterexample (show encoded) $
      counterexample (show roundtripped) $
      roundtripped == Right idx
