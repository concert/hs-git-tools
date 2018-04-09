{-# LANGUAGE FlexibleContexts #-}

module Git.Index.Types where

import Control.Monad.Except (MonadError(..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word
import System.Posix.Types (DeviceID, FileID, UserID, GroupID)

import Git.Types (Sha1, FileMode, GitError(..))


type FilePathText = Text

data IndexVersion =
  Version2 | Version3 | Version4
  deriving (Show, Eq, Ord, Enum, Bounded)

versionToWord32 :: IndexVersion -> Word32
versionToWord32 v = case v of
  Version2 -> 2
  Version3 -> 3
  Version4 -> 4

versionFromWord32 :: MonadError GitError m => Word32 -> m IndexVersion
versionFromWord32 w = maybe (throwError UnsupportedIndexVersion) return $
  lookup w [(versionToWord32 v, v) | v <- [minBound..]]

data Flag = AssumeValid | SkipWorkTree | IntentToAdd deriving (Show, Eq, Ord)

type Stage = Word8
type IndexEntries = Map (FilePathText, Stage) IndexEntry
data Index
  = Index
  { indexVersion :: IndexVersion
  , indexEntries :: IndexEntries
  } deriving Show

data IndexEntry
  = IndexEntry
  { ieMetaDataChangedAt :: POSIXTime
  , ieChangedAt :: POSIXTime
  , ieDevId :: DeviceID
  , ieInodeNo :: FileID
  , ieMode :: FileMode
  , ieUid :: UserID
  , ieGid :: GroupID
  , ieSize :: Word32  -- System.Posix.Types.FileOffset truncated to 32-bit
  , ieSha1 :: Sha1
  , ieFlags :: Set Flag
  } deriving (Show, Eq)
