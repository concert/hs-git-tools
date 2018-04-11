{-# LANGUAGE
    BinaryLiterals
  , FlexibleContexts
#-}

module Git.Index.Types where

import Prelude hiding (fail)

import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Except (MonadError(..))
import Data.Bits ((.&.), shiftR)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word
import qualified System.Path as Path
import System.Posix.Files
  (FileStatus, deviceID, fileID, fileMode, fileOwner, fileGroup, fileSize
  , statusChangeTimeHiRes, modificationTimeHiRes)
import System.Posix.Types
  (DeviceID, FileID, UserID, GroupID, FileOffset, CMode(..))

import Git.Types (Sha1, FileMode(..), fileModeFromInt, GitError(..))


data IndexVersion =
  Version2 | Version3 | Version4
  deriving (Show, Eq, Ord, Enum, Bounded)

versionToWord32 :: IndexVersion -> Word32
versionToWord32 v = case v of
  Version2 -> 2
  Version3 -> 3
  Version4 -> 4

versionFromWord32 :: MonadFail m => Word32 -> m IndexVersion
versionFromWord32 w = maybe (fail "Unsupported index version") return $
  lookup w [(versionToWord32 v, v) | v <- [minBound..]]

data Flag = AssumeValid | SkipWorkTree | IntentToAdd deriving (Show, Eq, Ord)

type Stage = Word8
type IndexEntries = Map Path.RelFileDir (Map  Stage IndexEntry)
data Index
  = Index
  { indexVersion :: IndexVersion
  , indexEntries :: IndexEntries
  } deriving Show

data GitFileStat
  = GitFileStat
  { gfsMetaDataChangedAt :: POSIXTime
  , gfsChangedAt :: POSIXTime
  , gfsDevId :: DeviceID
  , gfsInodeNo :: FileID
  , gfsMode :: FileMode
  , gfsUid :: UserID
  , gfsGid :: GroupID
  , gfsSize :: Word32  -- System.Posix.Types.FileOffset truncated to 32-bit
  } deriving (Show, Eq)

data IndexEntry
  = IndexEntry
  { ieGfs :: GitFileStat
  , ieSha1 :: Sha1
  , ieFlags :: Set Flag
  } deriving (Show, Eq)

gfsFromStat :: MonadFail m => FileStatus -> m GitFileStat
gfsFromStat fs = do
    fm <- normaliseFileMode $ fileMode fs
    return $ GitFileStat
      (statusChangeTimeHiRes fs) (modificationTimeHiRes fs)
      (deviceID fs) (fileID fs) fm
      (fileOwner fs) (fileGroup fs)
      (boundSize $ fileSize fs)
  where
    boundSize :: FileOffset -> Word32
    boundSize fo | fo <= fromIntegral (maxBound @Word32) = fromIntegral fo
                 | otherwise = maxBound

-- | Heuristically convert some unsupported file modes into
--   supported ones, failing if there's no way to accomodate the file type.
normaliseFileMode :: MonadFail m => CMode -> m FileMode
normaliseFileMode (CMode i) = case fileTy of
  0o100 -> return normPerms
  _ -> fileModeFromInt $ fromIntegral i
  where
    fileTy = shiftR (i .&. 0o777000) 9
    userExec = 0b001000000 .&. i /= 0
    groupWrite =  0b010000 .&. i /= 0
    normPerms
      | userExec = ExecFile
      | groupWrite = NonExecGroupWriteFile
      | otherwise = NonExecFile
