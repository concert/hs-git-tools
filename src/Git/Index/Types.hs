{-# LANGUAGE
    BinaryLiterals
  , DeriveFoldable
  , DeriveFunctor
#-}

module Git.Index.Types where

import Prelude hiding (fail)

import Control.Monad.Fail (MonadFail(..))
import Data.Bits ((.&.), shiftR)
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word
import qualified System.Path as Path
import System.Posix.Files
  (FileStatus, deviceID, fileID, fileMode, fileOwner, fileGroup, fileSize
  , statusChangeTimeHiRes, modificationTimeHiRes)
import System.Posix.Types
  ( CDev(..), DeviceID, CIno(..), FileID, CUid(..), UserID, CGid(..), GroupID
  , CMode(..))
import Text.Printf (printf)

import Git.Types (FileMode(..), fileModeFromInt)
import Git.Sha1 (Sha1)


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

data Flag
  = AssumeValid | SkipWorkTree | IntentToAdd
  deriving (Show, Eq, Ord, Enum, Bounded)

flagMinVersion :: Flag -> IndexVersion
flagMinVersion f = case f of
  AssumeValid -> minBound
  SkipWorkTree -> Version3
  IntentToAdd -> Version3

flagsMinVersion :: Set Flag -> IndexVersion
flagsMinVersion = foldl' (\v f -> max v $ flagMinVersion f) minBound

data Stage
  = StageNormal | StageBase | StageHead | StageIncoming
  deriving (Show, Eq, Ord, Enum, Bounded)

stageToInt :: Stage -> Int
stageToInt s = case s of
  StageNormal -> 0
  StageBase -> 1
  StageHead -> 2
  StageIncoming -> 3

intToStage :: MonadFail m => Int -> m Stage
intToStage i = maybe (fail $ printf "Bad stage value %d" i) return $
  lookup i [(stageToInt s, s) | s <- [minBound..]]

data Stages a
  = Normal a
  | BothAdded {ssHead :: a, ssIncoming :: a}
  | BothEdited {ssBase :: a, ssHead :: a, ssIncoming :: a}
  | RmEdited {ssBase :: a, ssIncoming :: a}
  | EditedRm {ssBase :: a, ssHead :: a}
  deriving (Show, Functor, Foldable)

stagesToMap :: Stages a -> Map Stage a
stagesToMap ses = case ses of
    Normal ie -> Map.singleton StageNormal ie
    BothAdded hd inc -> h hd <> i inc
    BothEdited base hd inc -> b base <> h hd <> i inc
    RmEdited base inc -> b base <> i inc
    EditedRm base hd -> b base <> h hd
  where
    b = Map.singleton StageBase
    h = Map.singleton StageHead
    i = Map.singleton StageIncoming

mapToStages :: MonadFail m => Map Stage a -> m (Stages a)
mapToStages m =
  let
    r = Map.lookup StageNormal m
    b = Map.lookup StageBase m
    h = Map.lookup StageHead m
    i = Map.lookup StageIncoming m
  in
    case (r, b, h, i) of
      (Just norm, Nothing, Nothing, Nothing) -> return $ Normal norm
      (Nothing, Nothing, Just hd, Just inc) -> return $ BothAdded hd inc
      (Nothing, Just base, Just hd, Just inc) -> return $ BothEdited base hd inc
      (Nothing, Just base, Nothing, Just inc) -> return $ RmEdited base inc
      (Nothing, Just base, Just hd, Nothing) -> return $ EditedRm base hd
      _ -> fail $ "Bad stage collection " ++ show (Map.keys m)

-- FIXME: I'm not sure index entries can ever be directories...
type IndexEntries = Map Path.RelFileDir (Stages IndexEntry)

iesMinVersion :: IndexEntries -> IndexVersion
iesMinVersion ies = (foldl' . foldl') max minBound $ fmap ieMinVersion <$> ies

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

gitFileStat :: GitFileStat
gitFileStat = GitFileStat 0 0 (CDev 0) (CIno 0) NonExecFile (CUid 0) (CGid 0) 0

data IndexEntry
  = IndexEntry
  { ieGfs :: GitFileStat
  , ieSha1 :: Sha1
  , ieFlags :: Set Flag
  } deriving (Show, Eq)

ieMinVersion :: IndexEntry -> IndexVersion
ieMinVersion = flagsMinVersion . ieFlags

gfsFromStat :: MonadFail m => FileStatus -> m GitFileStat
gfsFromStat fs = do
    fm <- normaliseFileMode $ fileMode fs
    return $ GitFileStat
      (statusChangeTimeHiRes fs) (modificationTimeHiRes fs)
      (deviceID fs) (fileID fs) fm
      (fileOwner fs) (fileGroup fs)
      (fromIntegral $ fileSize fs)

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
