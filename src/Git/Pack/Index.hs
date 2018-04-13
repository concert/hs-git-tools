{-# LANGUAGE FlexibleContexts #-}

module Git.Pack.Index where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (MonadState(..))
import Control.Exception (try)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word
import System.IO.MMap (mmapFileForeignPtr, Mode(..))
import Text.Printf (printf)

import Git.Internal
  ( MmapHandle, MmapFrom(..), MmapTo(..), mmapData
  , mmapWord32be, mmapWord64be, mmapSha1)
import Git.Sha1 (Sha1(..), sha1Size)
import Git.Types (GitError(..))

data PackIndexVersion = Version1 | Version2 deriving (Show, Eq, Enum, Bounded)

data PackIndexState
  = PackIndexStateV1
    { pisMmap :: MmapHandle
    , pisRecordNos :: Map Sha1 Word32
    , pisV1RecordOffsets :: Map Sha1 Word32
    , pisPackFileSha1 :: Maybe Sha1
    , pisSha1 :: Maybe Sha1}
  | PackIndexStateV2
    { pisMmap :: MmapHandle
    , pisRecordNos :: Map Sha1 Word32
    , pisRecordCrcs :: Map Sha1 Word32
    , pisV2RecordOffsets :: Map Sha1 Word64
    , pisPackFileSha1 :: Maybe Sha1
    , pisSha1 :: Maybe Sha1}

packIndexState :: PackIndexVersion -> MmapHandle -> PackIndexState
packIndexState v h = case v of
  Version1 -> PackIndexStateV1 h mempty mempty Nothing Nothing
  Version2 -> PackIndexStateV2 h mempty mempty mempty Nothing Nothing

pisVersion :: PackIndexState -> PackIndexVersion
pisVersion PackIndexStateV1 {} = Version1
pisVersion PackIndexStateV2 {} = Version2

instance Show PackIndexState where
  show pis =
    let
      v = pisVersion pis
      totRecs = getPackIndexTotalRecords pis
    in
      printf "<PackIndexState: %s, records: %d>" (show v) totRecs

openPackIndex
  :: (MonadIO m, MonadError GitError m) => FilePath -> m PackIndexState
openPackIndex indexPath = do
  h <- liftIO $ mmapFileForeignPtr indexPath ReadOnly Nothing
  v <- getPackIndexVersion h
  return $ packIndexState v h

withPackIndex
  :: MonadIO m
  => FilePath -> StateT PackIndexState (ExceptT GitError m) r
  -> m (Either GitError r)
withPackIndex path m = runExceptT $ do
  h <- excTIO $ mmapFileForeignPtr path ReadOnly Nothing
  v <- getPackIndexVersion h
  res <- evalStateT m (packIndexState v h)
  return res

packIndexHeaderSize :: PackIndexVersion -> Int
packIndexHeaderSize Version1 = 0
packIndexHeaderSize Version2 = 8

packIndexDataStart :: PackIndexVersion -> Int
packIndexDataStart v = packIndexHeaderSize v + 256 * 4

getPackIndexVersion
  :: MonadError GitError m => MmapHandle -> m PackIndexVersion
getPackIndexVersion h = case mmapData h (FromStart 0) (Length 4) of
    "\255tOc" -> let version = mmapWord32be h (FromStart 4) in
      if version == 2
        then return Version2
        else throwError UnsupportedPackIndexVersion
    _ -> return Version1

getPackIndexSha1
  :: (MonadState PackIndexState m, MonadError GitError m) => m Sha1
getPackIndexSha1 = do
    pis <- get
    case pisSha1 pis of
      Just sha1 -> return sha1
      Nothing -> let sha1 = mmapSha1 (pisMmap pis) (FromEnd sha1Size) in do
        put $ pis {pisSha1 = Just sha1}
        return sha1

getPackSha1FromIndex
  :: (MonadState PackIndexState m, MonadError GitError m) => m Sha1
getPackSha1FromIndex = do
    pis <- get
    case pisPackFileSha1 pis of
      Just sha1 -> return sha1
      Nothing ->
        let sha1 = mmapSha1 (pisMmap pis) (FromEnd $ 2 * sha1Size) in do
          put $ pis {pisPackFileSha1 = Just sha1}
          return sha1

excTIO :: (MonadIO m, MonadError GitError m) => IO a -> m a
excTIO io = liftIO (try io) >>= either (throwError . ErrorWithIO) return

getFanoutTableEntry :: PackIndexState -> Word8 -> Word32
getFanoutTableEntry pis fotIdx = mmapWord32be (pisMmap pis) $ FromStart $
  packIndexHeaderSize (pisVersion pis) + 4 * (fromIntegral fotIdx)

-- | Examines the pack index file's fan out table to determine the minimum
--   and maximum record numbers for the given SHA1
getPackIndexRecordNoBounds :: PackIndexState -> Sha1 -> (Word32, Word32)
getPackIndexRecordNoBounds pis sha1 =
  let
    fotIdx = BS.head $ unSha1 sha1
    minRn = if fotIdx == 0 then 0 else getFanoutTableEntry pis (fotIdx - 1)
    maxRn = getFanoutTableEntry pis fotIdx
  in
    (minRn, maxRn)

getPackIndexTotalRecords :: PackIndexState -> Word32
getPackIndexTotalRecords pis = getFanoutTableEntry pis 255

getPackIndexRecordNo
  :: (MonadState PackIndexState m, MonadError GitError m)
  => Sha1 -> m Word32
getPackIndexRecordNo sha1 = do
  pis <- get
  case Map.lookup sha1 $ pisRecordNos pis of
    Just recordNo -> return recordNo
    Nothing -> case pisVersion pis of
      Version1 -> throwError UnsupportedPackIndexVersion
      Version2 -> do
          let (minRecordNo, maxRecordNo) = getPackIndexRecordNoBounds pis sha1
          recordNo <- findSha1Idx minRecordNo maxRecordNo $ getSha1 $ pisMmap pis
          put $ pis {pisRecordNos = Map.insert sha1 recordNo $ pisRecordNos pis}
          return recordNo
  where
    getSha1 h i = mmapSha1 h $ FromStart $
      packIndexDataStart Version2 + sha1Size * fromIntegral i
    findSha1Idx
      :: MonadError GitError m
      => Word32 -> Word32 -> (Word32 -> Sha1) -> m Word32
    findSha1Idx minRn maxRn fetch = let candidate = (minRn + maxRn) `div` 2 in
      if minRn > maxRn then throwError Sha1NotInIndex else
      case compare (fetch candidate) sha1 of
        LT -> findSha1Idx (candidate + 1) maxRn fetch
        EQ -> return candidate
        GT -> findSha1Idx minRn (candidate - 1) fetch

getPackRecordCrc
  :: (MonadState PackIndexState m, MonadError GitError m)
  => Sha1 -> m Word32
getPackRecordCrc sha1 = do
  pis <- get
  case pisVersion pis of
    Version1 -> throwError $ UnsupportedOperation
      "getPackRecordCrc: version 1 pack files do not contain crc"
    Version2 -> case Map.lookup sha1 $ pisRecordCrcs pis of
      Just crc -> return crc
      Nothing -> do
        let totRecs = getPackIndexTotalRecords pis
        recordNo <- getPackIndexRecordNo sha1
        let crc = mmapWord32be (pisMmap pis) $ FromStart $
              packIndexDataStart Version2
              + sha1Size * fromIntegral totRecs
              + 4 * fromIntegral recordNo
        put $ pis {pisRecordCrcs = Map.insert sha1 crc $ pisRecordCrcs pis}
        return crc

getPackRecordOffset
  :: (MonadState PackIndexState m, MonadError GitError m)
  => Sha1 -> m Word64
getPackRecordOffset sha1 =
  do
    pis <- get
    case pisVersion pis of
      Version1 -> throwError UnsupportedPackIndexVersion
      Version2 -> case Map.lookup sha1 $ pisV2RecordOffsets pis of
        Just offset -> return offset
        Nothing -> do
          let totRecs = getPackIndexTotalRecords pis
          recordNo <- getPackIndexRecordNo sha1
          let offset = readOffset (pisMmap pis) totRecs recordNo
          put $ pis
            { pisV2RecordOffsets = Map.insert sha1 offset $
              pisV2RecordOffsets pis }
          return offset
  where
    readOffset h totRecs recordNo =
      let offset4Byte = mmapWord32be h $ FromStart $
            packIndexDataStart Version2
            + sha1Size * fromIntegral totRecs
            + 4 * (fromIntegral totRecs + fromIntegral recordNo)
      in if offset4Byte .&. 2 ^ (31 :: Int) == 0
        -- MSB not set: just return first 31 bits as Word64 offset
        then fromIntegral offset4Byte
        -- MSB is set: go read the large object table
        else let largeOffsetIdx = offset4Byte .&. (2 ^ (31 :: Int) - 1) in
            mmapWord64be h $ FromStart $
              packIndexDataStart Version2
              + sha1Size * fromIntegral totRecs
              + 8 * (fromIntegral totRecs + fromIntegral largeOffsetIdx)


{- Packfile index structure:

Word32 magic header: pack_index_sig
Word32 version number: 2

256 * Word32  Fanout table

n Sha1   of entry
n Word32 entry crc
(n - x) Word32 packfile offset of entry

if packfile > 2 GiB
x Word64 packfile offset of entry

Sha1   of referenced packfile
Sha1   file checksum

-}
