{-# LANGUAGE FlexibleContexts #-}

module Git.Pack.Index where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (MonadState(..))
import Control.Exception (try)
import Data.Attoparsec.ByteString (Parser, parseOnly)
import Data.Attoparsec.Binary (anyWord32be, anyWord64be)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Combinators (mapLeft)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word
import System.IO
  (openBinaryFile, Handle, IOMode(..), hSeek, SeekMode(..), hClose)
import Text.Printf (printf)

import Git.Types.Internal (liftEither)
import Git.Types (Sha1(..), sha1Size)
import qualified Git.Types.Sha1 as Sha1

data Version = Version1 | Version2 deriving (Show, Eq, Enum, Bounded)

data GitError
  = ErrorWithIO IOError
  | ParseError String
  | UnsupportedPackIndexVersion
  | UnsupportedOperation String
  | Sha1NotInIndex

instance Show GitError where
  show e = case e of
    ErrorWithIO ioe -> "IO error: " ++ show ioe
    ParseError s -> "Parse error: " ++ s
    UnsupportedPackIndexVersion -> "Unsupported pack index version"
    UnsupportedOperation s -> "Unsupported operation: " ++ s
    Sha1NotInIndex -> "sha1 not in index"

type GitM a = ExceptT GitError a

data PackIndexState
  = PackIndexStateV1
    { pisHandle :: Handle
    , pisTotalRecords :: Maybe Word32
    , pisFanOutTable :: Map Word8 Word32
    , pisRecordNos :: Map Sha1 Word32
    , pisV1RecordOffsets :: Map Sha1 Word32
    , pisPackFileSha1 :: Maybe Sha1
    , pisSha1 :: Maybe Sha1}
  | PackIndexStateV2
    { pisHandle :: Handle
    , pisTotalRecords :: Maybe Word32
    , pisFanOutTable :: Map Word8 Word32
    , pisRecordNos :: Map Sha1 Word32
    , pisRecordCrcs :: Map Sha1 Word32
    , pisV2RecordOffsets :: Map Sha1 Word64
    , pisPackFileSha1 :: Maybe Sha1
    , pisSha1 :: Maybe Sha1}

packIndexState :: Version -> Handle -> PackIndexState
packIndexState v h = case v of
  Version1 -> PackIndexStateV1
    h Nothing mempty mempty mempty Nothing Nothing
  Version2 -> PackIndexStateV2
    h Nothing mempty mempty mempty mempty Nothing Nothing

pisVersion :: PackIndexState -> Version
pisVersion PackIndexStateV1 {} = Version1
pisVersion PackIndexStateV2 {} = Version2

instance Show PackIndexState where
  show pis = let v = pisVersion pis in case pisTotalRecords pis of
    Nothing -> printf "<PackIndexState: %s>" (show v)
    Just totRecs -> printf "<PackIndexState: %s, records: %d>" (show v) totRecs

withPackIndex
  :: MonadIO m
  => FilePath -> StateT PackIndexState (ExceptT GitError m) r
  -> m (Either GitError r)
withPackIndex path m = runExceptT $ do
  h <- excTIO $ openBinaryFile path ReadMode
  v <- getPackIndexVersion h
  res <- evalStateT m (packIndexState v h)
  -- FIXME: this might be better bracketed in case of error, although the RTS
  -- should clean up the handle if it drops out of scope:
  excTIO $ hClose h
  return res

packIndexHeaderSize :: Version -> Integer
packIndexHeaderSize Version1 = 0
packIndexHeaderSize Version2 = 8

packIndexDataStart :: Version -> Integer
packIndexDataStart v = packIndexHeaderSize v + 256 * 4

getPackIndexVersion :: (MonadIO m, MonadError GitError m) => Handle -> m Version
getPackIndexVersion h = do
  excTIO $ hSeek h AbsoluteSeek 0
  (magic, rest) <- BS.splitAt 4 <$> excTIO (BS.hGet h 8)
  version <- bsToWord32 rest
  case magic of
    "\255tOc" -> if version == 2
      then return Version2
      else throwError UnsupportedPackIndexVersion
    _ -> return Version1

getPackIndexSha1
  :: (MonadIO m, MonadState PackIndexState m, MonadError GitError m) => m Sha1
getPackIndexSha1 = do
    pis <- get
    case pisSha1 pis of
      Just sha1 -> return sha1
      Nothing -> do
        sha1 <- readSha1 $ pisHandle pis
        put $ pis {pisSha1 = Just sha1}
        return sha1
  where
    readSha1 h = excTIO (
        hSeek h SeekFromEnd (-fromIntegral sha1Size)
        >> BS.hGet h (fromIntegral sha1Size)
      ) >>= bsToSha1

getPackSha1FromIndex
  :: (MonadIO m, MonadState PackIndexState m, MonadError GitError m) => m Sha1
getPackSha1FromIndex = do
    pis <- get
    case pisPackFileSha1 pis of
      Just sha1 -> return sha1
      Nothing -> do
        sha1 <- readPackFileSha1 $ pisHandle pis
        put $ pis {pisPackFileSha1 = Just sha1}
        return sha1
  where
    readPackFileSha1 h = excTIO (
        hSeek h SeekFromEnd (2 * (-fromIntegral sha1Size))
        >> BS.hGet h (fromIntegral sha1Size)
      ) >>= bsToSha1

excTIO :: (MonadIO m, MonadError GitError m) => IO a -> m a
excTIO io = liftIO (try io) >>= either (throwError . ErrorWithIO) return

parseOnly' :: MonadError GitError m => Parser a -> BS.ByteString -> m a
parseOnly' p = either (throwError . ParseError) return . parseOnly p

bsToWord32 :: MonadError GitError m => BS.ByteString -> m Word32
bsToWord32 = parseOnly' anyWord32be

hGetWord32 :: (MonadIO m, MonadError GitError m) => Handle -> m Word32
hGetWord32 h = excTIO (BS.hGet h 4) >>= bsToWord32

bsToWord64 :: MonadError GitError m => BS.ByteString -> m Word64
bsToWord64 = parseOnly' anyWord64be

hGetWord64 :: (MonadIO m, MonadError GitError m) => Handle -> m Word64
hGetWord64 h = excTIO (BS.hGet h 8) >>= bsToWord64

bsToSha1 :: MonadError GitError m => BS.ByteString -> m Sha1
bsToSha1 = liftEither . mapLeft ParseError . Sha1.fromByteString

-- | Examines the pack index file's fan out table to determine the minimum
--   record number for the given SHA1
getPackIndexMinRecordNo
  :: (MonadIO m, MonadState PackIndexState m, MonadError GitError m)
  => Sha1 -> m Word32
getPackIndexMinRecordNo sha1 = let fotIdx = BS.head $ unSha1 sha1 in do
    pis <- get
    let fot = pisFanOutTable pis
    case Map.lookup fotIdx fot of
      Just minRecordNo -> return minRecordNo
      Nothing -> let h = pisHandle pis in do
        excTIO $ hSeek h AbsoluteSeek $
          packIndexHeaderSize (pisVersion pis) + 4 * (fromIntegral fotIdx - 1)
        minRecordNo <- hGetWord32 h
        put $ pis {pisFanOutTable = Map.insert fotIdx minRecordNo fot}
        return minRecordNo

getPackIndexTotalRecords
  :: (MonadIO m, MonadState PackIndexState m, MonadError GitError m) => m Word32
getPackIndexTotalRecords = do
  pis <- get
  case pisTotalRecords pis of
    Just totRecs -> return totRecs
    Nothing -> let h = pisHandle pis in do
      excTIO $ hSeek h AbsoluteSeek $
        packIndexDataStart (pisVersion pis) - 4
      totRecs <- hGetWord32 h
      put $ pis {pisTotalRecords = Just totRecs}
      return totRecs

getPackIndexRecordNo
  :: (MonadIO m, MonadState PackIndexState m, MonadError GitError m)
  => Sha1 -> m Word32
getPackIndexRecordNo sha1 = do
  pis <- get
  case Map.lookup sha1 $ pisRecordNos pis of
    Just recordNo -> return recordNo
    Nothing -> case pisVersion pis of
      Version1 -> throwError UnsupportedPackIndexVersion
      Version2 -> let h = pisHandle pis in do
          minRecordNo <- getPackIndexMinRecordNo sha1
          totalRecords <- getPackIndexTotalRecords
          excTIO $ hSeek h AbsoluteSeek
            $ packIndexDataStart Version2
            + fromIntegral sha1Size * fromIntegral minRecordNo
          recordNo <- excTIO (LBS.hGetContents h) >>=
            findSha1Idx totalRecords minRecordNo
          put $ pis {pisRecordNos = Map.insert sha1 recordNo $ pisRecordNos pis}
          return recordNo
  where
    -- FIXME: this could be faster if implemented as a binary search, since the
    -- entries are ordered:
    findSha1Idx
      :: MonadError GitError m => Word32 -> Word32 -> LBS.ByteString -> m Word32
    findSha1Idx totRecs i lbs = let (first, rest) = LBS.splitAt 20 lbs in do
      sha1' <- bsToSha1 $ LBS.toStrict $ first
      case compare sha1' sha1 of
        LT -> if i + 1 >= totRecs
          then throwError Sha1NotInIndex -- End of table
          else findSha1Idx totRecs (i + 1) rest
        EQ -> return i
        GT -> throwError Sha1NotInIndex

getPackRecordCrc
  :: (MonadIO m, MonadState PackIndexState m, MonadError GitError m)
  => Sha1 -> m Word32
getPackRecordCrc sha1 = do
  pis <- get
  case pisVersion pis of
    Version1 -> throwError $ UnsupportedOperation
      "getPackRecordCrc: version 1 pack files do not contain crc"
    Version2 -> case Map.lookup sha1 $ pisRecordCrcs pis of
      Just crc -> return crc
      Nothing -> let h = pisHandle pis in do
        totRecs <- getPackIndexTotalRecords
        recordNo <- getPackIndexRecordNo sha1
        excTIO $ hSeek h AbsoluteSeek
          $ packIndexDataStart Version2
          + fromIntegral sha1Size * fromIntegral totRecs
          + 4 * fromIntegral recordNo
        crc <- hGetWord32 h
        put $ pis {pisRecordCrcs = Map.insert sha1 crc $ pisRecordCrcs pis}
        return crc

getPackRecordOffset
  :: (MonadIO m, MonadState PackIndexState m, MonadError GitError m)
  => Sha1 -> m Word64
getPackRecordOffset sha1 =
  do
    pis <- get
    case pisVersion pis of
      Version1 -> throwError UnsupportedPackIndexVersion
      Version2 -> case Map.lookup sha1 $ pisV2RecordOffsets pis of
        Just offset -> return offset
        Nothing -> do
          totRecs <- getPackIndexTotalRecords
          recordNo <- getPackIndexRecordNo sha1
          offset <- readOffset (pisHandle pis) totRecs recordNo
          put $ pis
            { pisV2RecordOffsets = Map.insert sha1 offset $
              pisV2RecordOffsets pis }
          return offset
  where
    readOffset h totRecs recordNo = do
      excTIO $ hSeek h AbsoluteSeek
        $ packIndexDataStart Version2
        + fromIntegral sha1Size * fromIntegral totRecs
        + 4 * (fromIntegral totRecs + fromIntegral recordNo)
      offset4Byte <- hGetWord32 h
      if offset4Byte .&. 2 ^ (31 :: Int) == 0
        -- MSB not set: just return first 31 bits as Word64 offset
        then return $ fromIntegral offset4Byte
        -- MSB is set: go read the large object table
        else let largeOffsetIdx = offset4Byte .&. (2 ^ (31 :: Int) - 1) in do
            excTIO $ hSeek h AbsoluteSeek
              $ packIndexDataStart Version2
              + fromIntegral sha1Size * fromIntegral totRecs
              + 8 * (fromIntegral totRecs + fromIntegral largeOffsetIdx)
            hGetWord64 h


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
