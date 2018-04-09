{-# LANGUAGE
    BinaryLiterals
  , FlexibleContexts
#-}

module Git.Index where

import Control.Monad (unless, when)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Attoparsec.ByteString
  (Parser, (<?>), string, endOfInput, many1, satisfy, takeTill)
import Data.Attoparsec.Binary (anyWord16be, anyWord32be)
import Data.Bits (Bits, (.&.), shiftR, testBit)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (POSIXTime, systemToPOSIXTime)
import Data.Time.Clock.System (SystemTime(..))
import Data.Word
import System.IO (openBinaryFile, IOMode(..))
import System.Posix.Types
  ( DeviceID, CDev(..), FileID, CIno(..), FileOffset
  , UserID, CUid(..), GroupID, CGid(..))

import Git.Pack (chunkNumBeP)
import Git.Serialise (lazyParseOnly, sha1ByteStringP, nullTermStringP)
import Git.Types (FileMode, fileModeFromInt, Sha1, GitError(..))
import Git.Types.Internal ()


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
  , ieSize :: Word32  -- FileOffset truncated to 32-bit
  , ieSha1 :: Sha1
  , ieFlags :: Set Flag
  } deriving (Show, Eq)

openIndex :: (MonadIO m, MonadError GitError m) => FilePath -> m Index
openIndex path = do
  h <- liftIO $ openBinaryFile path ReadMode
  (headBytes, content) <- liftIO $ LBS.splitAt 12 <$> LBS.hGetContents h
  (versionNo, numEntries) <- either (throwError . ParseError) return $
    lazyParseOnly (headerP <* endOfInput) headBytes
  version <- versionFromWord32 versionNo
  entries <- either (throwError . ParseError) return $
    lazyParseOnly (indexEntriesP version numEntries) content
  return $ Index version entries

headerP :: Parser (Word32, Word32)
headerP = do
  _ <- string "DIRC" <?> "Magic DIRCache header"
  version <- anyWord32be
  numEntries <- anyWord32be
  return (version, numEntries)

indexEntriesP :: IndexVersion -> Word32 -> Parser IndexEntries
indexEntriesP version = fmap Map.fromList . go ""
  where
    go _ 0 = return []
    go prevPath numEntries = do
      entryData@((path, _), _) <- entryP version prevPath
      (entryData:) <$> go path (numEntries - 1)

entryP
  :: IndexVersion -> FilePathText -> Parser ((FilePathText, Stage), IndexEntry)
entryP version prevPath = do
  ctime <- posixTimeP
  mtime <- posixTimeP
  devId <- CDev . fromIntegral <$> anyWord32be
  inodeNo <- CIno . fromIntegral <$> anyWord32be
  mode <- fileModeP
  uid <- CUid <$> anyWord32be
  gid <- CGid <$> anyWord32be
  size <- anyWord32be
  sha1 <- sha1ByteStringP
  (stage, flags) <- flagsP version
  path <- case version of
        Version4 -> v4PathP prevPath
        _ -> v1_3PathP
  return
    ( (path, stage)
    , IndexEntry ctime mtime devId inodeNo mode uid gid size sha1 flags)

posixTimeP :: Parser POSIXTime
posixTimeP = do
  seconds <- fromIntegral <$> anyWord32be
  nanofrac <- anyWord32be
  return $ systemToPOSIXTime $ MkSystemTime seconds nanofrac

fileModeP :: Parser FileMode
fileModeP = anyWord32be >>= fileModeFromInt . fromIntegral

flagsP :: IndexVersion -> Parser (Stage, Set Flag)
flagsP version = do
    bits <- anyWord16be
    let assumeValid = flag AssumeValid $ testBit bits 15
    let continue = testBit bits 14
    when (continue && version < Version3) $
      fail $ "Extended flags bit unexpectedly set in " ++ show version
    let stage = fromIntegral $ shiftR bits 12 .&. 0b11
    -- The name length info only appears to be useful for memory allocation
    -- purposes:
    -- let nameLen = lowMask bits 12  -- Version < 4
    if continue
      then (stage,) . (assumeValid <>) <$> extendedP
      else return (stage, assumeValid)
  where
    flag f b = if b then Set.singleton f else mempty
    extendedP = do
      bits <- anyWord16be
      unless (lowMask bits 13 == 0) $
        fail "Unexpected value in unused extended flags"
      return $
           (flag SkipWorkTree $ testBit bits 14)
        <> (flag IntentToAdd $ testBit bits 13)

v1_3PathP :: Parser FilePathText
v1_3PathP = fmap decodeUtf8 $ takeTill (== 0) <* many1 (satisfy (== 0))

v4PathP :: FilePathText -> Parser FilePathText
v4PathP prevPath = do
  cut <- fromIntegral <$> chunkNumBeP
  new <- nullTermStringP
  return $ Text.dropEnd cut prevPath <> new

lowMask :: (Bits a, Num a) => a -> Int -> a
lowMask bits n = bits .&. 2 ^ n - 1
