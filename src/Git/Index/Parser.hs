{-# LANGUAGE
    BinaryLiterals
  , FlexibleContexts
#-}

module Git.Index.Parser where

import Control.Applicative ((<|>))
import Control.Monad (unless, when, replicateM_)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Attoparsec.Binary (anyWord16be, anyWord32be)
import Data.Attoparsec.ByteString
  (Parser, (<?>), string, satisfy, takeTill, many', endOfInput)
import Data.Bits ((.&.), shiftR, testBit)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (foldl')
import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (POSIXTime, systemToPOSIXTime)
import Data.Time.Clock.System (SystemTime(..))
import Data.Word
import qualified System.Path as Path
import System.Path.IO (openBinaryFile, IOMode(..))
import System.Posix.Types (CDev(..), CIno(..), CUid(..), CGid(..))

import Git.Pack (chunkNumBeP)
import Git.Internal
  (lazyParseOnly, nullTermStringP, tellParsePos, lowMask)
import Git.Sha1 (sha1ByteStringParser)
import Git.Types (FileMode, fileModeFromInt, GitError(..), checkPath)
import Git.Index.Extensions (extensionP, CachedTree(..), ResolveUndo(..))
import Git.Index.Index (Index(..))

import Git.Index.Types
  ( IndexVersion(..), versionFromWord32
  , GitFileStat(..), IndexEntry(..), IndexEntries
  , Flag(..), Stage, intToStage, mapToStages)


openIndex' :: (MonadIO m, MonadError GitError m) => FilePath -> m Index
openIndex' path = p path >>= openIndex
  where
    p = either (throwError . ParseError) return . Path.parse

openIndex :: (MonadIO m, MonadError GitError m) => Path.AbsFile -> m Index
openIndex path = do
  content <- liftIO $ openBinaryFile path ReadMode >>=
    LBS.hGetContents
  either (throwError . ParseError) return $
    lazyParseOnly (indexP <* endOfInput) content

indexP :: Parser Index
indexP = do
  (version, numEntries) <- headerP
  entries <- indexEntriesP version numEntries
  (ct, ru) <- extensionsP
  _ <- sha1ByteStringParser
  return $ Index version entries ct ru

headerP :: Parser (IndexVersion, Word32)
headerP = do
  _ <- string "DIRC" <?> "Magic DIRCache header"
  version <- anyWord32be >>= versionFromWord32
  numEntries <- anyWord32be
  return (version, numEntries)

indexEntriesP :: IndexVersion -> Word32 -> Parser IndexEntries
indexEntriesP version numEntries =
    go (Path.rel "") numEntries >>= mapM mapToStages . momFromList
  where
    go _ 0 = return []
    go prevPath ne = do
      entryData@((path, _), _) <- entryP version prevPath
      (entryData:) <$> go path (ne - 1)

entryP
  :: IndexVersion -> Path.RelFileDir
  -> Parser ((Path.RelFileDir, Stage), IndexEntry)
entryP version prevPath = do
    startPos <- tellParsePos
    gfs <- gfsP
    sha1 <- sha1ByteStringParser
    (stage, flags) <- flagsP version
    path <- case version of
          Version4 -> v4PathP prevPath
          _ -> v2_3PathP <* padding startPos
    checkPath path
    return ((path, stage), IndexEntry gfs sha1 flags)
  where
    padding startPos = do
      pos <- tellParsePos
      let requiredPadding = 8 - ((pos - startPos) `rem` 8)
      replicateM_ requiredPadding (satisfy (== 0) <?> "nul padding")

gfsP :: Parser GitFileStat
gfsP = do
  ctime <- posixTimeP
  mtime <- posixTimeP
  devId <- CDev . fromIntegral <$> anyWord32be
  inodeNo <- CIno . fromIntegral <$> anyWord32be
  mode <- fileModeP
  uid <- CUid <$> anyWord32be
  gid <- CGid <$> anyWord32be
  size <- anyWord32be
  return $ GitFileStat ctime mtime devId inodeNo mode uid gid size

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
    stage <- intToStage $ fromIntegral $ shiftR bits 12 .&. 0b11
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

v2_3PathP :: Parser Path.RelFileDir
v2_3PathP = Path.rel . Char8.unpack <$> takeTill (== 0)

v4PathP :: Path.RelFileDir -> Parser Path.RelFileDir
v4PathP prevPath = let prevPathT = Text.pack $ Path.toString prevPath in do
  cut <- fromIntegral <$> chunkNumBeP
  new <- nullTermStringP
  return $ Path.rel $ Text.unpack $ Text.dropEnd cut prevPathT <> Text.pack new

momFromList :: (Ord k1, Ord k2) => [((k1, k2), v)] -> Map k1 (Map k2 v)
momFromList = foldl' f mempty
  where
    f acc ((k1, k2), v) = Map.alter (overK2M k2 v) k1 acc
    overK2M :: Ord k2 => k2 -> v -> Maybe (Map k2 v) -> Maybe (Map k2 v)
    overK2M k2 v = Just . Map.insert k2 v . maybe mempty id

data Extension
  = NoExt
  | ExtCachedTree CachedTree
  | ExtResolveUndo ResolveUndo
  deriving (Show)

singleExtP :: Parser Extension
singleExtP =
      (ExtCachedTree <$> extensionP)
  <|> (ExtResolveUndo <$> extensionP)
  <|> (const NoExt <$> extensionP @())

extensionsP :: Parser (CachedTree, ResolveUndo)
extensionsP =
    foldl' f (CachedTree mempty, ResolveUndo mempty) <$> many' singleExtP
  where
    f x NoExt = x
    f (_, ru) (ExtCachedTree ct) = (ct, ru)
    f (ct, _) (ExtResolveUndo ru) = (ct, ru)
