{-# LANGUAGE BinaryLiterals #-}

module Git.Pack.Pack where

import Prelude hiding (fail)

import Codec.Compression.Zlib (decompress)
import Control.Monad (unless)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Attoparsec.ByteString (Parser, parseOnly, string, (<?>), anyWord8)
import Data.Attoparsec.Binary (anyWord32be)
import Data.Bits ((.&.), (.|.), shift)
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import System.IO.MMap (mmapFileForeignPtr, Mode(..))

import Git.Serialise (tellParsePos)
import Git.Types (Sha1, sha1Size)
import Git.Types.Internal
  (MmapHandle, mmapData, MmapFrom(..), MmapTo(..), mmapSha1)
import Git.Types.SizedByteString (SizedByteString)
import qualified Git.Types.SizedByteString as SBS

import Debug.Trace

data PackObjectType
  = PotCommit | PotTree | PotBlob | PotTag
  | PotOfsDelta | PotRefDelta deriving (Show, Eq)

data PackHandle = PackHandle
  { phMmap :: MmapHandle
  , phNumObjects :: Word32
  } deriving (Show)

openPackFile :: (MonadIO m, MonadFail m) => FilePath -> m PackHandle
openPackFile path = do
    h <- liftIO $ mmapFileForeignPtr path ReadOnly Nothing
    (version, numObjects) <- either fail return $ parseOnly headerP $
        mmapData h (FromStart 0) (Length 12)
    unless (version == 2) $ fail "openPackFile: unsupported version"
    return $ PackHandle h numObjects
  where
    headerP = do
      _ <- string "PACK" <?> "Magic PACK header"
      version <- anyWord32be
      numObjects <- anyWord32be
      return (version, numObjects)

getPackSha1FromPack :: PackHandle -> Sha1
getPackSha1FromPack ph = mmapSha1 (phMmap ph) $ FromEnd (-sha1Size)

decodePackObjectType :: MonadFail m => Word8 -> m PackObjectType
decodePackObjectType w = case w of
  1 -> return PotCommit
  2 -> return PotTree
  3 -> return PotBlob
  4 -> return PotTag
  6 -> return PotOfsDelta
  7 -> return PotRefDelta
  _ -> traceShow w $ fail "Unrecognised object type"

-- | Gets the type of packed object, the start offset of the actual data blob
--   (i.e. the position in the file after we've read the object header) and the
--   size of the _uncompressed_ data for the blob.
getPackObjectInfo
  :: MonadFail m => PackHandle -> Word64 -> m (PackObjectType, Word64, Word64)
getPackObjectInfo ph offset = either fail return $ parseOnly objHeadP $
    -- It's ok to make a long bytestring from the mmapped file, because it won't
    -- actually be read by the kernel until we need it, and we'll stop parsing
    -- after the header is done:
    mmapData (phMmap ph) (FromStart $ fromIntegral offset) ToEnd
  where
    objHeadP = do
      byte1 <- anyWord8
      let msb = byte1 .&. 0b10000000
      ty <- decodePackObjectType $ shift (byte1 .&. 0b01110000) (-4)
      let lenHead = fromIntegral $ byte1 .&. 0b00001111
      if msb == 0
        then return (ty, offset + 4, lenHead)
        else do
          (o, l) <- lengthChunkP lenHead 4
          return $ (ty, offset + fromIntegral o, l)
    lengthChunkP :: Word64 -> Int -> Parser (Int, Word64)
    lengthChunkP lenHead i = do
      byte <- anyWord8
      let msb = byte .&. 0b10000000
      let lenHead' = lenHead .|. shift (fromIntegral $ byte .&. 0b01111111) i
      pos <- tellParsePos
      if msb == 0
        then return (pos, lenHead')
        else lengthChunkP lenHead' $ i + 7

-- | Uses LBS.hGetContents, so requires that PackHandle is no longer used for
--   anything else.
getPackObjectData :: PackHandle -> Word64 -> Word64 -> SizedByteString
getPackObjectData ph offset len =
  SBS.takeFromLazyByteString (fromIntegral len) $ decompress $
  LBS.fromStrict $ mmapData (phMmap ph)
  (FromStart $ fromIntegral offset) (Length $ fromIntegral len)
