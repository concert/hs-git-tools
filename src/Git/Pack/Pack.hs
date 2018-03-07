{-# LANGUAGE BinaryLiterals #-}

module Git.Pack.Pack where

import Prelude hiding (fail)

import Codec.Compression.Zlib (decompress)
import Control.Monad (unless)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Attoparsec.ByteString
  (parseOnly, parseWith, eitherResult, string, (<?>), anyWord8)
import Data.Attoparsec.Binary (anyWord32be)
import Data.Bits ((.&.), (.|.), shift)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import System.IO
  (Handle, IOMode(..), openBinaryFile, hSeek, SeekMode(..), hTell)

import Git.Types (Sha1, sha1Size)
import qualified Git.Types.Sha1 as Sha1
import Git.Types.SizedByteString (SizedByteString)
import qualified Git.Types.SizedByteString as SBS

import Debug.Trace

data PackObjectType
  = PotCommit | PotTree | PotBlob | PotTag
  | PotOfsDelta | PotRefDelta deriving (Show, Eq)

data PackHandle = PackHandle
  { phHandle :: Handle
  , phNumObjects :: Word32
  } deriving (Show)

openPackFile :: (MonadIO m, MonadFail m) => FilePath -> m PackHandle
openPackFile path = do
    h <- liftIO $ openBinaryFile path ReadMode
    (version, numObjects) <- (liftIO $ BS.hGet h 12) >>=
      either fail return . parseOnly headerP
    unless (version == 2) $ fail "openPackFile: unsupported version"
    return $ PackHandle h numObjects
  where
    headerP = do
      _ <- string "PACK" <?> "Magic PACK header"
      version <- anyWord32be
      numObjects <- anyWord32be
      return (version, numObjects)

getPackSha1FromPack :: (MonadIO m, MonadFail m) => PackHandle -> m Sha1
getPackSha1FromPack ph = let h = phHandle ph in do
    liftIO $ hSeek h SeekFromEnd (-fromIntegral sha1Size)
      >> BS.hGet h (fromIntegral sha1Size)
      >>= Sha1.fromByteString

decodePackObjecType :: MonadFail m => Word8 -> m PackObjectType
decodePackObjecType w = case w of
  1 -> return PotCommit
  2 -> return PotTree
  3 -> return PotBlob
  4 -> return PotTag
  6 -> return PotOfsDelta
  7 -> return PotRefDelta
  _ -> traceShow w $ fail "Unrecognised object type"

getPackObjectInfo
  :: (MonadIO m, MonadFail m) => PackHandle -> Word64
  -> m (PackObjectType, Integer, Integer)
getPackObjectInfo ph offset =
  let
    h = phHandle ph
    getByte = liftIO $ BS.hGet h 1
  in do
    liftIO $ hSeek h AbsoluteSeek $ fromIntegral offset
    -- FIXME: don't think reading from the file a byte at a time is the smartest
    -- move, but perhaps the kernel or RTS does something clever?
    (ty, size) <- getByte >>= parseWith getByte objHeadP >>=
         either fail return . eitherResult
    dataStart <- liftIO $ hTell h
    return (ty, dataStart, size)
  where
    objHeadP = do
      byte1 <- anyWord8
      let msb = byte1 .&. 0b10000000
      ty <- decodePackObjecType $ shift (byte1 .&. 0b01110000) (-4)
      let lenHead = fromIntegral $ byte1 .&. 0b00001111
      if msb == 0
        then return (ty, lenHead)
        else lengthChunkP lenHead 4 >>= return . (ty,)
    lengthChunkP lenHead i = do
      byte <- anyWord8
      let msb = byte .&. 0b10000000
      let lenHead' = lenHead .|. shift (fromIntegral $ byte .&. 0b01111111) i
      if msb == 0
        then return lenHead'
        else lengthChunkP lenHead' $ i + 7

-- | Uses LBS.hGetContents, so requires that PackHandle is no longer used for
--   anything else.
getObjectDataFromPack
  :: MonadIO m => PackHandle -> Integer -> Integer -> m SizedByteString
getObjectDataFromPack ph offset len = let h = phHandle ph in do
  liftIO $ hSeek h AbsoluteSeek offset
  SBS.takeFromLazyByteString len . decompress <$> liftIO (LBS.hGetContents h)
