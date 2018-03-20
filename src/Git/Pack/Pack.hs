{-# LANGUAGE
    BinaryLiterals
  , FlexibleContexts
#-}

module Git.Pack.Pack where

import Prelude hiding (fail, take)

import Codec.Compression.Zlib (decompress)
import Control.Monad (unless)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Attoparsec.ByteString
  ( Parser, parseOnly, string, (<?>), anyWord8, take, many1, peekWord8'
  , takeLazyByteString)
import Data.Attoparsec.Binary (anyWord32be)
import Data.Bits (Bits, (.&.), (.|.), shift, shiftL, testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import Data.Word
import System.IO.MMap (mmapFileForeignPtr, Mode(..))
import Text.Printf (printf)

import Git.Serialise (tellParsePos)
import Git.Types (Sha1, sha1Size, GitError(..), ObjectType(..))
import Git.Types.Internal
  (MmapHandle, mmapData, MmapFrom(..), MmapTo(..), mmapSha1)
import qualified Git.Types.Sha1 as Sha1
import Git.Types.SizedByteString (SizedByteString)
import qualified Git.Types.SizedByteString as SBS

import Git.Pack.Delta (DeltaInstruction(..), DeltaBody(..))

import Debug.Trace

data PackObjectType
  = PoTyCommit | PoTyTree | PoTyBlob | PoTyTag
  | PoTyOfsDelta | PoTyRefDelta deriving (Show, Eq)

packObjTyToObjTy :: MonadError GitError m => PackObjectType -> m ObjectType
packObjTyToObjTy poc = case poc of
  PoTyCommit -> return $ ObjTyCommit
  PoTyTree -> return $ ObjTyTree
  PoTyBlob -> return $ ObjTyBlob
  PoTyTag -> return $ ObjTyTag
  _ -> throwError $ GenericError "Bad pack object type"

data RefDelta = RefDelta {rdBaseSha1 :: Sha1, rdBody :: DeltaBody}
instance Show RefDelta where
  show (RefDelta sha1 _) = printf "<RefDelta: %s>" (show sha1)

data OfsDelta = OfsDelta {odBaseNegOfs :: Word64, odBody :: DeltaBody}
instance Show OfsDelta where
  show (OfsDelta negOfs _) = printf "<OfsDelta: %d" negOfs

data DeltaObject = RefDelta' RefDelta | OfsDelta' OfsDelta deriving (Show)

data PackHandle = PackHandle
  { phMmap :: MmapHandle
  , phNumObjects :: Word32
  } deriving (Show)

openPackFile :: (MonadIO m, MonadError GitError m) => FilePath -> m PackHandle
openPackFile path = do
    h <- liftIO $ mmapFileForeignPtr path ReadOnly Nothing
    (version, numObjects) <- either (throwError . ParseError) return $
        parseOnly headerP $ mmapData h (FromStart 0) (Length 12)
    unless (version == 2) $ throwError UnsupportedPackFileVersion
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
  1 -> return PoTyCommit
  2 -> return PoTyTree
  3 -> return PoTyBlob
  4 -> return PoTyTag
  6 -> return PoTyOfsDelta
  7 -> return PoTyRefDelta
  _ -> fail $ "Unrecognised object type " ++ show w

msb :: Word8 -> Bool
msb byte = byte .&. 0b10000000 /= 0

lsbs :: Word8 -> Word8
lsbs = (.&. 0b01111111)


-- | Gets the type of packed object, the start offset of the actual data blob
--   (i.e. the position in the file after we've read the object header) and the
--   size of the _uncompressed_ data for the blob.
getPackObjectInfo
  :: MonadError GitError m
  => PackHandle -> Word64 -> m (PackObjectType, Word64, Word64)
getPackObjectInfo ph offset = do
  (objTy, bytesConsumed, objSize) <-
    either (throwError . ParseError) return $
        parseOnly objHeadP $
        -- It's ok to make a long bytestring from the mmapped file, because it
        -- won't actually be read by the kernel until we need it, and we'll stop
        -- parsing after the header is done:
        mmapData (phMmap ph) (FromStart $ fromIntegral offset) ToEnd
  return (objTy, bytesConsumed + offset, objSize)

objHeadP :: Parser (PackObjectType, Word64, Word64)
objHeadP = do
  byte1 <- anyWord8
  ty <- decodePackObjectType $ shift (byte1 .&. 0b01110000) (-4)
  let lenHead = fromIntegral $ byte1 .&. 0b00001111
  len <- if msb byte1
    then lengthP lenHead
    else return lenHead
  pos <- tellParsePos
  return (ty, fromIntegral pos, len)

lengthP :: Word64 -> Parser Word64
lengthP lenHead = do
  byte <- anyWord8
  let lenHead' = shift lenHead 7 .|. (fromIntegral $ lsbs byte)
  if msb byte
    then lengthP lenHead'
    else return lenHead'

deltaInsP :: Parser DeltaInstruction
deltaInsP = do
    byte1 <- peekWord8'
    if msb byte1
      then copyP
      else insertP
  where
    -- | See the internals of Data.Attorparsec.Binary
    packBytes :: BS.ByteString -> Word32
    packBytes = BS.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0

    -- | Copy instruction offset/length are stored as compressed little-endian
    --   32 bit integers. The "cursor advance" mask, read from the copy
    --   instruction itself, indicates whether to simply pad a byte with zeros
    --   or to read an actual byte of data for the result.
    --
    --   E.g. if the last four bits of the copy instruction are `1010` and the
    --   next two bytes are `11010111` `01001011`, the resulting 32 bit integer
    --   is `11010111 00000000 01001011 00000000`
    --
    --   See https://codewords.recurse.com/issues/three/unpacking-git-packfiles
    leCompressedP :: Word8 -> Int -> Parser Word32
    leCompressedP curAdvMask maskLen =
      fmap (packBytes . BS.pack . Prelude.reverse) $
      sequence $ fmap (\b -> if b then anyWord8 else return 0) $
      takeBits maskLen curAdvMask

    takeBits :: Bits a => Int -> a -> [Bool]
    takeBits n bs = List.take n $ fmap (bs `testBit`) [0..]
    copyP = do
      byte <- anyWord8
      let ofsCursAdvances = byte .&. 0b00001111
      let lenCursAdvances = byte .&. 0b01110000
      ofs <- leCompressedP ofsCursAdvances 4
      len <- leCompressedP lenCursAdvances 3
      return $ Copy ofs len
    insertP = do
      len <- lsbs <$> anyWord8
      dat <- SBS.takeFromLazyByteString (fromIntegral len) <$>
        takeLazyByteString
      return $ Insert dat

deltaBodyP :: Parser DeltaBody
deltaBodyP = do
  sourceLen <- lengthP 0
  targetLen <- lengthP 0
  inss <- many1 deltaInsP
  return $ DeltaBody sourceLen targetLen inss

refDeltaP :: Parser RefDelta
refDeltaP = do
  sha1 <- take 20 >>= Sha1.fromByteString
  db <- deltaBodyP
  return $ RefDelta sha1 db

ofsDeltaP :: Parser OfsDelta
ofsDeltaP = do
  negOfs <- lengthP 0
  db <- deltaBodyP
  return $ OfsDelta negOfs db

getPackObjectData
  :: MonadError GitError m
  => PackHandle -> Word64
  -> m (Either (ObjectType, SizedByteString) DeltaObject)
getPackObjectData ph offset = do
    (packObjTy, dataStart, inflatedSize) <- getPackObjectInfo ph offset
    let objectData = SBS.takeFromLazyByteString (fromIntegral inflatedSize) $
          decompress $
          LBS.fromStrict $ mmapData (phMmap ph)
            (FromStart $ fromIntegral dataStart)
            (Length $ fromIntegral inflatedSize)
    case packObjTy of
      PoTyRefDelta -> Right . RefDelta' <$> doParse refDeltaP objectData
      PoTyOfsDelta -> Right . OfsDelta' <$> doParse ofsDeltaP objectData
      _ -> Left . (,objectData) <$> packObjTyToObjTy packObjTy
  where
    doParse p = either (throwError . ParseError) return . parseOnly p .
      LBS.toStrict . SBS.toLazyByteString
