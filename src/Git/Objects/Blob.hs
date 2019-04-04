{-# LANGUAGE
    DataKinds
#-}

module Git.Objects.Blob where

import Data.Attoparsec.ByteString (Parser, takeByteString)
import qualified Data.ByteString as BS
import Data.Word
import qualified System.Path as Path
import System.IO.MMap (mmapFileForeignPtr, Mode(..))

import Git.Internal (mmapData, MmapFrom(..), MmapTo(..))
import Git.Objects.Internal (Blob, Object(..))


encodeBlob :: Blob -> BS.ByteString
encodeBlob = blobData

blobParser :: Word64 -> Parser Blob
blobParser size = Blob . BS.take (fromIntegral size) <$> takeByteString

-- FIXME: this doesn't report trying to open the wrong type of file very well...
fromFile :: Path.AbsFile -> IO Blob
fromFile p = do
  h <- mmapFileForeignPtr (Path.toString p) ReadOnly Nothing
  return $ Blob $ mmapData h (FromStart 0) ToEnd
