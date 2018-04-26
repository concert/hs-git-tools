module Git.Objects.Blob where

import Data.Attoparsec.ByteString (takeByteString)
import qualified Data.ByteString as BS
import qualified System.Path as Path
import System.IO.MMap (mmapFileForeignPtr, Mode(..))
import Text.Printf (printf)

import Git.Internal (mmapData, MmapFrom(..), MmapTo(..))
import Git.Objects.GitObject (GitObject(..), ObjectType(..))


data Blob = Blob {blobData :: BS.ByteString}

instance Show Blob where
  show = printf "<blob: %d>" . BS.length . blobData

instance GitObject Blob where
  gitObjectType _ = ObjTyBlob
  encodeObject = blobData
  objectParser size = Blob . BS.take (fromIntegral size) <$> takeByteString

-- FIXME: this doesn't report trying to open the wrong type of file very well...
fromFile :: Path.AbsFile -> IO Blob
fromFile p = do
  h <- mmapFileForeignPtr (Path.toString p) ReadOnly Nothing
  return $ Blob $ mmapData h (FromStart 0) ToEnd
