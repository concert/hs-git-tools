module Git.Store where

import Codec.Compression.Zlib (compress, decompress)
import qualified Data.ByteString.Lazy as LBS
import System.Directory (renameFile, createDirectoryIfMissing)
import System.FilePath.Posix ((</>))
import System.IO (openTempFile, openBinaryFile, hClose, IOMode(..))

import Git.Serialise (GitObject(..))
import Git.Types.Sha1 (Sha1, hashLazy, toHexString)
import qualified Git.Types.SizedByteString as SBS

storeObject :: GitObject a => FilePath -> a -> IO Sha1
storeObject storePath obj = let encoded = encodeObject obj in do
  (tmpPath, handle) <- openTempFile storePath "compressed_encoded_object"
  LBS.hPut handle $ compress $ SBS.toLazyByteString $ encoded
  hClose handle
  -- FIXME: It might be good to try to parallelise the hashing with the file
  -- writing:
  let sha1 = hashLazy $ SBS.toLazyByteString encoded
  let (sha1Head, filename) = splitAt 2 $ toHexString $ sha1
  let dirPath = storePath </> sha1Head
  createDirectoryIfMissing True dirPath
  renameFile tmpPath $ dirPath </> filename
  return sha1

-- We only let you get access to the retrieved objects in a context, because we
-- have to have a hook for cleaning up the temporary uncompressed files.
retrieveObject :: GitObject a => FilePath -> Sha1 -> IO a
retrieveObject storePath sha1 =
  let (sha1Head, filename) = splitAt 2 $ toHexString sha1 in
  openBinaryFile (storePath </> sha1Head </> filename) ReadMode
  >>= LBS.hGetContents >>= decodeObject . decompress
