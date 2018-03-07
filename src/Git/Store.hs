{-# LANGUAGE FlexibleContexts #-}

module Git.Store where

import Codec.Compression.Zlib (compress, decompress)
import Control.Exception (try, throwIO)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except (runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Attoparsec.ByteString (endOfInput)
import qualified Data.ByteString.Lazy as LBS
import Data.List (isSuffixOf)
import Data.Tagged (Tagged(..))
import Data.Word
import System.Directory (renameFile, createDirectoryIfMissing, listDirectory)
import System.FilePath.Posix ((</>))
import System.IO (openTempFile, openBinaryFile, hClose, IOMode(..))
import System.IO.Error (isDoesNotExistError)

import Git.Serialise (GitObject(..), lazyParseOnly)
import Git.Types.Internal (firstSuccess, replaceSuffix)
import Git.Types.Sha1 (Sha1, hashLazy, toHexString)
import qualified Git.Types.SizedByteString as SBS
import Git.Pack
  ( withPackIndex, getPackRecordOffset, openPackFile, getPackObjectInfo
  , getObjectDataFromPack)

storeObject :: GitObject a => FilePath -> a -> IO (Tagged a Sha1)
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
  return $ Tagged sha1

retrieveObject :: forall a. GitObject a => FilePath -> Tagged a Sha1 -> IO a
retrieveObject storePath sha1 = do
  eOrObj <- try readDirectly
  case eOrObj of
    Right obj -> return obj
    Left e -> if isDoesNotExistError e then readFromPack else throwIO e
  where
    (sha1Head, filename) = splitAt 2 $ toHexString $ unTagged sha1
    readDirectly = openBinaryFile (storePath </> sha1Head </> filename) ReadMode
        >>= LBS.hGetContents >>= decodeObject . decompress
    readFromPack :: IO a
    readFromPack = fmap (either error id) $ runExceptT $ do
        files <- liftIO $ listDirectory $ storePath </> "pack"
        let indexNames = filter (".idx" `isSuffixOf`) files
        (indexName, packOffset) <- withExceptT (snd . head) $ firstSuccess
          (searchPackIndex . (storePath </>) . ("pack" </>)) indexNames
        packName <- replaceSuffix "idx" "pack" indexName
        -- FIXME: this currently leaks too many details of how to look at pack
        -- files...
        ph <- openPackFile (storePath </> "pack" </> packName)
        (ty, offset, size) <- getPackObjectInfo ph packOffset
        -- FIXME: this DOES NOT CHECK THE OBJECT TYPE!
        getObjectDataFromPack ph offset size >>=
          lazyParseOnly (objectParser size <* endOfInput) . SBS.toLazyByteString
    searchPackIndex :: (MonadIO m, MonadError String m) => FilePath -> m Word64
    searchPackIndex path =
      withPackIndex path (getPackRecordOffset $ unTagged sha1) >>=
      either (throwError . show) return
