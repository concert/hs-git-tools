{-# LANGUAGE FlexibleContexts #-}

module Git.Store where

import Codec.Compression.Zlib (compress, decompress)
import Control.Exception (try, throwIO)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Data.Tagged (Tagged(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))
import System.IO (openBinaryFile, IOMode(..))
import System.IO.Error (isDoesNotExistError)

import Git.Internal (Wrapable(..))
import Git.Objects
  (GitObject(..), Object, decodeObject, encodeLooseObject, decodeLooseObject)
import Git.Sha1 (Sha1)
import qualified Git.Sha1 as Sha1
import Git.Pack (withPackSet, packSetGetObjectData)
import Git.Repository (Repo, repoObjectsPath)


storeObject :: GitObject a => Repo -> a -> IO (Tagged a Sha1)
storeObject repo obj =
  let
    (sha1, encoded) = encodeLooseObject obj
    (sha1Head, filename) = splitAt 2 $ Sha1.toHexString $ unTagged sha1
    dirPath = repoObjectsPath repo </> sha1Head
  in do
    createDirectoryIfMissing True dirPath
    handle <- openBinaryFile (dirPath </> filename) WriteMode
    LBS.hPut handle $ compress $ LBS.fromStrict $ encoded
    return sha1

retrieveObject
  :: forall a. (Wrapable Object a, GitObject a) => Repo -> Tagged a Sha1 -> IO a
retrieveObject repo sha1 = do
  eOrObj <- try $ readDirectly >>= unwrap
  case eOrObj of
    Right obj -> return obj
    Left e -> if isDoesNotExistError e then readFromPack else throwIO e
  where
    storePath = repoObjectsPath repo
    (sha1Head, filename) = splitAt 2 $ Sha1.toHexString $ unTagged sha1
    readDirectly = openBinaryFile (storePath </> sha1Head </> filename) ReadMode
        >>= LBS.hGetContents >>= decodeLooseObject . decompress
    readFromPack :: IO a
    readFromPack = do
      (objTy, sbs) <- withPackSet (storePath </> "pack") $
        packSetGetObjectData $ unTagged sha1
      unless (objTy == gitObjectType (Proxy @a)) $ error "Bad object type"
      decodeObject sbs
