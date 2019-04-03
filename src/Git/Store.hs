{-# LANGUAGE
    FlexibleContexts
  , PolyKinds
#-}

module Git.Store where

import Codec.Compression.Zlib (compress, decompress)
import Control.Exception (try, throwIO)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Data.Tagged (Tagged(..))
import System.IO.Error (isDoesNotExistError)
import qualified System.Path as Path
import System.Path ((</>))
import System.Path.Directory (createDirectoryIfMissing)
import System.Path.IO (openBinaryFile, IOMode(..))

import Git.Objects
  (Object, GitObject(..), decodeObject, encodeLooseObject, decodeLooseObject)
import Git.Sha1 (Sha1)
import qualified Git.Sha1 as Sha1
import Git.Pack (withPackSet, packSetGetObjectData)
import Git.Repository (Repo, repoObjectsPath)


storeObject :: Repo -> Object t -> IO (Tagged t Sha1)
storeObject repo obj =
  let
    (sha1, encoded) = encodeLooseObject obj
    (sha1Head, filename) = splitSha1 sha1
    dirPath = repoObjectsPath repo </> sha1Head
  in do
    createDirectoryIfMissing True dirPath
    handle <- openBinaryFile (dirPath </> filename) WriteMode
    LBS.hPut handle $ compress $ LBS.fromStrict $ encoded
    return sha1

retrieveObject
  :: forall t. GitObject t => Repo -> Tagged t Sha1 -> IO (Object t)
retrieveObject repo sha1 = do
  eOrObj <- try readDirectly
  case eOrObj of
    Right obj -> return obj
    Left e -> if isDoesNotExistError e then readFromPack else throwIO e
  where
    storePath = repoObjectsPath repo
    (sha1Head, filename) = splitSha1 sha1
    readDirectly = openBinaryFile (storePath </> sha1Head </> filename) ReadMode
        >>= LBS.hGetContents >>= decodeLooseObject . decompress
    readFromPack :: IO (Object t)
    readFromPack = do
      (objTy, sbs) <- withPackSet (storePath </> Path.relDir "pack") $
        packSetGetObjectData $ unTagged sha1
      unless (objTy == goTy (Proxy @t)) $ error "Bad object type"
      decodeObject sbs


splitSha1 :: Tagged (a :: k) Sha1 -> (Path.RelDir, Path.RelFile)
splitSha1 sha1 = let (h, t) = splitAt 2 $ Sha1.toHexString $ unTagged sha1 in
  (Path.relDir h, Path.relFile t)
