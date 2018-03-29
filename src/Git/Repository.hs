module Git.Repository
  ( Repo, repoFilePath, openRepo
  , repoGitPath, repoConfigPath, repoObjectsPath, repoRefsPath, repoHeadPath
  , RepoError(..)
  ) where

import Control.Exception (try, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT, throwError)
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))
import System.IO.Error (isDoesNotExistError)

newtype Repo = Repo {repoFilePath :: FilePath} deriving (Show)

repoGitPath, repoConfigPath, repoObjectsPath, repoRefsPath, repoHeadPath
  :: Repo -> FilePath
repoGitPath repo = repoFilePath repo </> ".git"
repoConfigPath repo = repoGitPath repo </> "config"
repoObjectsPath repo = repoGitPath repo </> "objects"
repoRefsPath repo = repoGitPath repo </> "refs"
repoHeadPath repo = repoGitPath repo </> "HEAD"

data RepoError = NotAGitRepo FilePath deriving Show

openRepo :: FilePath -> ExceptT RepoError IO Repo
openRepo path = liftIO (try $ listDirectory $ path </> ".git") >>=
    either filterIOError guardListing
  where
    die = throwError $ NotAGitRepo path
    filterIOError e = if isDoesNotExistError e then die else liftIO $ throwIO e
    guardListing listing =
      if all (`elem` listing) ["config", "HEAD", "objects", "refs"]
      then return $ Repo path else die
