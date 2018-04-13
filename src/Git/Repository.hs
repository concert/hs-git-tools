module Git.Repository
  ( Repo, repoFilePath, openRepo
  , repoGitPath, repoConfigPath, repoObjectsPath, repoRefsPath, repoHeadPath
  , RepoError(..)
  ) where

import Control.Exception (try, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT, throwError)
import System.IO.Error (isDoesNotExistError)
import qualified System.Path as Path
import System.Path ((</>))
import System.Path.Directory (getDirectoryContents)

newtype Repo = Repo {repoFilePath :: Path.AbsDir} deriving (Show)

repoHeadPath, repoConfigPath :: Repo -> Path.AbsFile
repoConfigPath repo = repoGitPath repo </> configPath
repoHeadPath repo = repoGitPath repo </> headPath

repoGitPath, repoObjectsPath, repoRefsPath :: Repo -> Path.AbsDir
repoGitPath repo = repoFilePath repo </> gitPath
repoObjectsPath repo = repoGitPath repo </> objectsPath
repoRefsPath repo = repoGitPath repo </> refsPath

data RepoError = NotAGitRepo Path.AbsDir deriving Show

openRepo :: Path.AbsDir -> ExceptT RepoError IO Repo
openRepo path =
    liftIO (try $ getDirectoryContents $ repoGitPath repo) >>=
    either filterIOError guardListing
  where
    repo = Repo path
    die = throwError $ NotAGitRepo path
    filterIOError e = if isDoesNotExistError e then die else liftIO $ throwIO e
    guardListing listing =
      if all (`elem` listing)
        [ Path.toFileDir configPath
        , Path.toFileDir headPath
        , Path.toFileDir objectsPath
        , Path.toFileDir refsPath
        ]
      then return repo else die

headPath, configPath :: Path.RelFile
headPath = Path.relFile "HEAD"
configPath = Path.relFile "config"

gitPath, objectsPath, refsPath :: Path.RelDir
gitPath = Path.relDir ".git"
objectsPath = Path.relDir "objects"
refsPath = Path.relDir "refs"
