{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
#-}

module Git.Index.Update where

import Control.Monad (when)
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (MonadState(..), StateT(..))
import qualified Data.Map as Map
import Data.Tagged (unTagged)
import System.Posix.Files (getFileStatus)
import qualified System.Path as Path
import System.Path ((</>))

import Git.Types (GitError(..))
import qualified Git.Objects.Blob as Blob
import Git.Repository (Repo(..), repoIndexPath)
import Git.Store (storeObject)

import Git.Index.Builder (writeIndex)
import Git.Index.Index (Index(..), gfsFromIndex)
import Git.Index.Parser (openIndex)
import Git.Index.Types
  (Stages(..), IndexEntry(..), IndexEntries, GitFileStat(..), gfsFromStat)


type IndexM m r = ExceptT GitError (ReaderT Repo (StateT Index m)) r

withIndex :: (MonadIO m) => Repo -> IndexM m r -> m r
withIndex repo im = let path = repoIndexPath repo in do
    idx <- (runExceptT $ openIndex path) >>= dieOnErr
    (res, idx') <- runStateT (runReaderT (runExceptT im) repo) idx
    -- Whether we died or not, we have to write the updated index because we may
    -- have made corresponding state changes to the repository:
    liftIO $ writeIndex path idx'
    dieOnErr res
  where
    dieOnErr = either (liftIO . error . show) return


updateIndex
  :: (MonadIO m, MonadError GitError m, MonadState Index m)
  => Repo -> Path.RelFile -> m ()
updateIndex repo relpath = do
    stat <- liftIO $ getFileStatus $ Path.toString abspath
    workingGfs <- either (throwError . CannotStatFile) return $ gfsFromStat stat
    idx <- get
    when (maybe True id $ shouldUpdateContent workingGfs <$> getIdxGfs idx) $ do
      sha1 <- liftIO $ Blob.fromFile abspath >>= storeObject repo
      let stages = Normal $ IndexEntry workingGfs (unTagged sha1) mempty
      put $ insertIdxEntry (Path.toFileDir relpath) stages idx
  where
    abspath = repoFilePath repo </> relpath
    getIdxGfs idx = gfsFromIndex (Path.toFileDir relpath) idx >>= \case
      Normal ie -> Just ie
      _ -> Nothing

shouldUpdateContent :: GitFileStat -> GitFileStat -> Bool
shouldUpdateContent workingGfs idxGfs =
     gfsChangedAt workingGfs > gfsChangedAt idxGfs
  || gfsSize workingGfs /= gfsSize idxGfs
  || gfsInodeNo workingGfs /= gfsInodeNo idxGfs
  || gfsDevId workingGfs /= gfsDevId idxGfs

insertIdxEntry :: Path.RelFileDir -> Stages IndexEntry -> Index -> Index
insertIdxEntry path sie = overIes $ Map.insert path sie

overIes :: (IndexEntries -> IndexEntries) -> Index -> Index
overIes f idx = idx {indexEntries = f $ indexEntries idx}
