{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
#-}

module Git.Index.Update where

import Control.Exception (try, IOException)
import Control.Monad (when)
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.State (MonadState(..), StateT(..))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Tagged (unTagged)
import System.Posix.Files (getFileStatus)
import qualified System.Path as Path
import System.Path ((</>))
import System.Path.Directory (relDirectoryContents)
import System.Path.IO (openBinaryFile, IOMode(ReadMode))
import qualified Data.ByteString.Lazy as LBS
import Data.Attoparsec.ByteString (Parser, endOfInput)

import Git.Internal (lazyParseOnly)
import Git.Types (GitError(..))
import qualified Git.Objects.Blob as Blob
import Git.Repository (Repo(..), repoIndexPath)
import Git.Store (storeObject)

import Git.Index.Builder (writeIndex)
import Git.Index.Index (Index(..), gfsFromIndex)
import Git.Index.Parser (openIndex)
import Git.Index.Types
  (Stages(..), IndexEntry(..), IndexEntries, GitFileStat(..), gfsFromStat)
import Git.Index.Ignore (ignore, Ignores, ignoresP)


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
  :: (MonadIO m, MonadError GitError m, MonadState Index m, MonadReader Repo m)
  => Path.RelFile -> m ()
updateIndex relpath = do
    repo <- ask
    let abspath = repoFilePath repo </> relpath
    stat <- liftIO $ getFileStatus $ Path.toString abspath
    workingGfs <- either (throwError . CannotStatFile) return $ gfsFromStat stat
    idx <- get
    when (maybe True id $ shouldUpdateContent workingGfs <$> getIdxGfs idx) $ do
      sha1 <- liftIO $ Blob.fromFile abspath >>= storeObject repo
      let stages = Normal $ IndexEntry workingGfs (unTagged sha1) mempty
      put $ insertIdxEntry (Path.toFileDir relpath) stages idx
  where
    getIdxGfs idx = gfsFromIndex (Path.toFileDir relpath) idx >>= \case
      Normal ie -> Just ie
      _ -> Nothing

parseContent :: MonadFail m => Parser a -> Path.AbsFile -> IO (m a)
parseContent parser path = do
    content <- openBinaryFile path ReadMode >>= LBS.hGetContents
    pure $ lazyParseOnly (parser <* endOfInput) content

getIgnores :: (MonadIO m, MonadError GitError m) => Path.AbsDir -> m Ignores
getIgnores dirP = do
    eMfIgnores <- liftIO $ (
        try $ parseContent ignoresP $ dirP </> Path.relFile ".gitignore"
        :: IO (Either IOException (Either String Ignores)))
    case eMfIgnores of
        Left _ -> pure mempty  -- Failed to open .gitignore
        Right mfIgnores -> either (throwError . ParseError) pure mfIgnores

updateIndexDir
  :: (MonadIO m, MonadError GitError m, MonadState Index m, MonadReader Repo m)
  => Path.RelDir -> m ()
updateIndexDir targetDir = do
    repoRoot <- repoFilePath <$> ask
    parentIgnores <- mapM getIgnores $ parentDirPaths repoRoot
    addDirContents repoRoot (foldr (<>) mempty parentIgnores) targetDir
  where
    (_, targetSegs, _) = Path.splitPath $ Path.normalise targetDir
    parentDirPaths repoRoot = snd $ foldr
        (\seg (pp, acc) -> let p = pp </> seg in (p, p : acc))
        (repoRoot, [])
        (drop 1 $ reverse targetSegs)
    addDirContents repoRoot parentIgnores relPath = do
      (dirs, files) <- liftIO $ relDirectoryContents $ repoRoot </> relPath
      dirIgnores <- getIgnores $ Path.makeAbsolute repoRoot relPath
      let ignores = parentIgnores <> dirIgnores
      mapM_ updateIndex $ filter (not . ignore ignores) files
      -- FIXME: Also need to ignore .git
      mapM_ (addDirContents repoRoot ignores) $ fmap (relPath </>) $ filter (not . ignore ignores) dirs

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
