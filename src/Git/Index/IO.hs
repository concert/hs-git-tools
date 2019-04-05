{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
#-}
module Git.Index.IO where

import Control.Exception (try)
import Control.Monad.Except (MonadError(..), runExceptT, ExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)
import Data.Attoparsec.ByteString (Parser, endOfInput)
import qualified Data.ByteString.Lazy as LBS
import qualified System.Path as Path
import System.Path.IO (openBinaryFile, withBinaryFile, IOMode(..))

import GHC.IO.Exception (IOException(..), IOErrorType(..))

import Git.Index.Builder (indexLbs)
import Git.Index.Index (Index, index)
import Git.Index.Update (IndexM)
import Git.Index.Parser (indexP)
import Git.Internal (lazyParseOnly)
import Git.Repository (Repo, repoIndexPath)
import Git.Types.Error (GitError(..))


openParsable'
  :: (MonadIO m, MonadError GitError m) => Parser a -> FilePath -> m a
openParsable' parser path = p path >>= openParsable parser
  where
    p = either (throwError . ParseError) return . Path.parse

openParsable
  :: (MonadIO m, MonadError GitError m) => Parser a -> Path.AbsFile -> m a
openParsable parser path = do
  result <- liftIO $ try $ openBinaryFile path ReadMode >>= LBS.hGetContents
  case result of
    Left ioErr -> throwError $ ErrorWithIO ioErr
    Right content -> either (throwError . ParseError) return $
        lazyParseOnly (parser <* endOfInput) content

openIndex' :: (MonadIO m, MonadError GitError m) => FilePath -> m Index
openIndex' = openParsable' indexP

openIndex :: (MonadIO m, MonadError GitError m) => Path.AbsFile -> m Index
openIndex = openParsable indexP


writeIndex :: Path.AbsFile -> Index -> IO ()
writeIndex path idx = withBinaryFile path WriteMode $ \h ->
  LBS.hPut h $ indexLbs idx


-- | Guarantees to give us an `Index` even if there is no .git/index file
withIndex
  :: (MonadIO m, MonadError GitError m) => Path.AbsFile -> (Index -> m r) -> m r
withIndex path mr = do
    idx <- catchError (openIndex path) $ \case
      ErrorWithIO (IOError {ioe_type = NoSuchThing}) ->
        return $ index maxBound
      e -> throwError e
    mr idx

withIndexM
  :: (MonadIO m, MonadError GitError m) => Repo -> IndexM m r -> m r
withIndexM repo im = let path = repoIndexPath repo in
  withIndex path $ \idx -> do
    (res, idx') <- runStateT (runReaderT (runExceptT im) repo) idx
    -- Whether we died or not, we have to write the updated index because we may
    -- have made corresponding state changes to the repository:
    liftIO $ writeIndex path idx'
    either throwError return res

-- Bleugh, this transformer wrapping feels yucky...
-- It's kind of a facet of the fact that errors can occur both within the IndexM
-- thing and in the wrapping context, and that this is a convenience for just
-- being able to run IndexM things interactively in IO.
withIndexM' :: (MonadIO m) => Repo -> IndexM (ExceptT GitError m) r -> m r
withIndexM' repo im =
  (runExceptT $ withIndexM repo im)
  >>= either (liftIO . error . show) return
