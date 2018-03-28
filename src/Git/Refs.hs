module Git.Refs where

import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString as BS
import Data.Tagged (Tagged(..))
import System.FilePath.Posix ((</>))
import System.Directory (listDirectory)
import System.IO (withBinaryFile, IOMode(..))

import Git.Repository (Repo, repoRefsPath)
import Git.Types (Sha1, Commit)
import Git.Types.Internal ()
import qualified Git.Types.Sha1 as Sha1

newtype Ref = Ref {unRef :: String} deriving (Show, Eq)

data HEAD = HeadRef Ref | HeadCommit (Tagged Commit Sha1) deriving (Eq, Show)

getRefs :: Repo -> IO [(Ref, Tagged Commit Sha1)]
getRefs repo = let headsPath = repoRefsPath repo </> "heads" in
  listDirectory headsPath >>=
  mapM (\n -> (Ref n,) <$> openRef (headsPath </> n))

openRef :: (MonadIO m, MonadFail m) => FilePath -> m (Tagged Commit Sha1)
openRef path = do
  liftIO (
    withBinaryFile path ReadMode
    -- FIXME: perhaps it's not the nicest parsing just to take the first
    -- sha1Size bytes...
    (fmap (Sha1.fromByteString . BS.take Sha1.sha1Size) . BS.hGetContents))
  >>= either fail (return . Tagged)
