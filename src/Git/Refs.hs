module Git.Refs where

import Control.Exception (try, throw)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString as BS
import Data.Tagged (Tagged(..))
import System.FilePath.Posix ((</>))
import System.Directory (listDirectory)
import System.IO (withBinaryFile, IOMode(..))
import System.IO.Error (isDoesNotExistError)

import Git.Repository (Repo, repoRefsPath)
import Git.Types (Sha1, Commit)
import Git.Types.Internal ()
import qualified Git.Types.Sha1 as Sha1

data RtHead = RtHead deriving (Show, Eq)
data RtTag = RtTag deriving (Show, Eq)
data RtRemote = RtRemote {rtRemoteName :: String} deriving (Show, Eq)

class RefType rt where
  refTypePath :: Repo -> rt -> FilePath

instance RefType RtHead where
  refTypePath repo _ = repoRefsPath repo </> "heads"
instance RefType RtTag where
  refTypePath repo _ = repoRefsPath repo </> "tags"
instance RefType RtRemote where
  refTypePath repo rt = repoRefsPath repo </> "remotes" </> rtRemoteName rt

data Ref rt = Ref {refType :: rt, unRef :: String} deriving (Show, Eq)

getRefs :: RefType rt => Repo -> rt -> IO [Ref rt]
getRefs repo rt = either filterError (fmap $ Ref rt) <$>
    try (listDirectory $ refTypePath repo rt)
  where
    filterError e = if isDoesNotExistError e then [] else throw e

openRef
  :: (MonadIO m, MonadFail m, RefType rt)
  => Repo -> Ref rt -> m (Tagged Commit Sha1)
openRef repo ref = let path = refTypePath repo (refType ref) </> unRef ref in do
  liftIO (
    withBinaryFile path ReadMode
    -- FIXME: perhaps it's not the nicest parsing just to take the first
    -- sha1Size bytes...
    (fmap (Sha1.fromByteString . BS.take Sha1.sha1Size) . BS.hGetContents))
  >>= either fail (return . Tagged)

data HEAD = HeadRef (Ref RtHead) | HeadCommit (Tagged Commit Sha1) deriving (Eq, Show)
