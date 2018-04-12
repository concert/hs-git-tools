module Git.Refs where

import Control.Exception (try, throw)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser, parseOnly, string, takeByteString)
import qualified Data.Attoparsec.ByteString.Char8 as Char8
import qualified Data.ByteString as BS
import Data.Tagged (Tagged(..))
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import System.FilePath.Posix ((</>))
import System.Directory (listDirectory)
import System.IO (withBinaryFile, IOMode(..))
import System.IO.Error (isDoesNotExistError)

import Git.Internal ()
import Git.Objects (Commit)
-- FIXME: sha1HexParser should probably live nearer Git.Sha1...
import Git.Objects.Serialise (sha1HexParser)
import Git.Repository (Repo, repoRefsPath, repoHeadPath)
import Git.Sha1 (Sha1)
import qualified Git.Sha1 as Sha1


class Sha1Pointer a where
  getSha1 :: (MonadIO m, MonadFail m) => Repo -> a -> m Sha1

instance Sha1Pointer Sha1 where
  getSha1 _ = return


data RtHead = RtHead deriving (Show, Eq)
data RtTag = RtTag deriving (Show, Eq)
data RtRemote = RtRemote {rtRemoteName :: String} deriving (Show, Eq)

data Ref rt = Ref {refType :: rt, unRef :: String} deriving (Show, Eq)

class RefType rt where
  refTypePath :: Repo -> rt -> FilePath
  refTypePathParser :: Parser (Ref rt)

instance RefType RtHead where
  refTypePath repo _ = repoRefsPath repo </> "heads"
  refTypePathParser = string "refs/heads/" >> (Ref RtHead <$> stringParser)
instance RefType RtTag where
  refTypePath repo _ = repoRefsPath repo </> "tags"
  refTypePathParser = string "refs/tags/" >> (Ref RtTag <$> stringParser)
instance RefType RtRemote where
  refTypePath repo rt = repoRefsPath repo </> "remotes" </> rtRemoteName rt
  refTypePathParser = do
    _ <- string "refs/remotes/"
    remoteName <- decodeString <$> Char8.takeTill (== '/')
    refName <- stringParser
    return $ Ref (RtRemote remoteName) refName

getRefs :: RefType rt => Repo -> rt -> IO [Ref rt]
getRefs repo rt = either filterError (fmap $ Ref rt) <$>
    try (listDirectory $ refTypePath repo rt)
  where
    filterError e = if isDoesNotExistError e then [] else throw e

openRef
  :: (MonadIO m, MonadFail m, RefType rt)
  => Repo -> Ref rt -> m (Tagged Commit Sha1)
openRef repo ref = Tagged <$> getSha1 repo ref

instance RefType rt => Sha1Pointer (Ref rt) where
  getSha1 repo ref = let path = refTypePath repo (refType ref) </> unRef ref in
    liftIO (
      withBinaryFile path ReadMode
      (fmap (Sha1.fromHexText . Text.stripEnd . decodeUtf8) . BS.hGetContents))
    >>= either fail return


data HEAD rt
  = HeadRef (Ref rt)
  | HeadCommit (Tagged Commit Sha1)
  deriving (Eq, Show)

getHead :: (MonadIO m, MonadFail m) => Repo -> m (HEAD RtHead)
getHead repo = liftIO (
      withBinaryFile (repoHeadPath repo) ReadMode
      (\h -> parseOnly headParser <$> BS.hGetContents h))
    >>= either fail return
  where
    headParser = headRefParser <|> (HeadCommit . Tagged <$> sha1HexParser)
    headRefParser = string "ref: " >> fmap HeadRef refTypePathParser

instance RefType rt => Sha1Pointer (HEAD rt) where
  getSha1 repo h = case h of
    (HeadRef ref) -> getSha1 repo ref
    (HeadCommit sha1) -> return $ unTagged sha1


decodeString :: BS.ByteString -> String
decodeString = Text.unpack . Text.stripEnd . decodeUtf8

stringParser :: Parser String
stringParser = decodeString <$> takeByteString
