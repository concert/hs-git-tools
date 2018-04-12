module Git.Objects.Types where

import qualified Data.ByteString as BS
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (TimeZone, ZonedTime, utcToZonedTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Text.Printf (printf)

import Git.Sha1 (Sha1)
import Git.Types (FileMode)

data Blob = Blob {blobData :: BS.ByteString} deriving Eq

instance Show Blob where
  show = printf "<blob: %d>" . BS.length . blobData

newtype Tree = Tree {unTree :: Map Text TreeRow} deriving (Show, Eq)

data TreeRow = TreeRow
  { treeRowMode :: FileMode
  , treeRowSha1 :: Sha1
  } deriving (Show, Eq)

data Commit = Commit
  { commitTreeHash :: Sha1
  , commitParents :: [Sha1]
  , commitAuthor :: Text
  , commitAuthorEmail :: Text
  , commitAuthoredAt :: ZonedTime
  , commitCommitter :: Text
  , commitCommitterEmail :: Text
  , commitCommittedAt :: ZonedTime
  , commitMsg :: BS.ByteString
  }

instance Show Commit where
  show c = printf "<commit: %s <%s> %s>"
    (Text.unpack $ commitAuthor c)
    (Text.unpack $ commitAuthorEmail c)
    (show $ commitAuthoredAt c)

toZonedTime :: POSIXTime -> TimeZone -> ZonedTime
toZonedTime pt tz = utcToZonedTime tz $ posixSecondsToUTCTime pt

data Tag = Tag

data ObjectType =
  ObjTyBlob | ObjTyTree | ObjTyCommit | ObjTyTag
  deriving (Show, Eq, Enum, Bounded)
