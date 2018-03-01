module Git.Types.Objects where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (TimeZone, LocalTime, utcToLocalTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Text.Printf (printf)

import Git.Types.Sha1 (Sha1)
import Git.Types.SizedByteString (SizedByteString)
import qualified Git.Types.SizedByteString as SBS

data Blob = Blob {blobData :: SizedByteString}

instance Show Blob where
  show = printf "<blob: %d>" . SBS.length . blobData

data Tree = Tree

data Commit = Commit
  { commitTreeHash :: Sha1
  , commitParents :: [Sha1]
  , commitAuthor :: Text
  , commitAuthorEmail :: Text
  , commitAuthoredAt :: POSIXTime
  , commitAuthoredTz :: TimeZone
  , commitCommitter :: Text
  , commitCommitterEmail :: Text
  , commitCommittedAt :: POSIXTime
  , commitCommittedTz :: TimeZone
  , commitMsg :: SizedByteString
  }

instance Show Commit where
  show c = printf "<commit: %s <%s> %s>"
    (Text.unpack $ commitAuthor c)
    (Text.unpack $ commitAuthorEmail c)
    (show $ toLocalTime (commitAuthoredAt c) (commitAuthoredTz c))

toLocalTime :: POSIXTime -> TimeZone -> LocalTime
toLocalTime pt tz = utcToLocalTime tz $ posixSecondsToUTCTime pt

data Tag = Tag

data Object
  = ObjectBlob Blob
  | ObjectTree Tree
  | ObjectCommit Commit
  | ObjectTag Tag
