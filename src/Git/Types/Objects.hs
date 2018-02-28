module Git.Types.Objects where

import Data.ByteString as BS
import Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
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
  , commitAuthor :: Text
  , commitAuthorEmail :: Text
  , commitAuthoredAt :: POSIXTime
  , commitAuthoredTz :: ()
  , commitCommitter :: Text
  , commitCommitterEmail :: Text
  , commitCommittedAt :: POSIXTime
  , commitCommittedTz :: ()
  , commitMsg :: BS.ByteString
  }

data Tag = Tag

data Object
  = ObjectBlob Blob
  | ObjectTree Tree
  | ObjectCommit Commit
  | ObjectTag Tag
