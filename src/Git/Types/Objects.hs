module Git.Types.Objects where

import Prelude hiding (fail)

import Control.Monad.Fail (MonadFail(..))
import qualified Data.ByteString as BS
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (TimeZone, ZonedTime, utcToZonedTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Text.Printf (printf)

import Git.Types.Sha1 (Sha1)

data Blob = Blob {blobData :: BS.ByteString} deriving Eq

instance Show Blob where
  show = printf "<blob: %d>" . BS.length . blobData

newtype Tree = Tree {unTree :: Map Text TreeRow} deriving (Show, Eq)

data FileMode
  = Directory
  | NonExecFile
  | NonExecGroupWriteFile
  | ExecFile
  | SymLink
  | GitLink
  deriving (Show, Eq, Ord, Enum, Bounded)

fileModeToInt :: FileMode -> Int
fileModeToInt fm = case fm of
  Directory -> 0o40000
  NonExecFile -> 0o100644
  NonExecGroupWriteFile -> 0o100664
  ExecFile -> 0o100755
  SymLink -> 0o120000
  GitLink -> 0o160000

fileModeFromInt :: MonadFail m => Int -> m FileMode
fileModeFromInt i = maybe (fail $ "Bad file mode " ++ show i) return $
  lookup i [(fileModeToInt fm, fm) | fm <- [minBound..]]

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

data Object
  = ObjBlob Blob
  | ObjTree Tree
  | ObjCommit Commit
  | ObjTag Tag

objectType :: Object -> ObjectType
objectType obj = case obj of
  ObjBlob _ -> ObjTyBlob
  ObjTree _ -> ObjTyTree
  ObjCommit _ -> ObjTyCommit
  ObjTag _ -> ObjTyTag
