{-# LANGUAGE
    DataKinds
  , GADTs
  , KindSignatures
  , LambdaCase
#-}

module Git.Objects.Internal where

import qualified Data.ByteString as BS
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (ZonedTime)
import qualified System.Path as Path
import Text.Printf (printf)

import Git.Sha1 (Sha1)
import Git.Types (FileMode(..))


data ObjectType =
  ObjTyBlob | ObjTyTree | ObjTyCommit | ObjTyTag
  deriving (Show, Eq, Enum, Bounded)

data TreeRow = TreeRow
  { treeRowMode :: FileMode
  , treeRowSha1 :: Sha1
  } deriving (Show, Eq)

data Object (t :: ObjectType) where
  Blob ::
    { blobData :: BS.ByteString
    } -> Blob
  Tree ::
    { unTree :: Map Path.RelFileDir TreeRow
    } -> Tree
  Commit ::
    { commitTreeHash :: Sha1
    , commitParents :: [Sha1]
    , commitAuthor :: Text
    , commitAuthorEmail :: Text
    , commitAuthoredAt :: ZonedTime
    , commitCommitter :: Text
    , commitCommitterEmail :: Text
    , commitCommittedAt :: ZonedTime
    , commitMsg :: BS.ByteString
    } -> Commit
  Tag :: Tag

type Blob = Object 'ObjTyBlob
type Tree = Object 'ObjTyTree
type Commit = Object 'ObjTyCommit
type Tag = Object 'ObjTyTag

instance Show (Object t) where
  show = \case
    Blob {blobData = bs} -> printf "<blob: %d>" $ BS.length bs
    Tree {unTree = m} -> printf "<tree: %s>" $ show m
    Commit
      { commitAuthor = a
      , commitAuthorEmail = e
      , commitAuthoredAt = t} -> printf "<commit: %s <%s> %s>" a e (show t)
    Tag {} -> "<tag>"

data SomeObject where
  SomeObject :: Object t -> SomeObject

objectType :: Object t -> ObjectType
objectType = \case
  Blob {} -> ObjTyBlob
  Tree {} -> ObjTyTree
  Commit {} -> ObjTyCommit
  Tag {} -> ObjTyTag
