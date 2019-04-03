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

import Git.Objects.GitObject (ObjectType(..))
import Git.Sha1 (Sha1)
import Git.Types (FileMode(..))


data TreeRow = TreeRow
  { treeRowMode :: FileMode
  , treeRowSha1 :: Sha1
  } deriving (Show, Eq)

data NewObject (t :: ObjectType) where
  NObjBlob ::
    { nobjBlobData :: BS.ByteString
    } -> Blob
  NObjTree ::
    { nobjUnTree :: Map Path.RelFileDir TreeRow
    } -> Tree
  NObjCommit ::
    { nobjCommitTreeHash :: Sha1
    , nobjCommitParents :: [Sha1]
    , nobjCommitAuthor :: Text
    , nobjCommitAuthorEmail :: Text
    , nobjCommitAuthoredAt :: ZonedTime
    , nobjCommitCommitter :: Text
    , nobjCommitCommitterEmail :: Text
    , nobjCommitCommittedAt :: ZonedTime
    , nobjCommitMsg :: BS.ByteString
    } -> Commit
  NObjTag :: Tag

type Blob = NewObject 'ObjTyBlob
type Tree = NewObject 'ObjTyTree
type Commit = NewObject 'ObjTyCommit
type Tag = NewObject 'ObjTyTag

instance Show (NewObject t) where
  show = \case
    NObjBlob {nobjBlobData = bs} -> printf "<blob: %d>" $ BS.length bs
    NObjTree {nobjUnTree = m} -> printf "<tree: %s>" $ show m
    NObjCommit
      { nobjCommitAuthor = a
      , nobjCommitAuthorEmail = e
      , nobjCommitAuthoredAt = t} -> printf "<commit: %s <%s> %s>" a e (show t)
    NObjTag {} -> "<tag>"

data SomeNewObject where
  SomeNewObject :: NewObject t -> SomeNewObject

nObjectType :: NewObject t -> ObjectType
nObjectType = \case
  NObjBlob {} -> ObjTyBlob
  NObjTree {} -> ObjTyTree
  NObjCommit {} -> ObjTyCommit
  NObjTag {} -> ObjTyTag
