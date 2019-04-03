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
import Git.Objects.Tree (TreeRow)
import Git.Sha1 (Sha1)


data NewObject (t :: ObjectType) where
  NObjBlob ::
    { nobjBlobData :: BS.ByteString
    } -> NewObject 'ObjTyBlob
  NObjTree ::
    { nobjUnTree :: Map Path.RelFileDir TreeRow
    } -> NewObject 'ObjTyTree
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
    } -> NewObject 'ObjTyCommit
  NObjTag :: NewObject 'ObjTyTag

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
