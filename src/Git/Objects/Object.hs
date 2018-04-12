{-# LANGUAGE
    MultiParamTypeClasses
#-}

module Git.Objects.Object where

import Git.Internal (Wrapable(..))
import Git.Objects.Types (Blob, Commit, Tag, Tree, ObjectType(..))


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

instance Wrapable Object Blob where
  wrap = ObjBlob
  unwrap (ObjBlob blob) = return blob
  unwrap _ = fail "Incorrect object type"

instance Wrapable Object Tree where
  wrap = ObjTree
  unwrap (ObjTree t) = return t
  unwrap _ = fail "Incorrect object type"

instance Wrapable Object Commit where
  wrap = ObjCommit
  unwrap (ObjCommit c) = return c
  unwrap _ = fail "Incorrect object type"

instance Wrapable Object Tag where
  wrap = ObjTag
  unwrap (ObjTag t) = return t
  unwrap _ = fail "Incorrect object type"
