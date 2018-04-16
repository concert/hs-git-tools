module Git.Objects.Tag where

import Git.Objects.GitObject (GitObject(..), ObjectType(..))


data Tag = Tag

instance GitObject Tag where
  gitObjectType _ = ObjTyTag
  encodeObject = undefined
  objectParser = undefined
