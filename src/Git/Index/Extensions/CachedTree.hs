module Git.Index.Extensions.CachedTree where

import Git.Index.Extensions.Class (IndexExtension(..))

data CachedTree = CachedTree deriving (Show)

instance IndexExtension CachedTree where
  extSignature _ = "TREE"
  extParser = undefined
