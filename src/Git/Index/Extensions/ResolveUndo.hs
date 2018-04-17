module Git.Index.Extensions.ResolveUndo where

import Git.Index.Extensions.Class (IndexExtension(..))

data ResolveUndo = ResolveUndo deriving (Show)

instance IndexExtension ResolveUndo where
  extSignature _ = "REUC"
  extParser = undefined
