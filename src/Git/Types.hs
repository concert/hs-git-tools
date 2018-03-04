module Git.Types (module X, Version(..)) where

import Git.Types.Sha1 as X
import Git.Types.Objects as X

data Version = Version1 | Version2 deriving (Show, Eq, Enum, Bounded)
