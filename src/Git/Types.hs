module Git.Types
  ( FileMode(..), fileModeToInt, fileModeFromInt
  , module X
  ) where

import Prelude hiding (fail)

import Control.Monad.Fail (MonadFail(..))
import Text.Printf (printf)

import Git.Types.Error as X


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
fileModeFromInt i = maybe (fail $ printf "Bad file mode 0o%o" i) return $
  lookup i $ (\fm -> (fileModeToInt fm, fm)) <$> [minBound..]
