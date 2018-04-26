module Git.Types
  ( FileMode(..), fileModeToInt, fileModeFromInt
  , checkPath, legalPathComponent
  , module X
  ) where

import Prelude hiding (fail)

import Control.Monad (unless)
import Control.Monad.Fail (MonadFail(..))
import Text.Printf (printf)
import qualified System.Path as Path
import System.Path.PartClass (AbsRel, FileDir)

import Git.Internal (pathToList)
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
  Directory -> 0o040000
  NonExecFile -> 0o100644
  NonExecGroupWriteFile -> 0o100664
  ExecFile -> 0o100755
  SymLink -> 0o120000
  GitLink -> 0o160000

fileModeFromInt :: MonadFail m => Int -> m FileMode
fileModeFromInt i = maybe (fail $ printf "Bad file mode 0o%06o" i) return $
  lookup i $ [(fileModeToInt fm, fm) | fm <- [minBound..]]

illegalPathComponent :: String -> Bool
illegalPathComponent comp = comp `elem` ["", ".", "..", ".git"] ||
  any (\c -> c == '\NUL' || c == '/') comp

legalPathComponent :: String -> Bool
legalPathComponent = not . illegalPathComponent

checkPath :: (AbsRel ar, FileDir fd, MonadFail m) => Path.Path ar fd -> m ()
checkPath path = mapM_ checkComponent $ pathToList path
  where
    checkComponent comp = unless (legalPathComponent comp) $ fail $
      printf "Bad path component '%s' in path %s" comp (Path.toString path)
