module Git.Types.Error where

import Control.Exception (Exception)
import Data.Word
import Text.Printf (printf)

data GitError
  = ErrorWithIO IOError
  | GenericError String
  | ParseError String
  | UnsupportedPackIndexVersion
  | UnsupportedOperation String
  | Sha1NotInIndex
  | UnsupportedPackFileVersion
  | UnrecognisedPackObjectType Word8
  | FailedDeltaConsistencyCheck
  -- FIXME: decide about what type lengths and offsets should be, given
  -- ByteString's API...
  | FailedPreDeltaApplicationLengthCheck Int Word64
  | FailedPostDeltaApplicationLengthCheck Int Word64
  deriving Eq

instance Show GitError where
  show e = case e of
    ErrorWithIO ioe -> "IO error: " ++ show ioe
    GenericError s -> "Error: " ++ s
    ParseError s -> "Parse error: " ++ s
    UnsupportedPackIndexVersion -> "Unsupported pack index version"
    UnsupportedOperation s -> "Unsupported operation: " ++ s
    Sha1NotInIndex -> "sha1 not in index"
    UnsupportedPackFileVersion -> "Unsupported pack file version"
    UnrecognisedPackObjectType w -> "Unrecognised pack object type: " ++ show w
    FailedDeltaConsistencyCheck -> "Failed delta consistency check"
    FailedPreDeltaApplicationLengthCheck actual expected -> printf
      "Failed pre-delta-application length check: %d (actual) /= %d (expected)"
      actual expected
    FailedPostDeltaApplicationLengthCheck actual expected -> printf
      "Failed post-delta-application length check: %d (actual) /= %d (expected)"
      actual expected

instance Exception GitError
