module Git.Types.Error where

import Data.Word

data GitError
  = ErrorWithIO IOError
  | GenericError String
  | ParseError String
  | UnsupportedPackIndexVersion
  | UnsupportedOperation String
  | Sha1NotInIndex
  | UnsupportedPackFileVersion
  | UnrecognisedPackObjectType Word8
  | FailedPreDeltaApplicationLengthCheck
  | FailedPostDeltaApplicationLengthCheck
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
    FailedPreDeltaApplicationLengthCheck ->
      "Failed pre-delta-application length check"
    FailedPostDeltaApplicationLengthCheck ->
      "Failed post-delta-application length check"
