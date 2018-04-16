module Git.Objects.GitObject where

import Data.Attoparsec.ByteString (Parser)
import qualified Data.ByteString as BS
import Data.Word


data ObjectType =
  ObjTyBlob | ObjTyTree | ObjTyCommit | ObjTyTag
  deriving (Show, Eq, Enum, Bounded)

class GitObject a where
  gitObjectType :: proxy a -> ObjectType
  encodeObject :: a -> BS.ByteString
  objectParser :: Word64 -> Parser a
