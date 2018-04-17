module Git.Objects.Blob where

import Data.Attoparsec.ByteString (takeByteString)
import qualified Data.ByteString as BS
import Text.Printf (printf)

import Git.Objects.GitObject (GitObject(..), ObjectType(..))


data Blob = Blob {blobData :: BS.ByteString}

instance Show Blob where
  show = printf "<blob: %d>" . BS.length . blobData

instance GitObject Blob where
  gitObjectType _ = ObjTyBlob
  encodeObject = blobData
  objectParser size = Blob . BS.take (fromIntegral size) <$> takeByteString
