{-# LANGUAGE
    DataKinds
#-}

module Git.Objects.Tag where

import Data.Attoparsec.ByteString (Parser)
import qualified Data.ByteString as BS
import Data.Word

import Git.Objects.GitObject (GitObject(..))
import Git.Objects.Internal (NewObject(..), ObjectType(..))


data Tag = Tag

instance GitObject Tag where
  gitObjectType _ = ObjTyTag
  encodeObject = undefined
  objectParser = undefined

encodeTag :: NewObject 'ObjTyTag -> BS.ByteString
encodeTag = undefined

tagParser :: Word64 -> Parser (NewObject 'ObjTyTag)
tagParser = undefined
