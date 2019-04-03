{-# LANGUAGE
    DataKinds
  , GADTs
  , KindSignatures
  , MultiParamTypeClasses
#-}

module Git.Objects.Serialise
  ( encodeLooseObject, encodeObject
  , decodeLooseObject, decodeObject
  , encodeObjectType
  , decodeSomeLooseObject
  , GitObject(..)
  ) where

import Prelude hiding (fail, take)

import Control.Monad (unless)
import Control.Monad.Fail (MonadFail(..))
import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Attoparsec.ByteString.Lazy
  ( Parser, string, choice, endOfInput, (<?>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy(..))
import Data.String (IsString)
import Data.Tagged (Tagged(..))
import Data.Word
import Text.Printf (printf)

import Git.Internal (char_, lazyParseOnly)
import Git.Sha1 (Sha1)
import qualified Git.Sha1 as Sha1
import Git.Objects.Blob (encodeBlob, blobParser)
import Git.Objects.Commit (encodeCommit, commitParser)
import Git.Objects.Internal
  ( Object(..), ObjectType(..), SomeObject(..), nObjectType)
import Git.Objects.Tag (encodeTag, tagParser)
import Git.Objects.Tree (encodeTree, treeParser)

encodeObjectType :: IsString str => ObjectType -> str
encodeObjectType objTy = case objTy of
  ObjTyBlob -> "blob"
  ObjTyTree -> "tree"
  ObjTyCommit -> "commit"
  ObjTyTag -> "tag"

encodeObject :: Object t -> BS.ByteString
encodeObject o = case o of
  Blob {} -> encodeBlob o
  Tree {} -> encodeTree o
  Commit {} -> encodeCommit o
  Tag {} -> encodeTag o

encodeLooseObject :: Object t -> (Tagged t Sha1, BS.ByteString)
encodeLooseObject o =
  let
    body = encodeObject o
    encodedWithHeader =
        encodeObjectType (nObjectType o) <> " "
        <> (Char8.pack $ printf "%d" $ BS.length body)
        <> "\NUL" <> body
    -- FIXME: are we concerned about encoding large files using too much memory?
    bodySha1 = Tagged $ Sha1.hashStrict encodedWithHeader
  in
    (bodySha1, encodedWithHeader)

objectTypeParser :: Parser ObjectType
objectTypeParser = choice $ mkParser <$> [minBound..]
  where
    mkParser objTy = string (encodeObjectType objTy) >> return objTy

getObjectParser :: ObjectType -> Word64 -> Parser SomeObject
getObjectParser objTy = case objTy of
    ObjTyBlob -> f . blobParser
    ObjTyTree -> f . treeParser
    ObjTyCommit -> f . commitParser
    ObjTyTag -> f . tagParser
  where
    f :: Parser (Object t) -> Parser SomeObject
    f = fmap SomeObject

looseSomeObjectParser :: Parser SomeObject
looseSomeObjectParser = do
  objTy <- objectTypeParser
  size <- (char_ ' ' >> decimal <* char_ '\NUL') <?>  "object size"
  getObjectParser objTy size

decodeSomeLooseObject :: MonadFail m => LBS.ByteString -> m SomeObject
decodeSomeLooseObject = lazyParseOnly (looseSomeObjectParser <* endOfInput)

class GitObject (t :: ObjectType) where
  goTy :: proxy t -> ObjectType
  goParser :: Word64 -> Parser (Object t)

instance GitObject 'ObjTyBlob where
  goTy _ = ObjTyBlob
  goParser = blobParser
instance GitObject 'ObjTyTree where
  goTy _ = ObjTyTree
  goParser = treeParser
instance GitObject 'ObjTyCommit where
  goTy _ = ObjTyCommit
  goParser = commitParser
instance GitObject 'ObjTyTag where
  goTy _ = ObjTyTag
  goParser = tagParser

decodeObject :: (MonadFail m, GitObject t) => BS.ByteString -> m (Object t)
decodeObject bs = either fail return $ parseOnly
  (goParser (fromIntegral $ BS.length bs) <* endOfInput) bs

looseObjectParser :: forall t. GitObject t => Parser (Object t)
looseObjectParser = do
  objTy <- objectTypeParser
  unless (objTy == goTy (Proxy @t)) $ fail $ "Bad object type: " ++ show objTy
  size <- (char_ ' ' >> decimal <* char_ '\NUL') <?>  "object size"
  goParser size

decodeLooseObject
  :: (MonadFail m, GitObject t) => LBS.ByteString -> m (Object t)
decodeLooseObject = lazyParseOnly (looseObjectParser <* endOfInput)
