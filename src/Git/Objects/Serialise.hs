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
  ) where

import Prelude hiding (fail, take)

import Control.Monad.Fail (MonadFail(..))
import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Attoparsec.ByteString.Lazy
  ( Parser, string, choice, endOfInput, (<?>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ((<>))
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
import Git.Objects.Object (Object(..))
import Git.Objects.GitObject (GitObject(..), ObjectType(..))
import Git.Objects.Internal (NewObject(..), SomeNewObject(..), nObjectType)
import Git.Objects.Tag (encodeTag, tagParser)
import Git.Objects.Tree (encodeTree, treeParser)

encodeObjectType :: IsString str => ObjectType -> str
encodeObjectType objTy = case objTy of
  ObjTyBlob -> "blob"
  ObjTyTree -> "tree"
  ObjTyCommit -> "commit"
  ObjTyTag -> "tag"

encodeLooseObject
  :: forall a. GitObject a => a -> (Tagged a Sha1, BS.ByteString)
encodeLooseObject obj =
  let
    body = encodeObject obj
    encodedWithHeader =
        encodeObjectType (gitObjectType $ Proxy @a) <> " "
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

getObjectParser :: ObjectType -> Word64 -> Parser Object
getObjectParser objTy size = case objTy of
  ObjTyBlob -> ObjBlob <$> objectParser size
  ObjTyTree -> ObjTree <$> objectParser size
  ObjTyCommit -> ObjCommit <$> objectParser size
  ObjTyTag -> ObjTag <$> objectParser size

looseObjectParser :: Parser Object
looseObjectParser = do
  objTy <- objectTypeParser
  size <- (char_ ' ' >> decimal <* char_ '\NUL') <?>  "object size"
  getObjectParser objTy size

decodeLooseObject :: MonadFail m => LBS.ByteString -> m Object
decodeLooseObject = lazyParseOnly (looseObjectParser <* endOfInput)

decodeObject :: (MonadFail m, GitObject a) => BS.ByteString -> m a
decodeObject bs = either fail return $ parseOnly
  (objectParser (fromIntegral $ BS.length bs) <* endOfInput) bs


nEncodeObject :: NewObject t -> BS.ByteString
nEncodeObject o = case o of
  NObjBlob {} -> encodeBlob o
  NObjTree {} -> encodeTree o
  NObjCommit {} -> encodeCommit o
  NObjTag {} -> encodeTag o

nEncodeLooseObject :: NewObject t -> (Tagged t Sha1, BS.ByteString)
nEncodeLooseObject o =
  let
    body = nEncodeObject o
    encodedWithHeader =
        encodeObjectType (nObjectType o) <> " "
        <> (Char8.pack $ printf "%d" $ BS.length body)
        <> "\NUL" <> body
    -- FIXME: are we concerned about encoding large files using too much memory?
    bodySha1 = Tagged $ Sha1.hashStrict encodedWithHeader
  in
    (bodySha1, encodedWithHeader)

nGetObjectParser :: ObjectType -> Word64 -> Parser SomeNewObject
nGetObjectParser objTy = case objTy of
    ObjTyBlob -> f . blobParser
    ObjTyTree -> f . treeParser
    ObjTyCommit -> f . commitParser
    ObjTyTag -> f . tagParser
  where
    f :: Parser (NewObject t) -> Parser SomeNewObject
    f = fmap SomeNewObject

nLooseObjectParser :: Parser SomeNewObject
nLooseObjectParser = do
  objTy <- objectTypeParser
  size <- (char_ ' ' >> decimal <* char_ '\NUL') <?>  "object size"
  nGetObjectParser objTy size

class NewGitObject (t :: ObjectType) where
  ngoParser :: Word64 -> Parser (NewObject t)

instance NewGitObject ObjTyBlob where
  ngoParser = blobParser
instance NewGitObject ObjTyTree where
  ngoParser = treeParser
instance NewGitObject ObjTyCommit where
  ngoParser = commitParser
instance NewGitObject ObjTyTag where
  ngoParser = tagParser

nDecodeObject
  :: (MonadFail m, NewGitObject t) => BS.ByteString -> m (NewObject t)
nDecodeObject bs = either fail return $ parseOnly
  (ngoParser (fromIntegral $ BS.length bs) <* endOfInput) bs
