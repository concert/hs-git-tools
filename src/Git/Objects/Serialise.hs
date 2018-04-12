{-# LANGUAGE
    MultiParamTypeClasses
#-}

module Git.Objects.Serialise
  ( encodeLooseObject, encodeObject
  , decodeLooseObject, decodeObject
  , GitObject, objectType, wrap, unwrap
  , encodeObjectType
  , tellParsePos
  , lazyParseOnly
  , sha1HexParser
  ) where

import Prelude hiding (fail, take)

import Control.Monad (void)
import Control.Monad.Fail (MonadFail(..))
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.ByteString.Char8
  (char, signed, takeTill, anyChar, decimal, isDigit, satisfy)
import Data.Attoparsec.ByteString.Lazy
  ( Parser, Result(..), parse, string, choice, endOfInput
  , takeByteString, many', many1, take, anyWord8, (<?>))
import qualified Data.Attoparsec.Internal.Types as ApIntern
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord, digitToInt)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.List (intercalate, foldl')
import Data.Proxy (Proxy(..))
import Data.String (IsString)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
  (minutesToTimeZone, timeZoneMinutes, ZonedTime(..), zonedTimeToUTC)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word
import Text.Printf (printf)

import Git.Internal (Wrapable(..))
import Git.Sha1 (Sha1)
import Git.Types (fileModeFromInt, fileModeToInt)
import qualified Git.Sha1 as Sha1
import Git.Objects.Object (Object(..))
import Git.Objects.Types
  (Blob(..), Tree(..), TreeRow(..), Commit(..), Tag(..), toZonedTime
  , ObjectType(..))

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
        encodeObjectType (objectType $ Proxy @a) <> " "
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


class GitObject a where
  objectType :: proxy a -> ObjectType
  encodeObject :: a -> BS.ByteString
  objectParser :: Word64 -> Parser a

instance GitObject Blob where
  objectType _ = ObjTyBlob
  encodeObject = blobData
  objectParser size = Blob . BS.take (fromIntegral size) <$> takeByteString

sha1ByteStringP :: Parser Sha1
sha1ByteStringP = take 20 >>= Sha1.fromByteString

instance GitObject Tree where
  objectType _ = ObjTyTree
  encodeObject =
      Builder.toByteString . mconcat . fmap rowB . Map.toList . unTree
    where
      sha1ByteStringB = b . Sha1.unSha1
      fileModeB = b . toOctBS . fileModeToInt
      nameB = b . encodeUtf8
      rowB (name, TreeRow mode sha1) =
        fileModeB mode <> b " " <> nameB name <> b "\NUL"
        <> sha1ByteStringB sha1
  objectParser _ = Tree . Map.fromList <$> many' rowP
    where
      rowP = do
        fileMode <- (oct >>= fileModeFromInt) <* char ' '
        name <- decodeUtf8 <$> takeTill' (== '\NUL')
        sha1 <- sha1ByteStringP
        return (name, TreeRow fileMode sha1)


instance GitObject Commit where
  objectType _ = ObjTyCommit
  encodeObject c = metadata <> "\n" <> commitMsg c
    where
      metadata = Builder.toByteString $
           sha1RowB "tree" (commitTreeHash c)
        <> mconcat (sha1RowB "parent" <$> commitParents c)
        <> contributorRowB "author" (commitAuthor c)
             (commitAuthorEmail c) (commitAuthoredAt c)
        <> contributorRowB "committer" (commitCommitter c)
             (commitCommitterEmail c) (commitCommittedAt c)
      sha1StringB = b . Char8.pack . Sha1.toHexString
      sha1RowB role sha1 = b role <> b " " <> sha1StringB sha1 <> b "\n"
      contributorRowB r n e t = b r <> b " " <> b (encodeUtf8 n) <> b " <"
        <> b (encodeUtf8 e) <> b "> " <> tB t <> b "\n"
      tB t =
        let
          tm = floor $ utcTimeToPOSIXSeconds $ zonedTimeToUTC t :: Int
          tzm = timeZoneMinutes $ zonedTimeZone t
          (h, m) = divMod (abs tzm) 60
        in
          bt (Text.pack $ printf "%d" tm)
          <> b " "
          <> (if tzm < 0 then b "-" else b "+")
          <> bt (Text.pack $ printf "%02d" h)
          <> bt (Text.pack $ printf "%02d" m)
  objectParser size = do
      startPos <- tellParsePos
      treeSha1 <- treeRowP
      parentSha1s <- many' parentRowP
      (authName, authEmail, authAt, authTz) <- contributorRowP "author"
      (commName, commEmail, commAt, commTz) <- contributorRowP "committer"
      char_ '\n'
      pos <- tellParsePos
      msg <- BS.take (startPos + fromIntegral size - pos) <$> takeByteString
      return $ Commit
        treeSha1
        parentSha1s
        authName authEmail (toZonedTime authAt authTz)
        commName commEmail (toZonedTime commAt commTz)
        msg
    where
      sha1RowP role = string role >> char ' ' >> sha1HexParser <* char '\n'
      treeRowP = sha1RowP "tree"
      parentRowP = sha1RowP "parent"
      emailP = char '<' >> takeTill' (== '>')
      twoDigit :: Parser Int
      twoDigit = do
        x <- digitToInt <$> satisfy isDigit
        y <- digitToInt <$> satisfy isDigit
        return $ 10 * x + y
      tzMinutes = do
        hrs <- twoDigit
        mins <- twoDigit
        return $ 60 * hrs + mins
      contributorRowP role = do
        _ <- string role
        char_ ' '
        name <- decodeUtf8 . BS.init <$> takeTill (== '<')
        email <- decodeUtf8 <$> emailP
        char_ ' '
        posixTime <- fromIntegral @Int <$> decimal
        char_ ' '
        tz <- minutesToTimeZone <$> signed tzMinutes
        char_ '\n'
        return (name, email, posixTime, tz)

instance GitObject Tag where
  objectType _ = ObjTyTag
  encodeObject = undefined
  objectParser = undefined

lazyParseOnly :: MonadFail m => Parser a -> LBS.ByteString -> m a
lazyParseOnly p bs = case parse p bs of
    Fail _ [] err -> fail err
    Fail _ ctxs err -> fail $ intercalate " > " ctxs ++ ": " ++ err
    Done _ r -> return r

tellParsePos :: Parser Int
tellParsePos = ApIntern.Parser $ \t pos more _lose success ->
  success t pos more (ApIntern.fromPos pos)

takeTill' :: (Char -> Bool) -> Parser BS.ByteString
takeTill' p = takeTill p <* anyWord8

char_ :: Char -> Parser ()
char_ = void . char

satisfyMap :: String -> (Char -> Maybe a) -> Parser a
satisfyMap errStr f = anyChar >>= maybe (fail errStr) return . f

sha1HexParser :: Parser Sha1
sha1HexParser = take Sha1.sha1HexSize >>= Sha1.fromHexByteString

oct :: Parser Int
oct = numberValue <$> many1 (satisfyMap "digit" digitToInt) <?> "octal"
  where
    digitToInt c = let dig = ord c - ord '0' in
      if (fromIntegral dig :: Word) < 8 then Just dig else Nothing
    numberValue = foldl' (\acc -> ((8 * acc) +)) 0


b :: BS.ByteString -> Builder
b = Builder.fromByteString

bt :: Text -> Builder
bt = b . encodeUtf8

toOctBS :: Integral a => a -> BS.ByteString
toOctBS = BS.pack . fmap (fromIntegral . wordToDigit) . toWords
  where
    wordToDigit = (+ (fromIntegral $ ord '0'))
    toWords = go []
    go acc o = let (q, r) = quotRem o 8 in
      case q of
        0 -> r : acc
        _ -> if q < 8
                then q : r : acc
                else go (r : acc) q
