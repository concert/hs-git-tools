module Git.Serialise
  ( encodeLooseObject, encodeObject
  , decodeLooseObject, decodeObject
  , GitObject, objectType, wrap, unwrap
  , tellParsePos
  , lazyParseOnly, parseSbs
  , sha1HexParser
  ) where

import Prelude hiding (fail, take)

import Control.Monad (void)
import Control.Monad.Fail (MonadFail(..))
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import Data.Attoparsec.ByteString.Char8
  (char, signed, takeTill, anyChar, decimal, isDigit, satisfy)
import Data.Attoparsec.ByteString.Lazy
  ( Parser, Result(..), parse, string, choice, endOfInput
  , takeLazyByteString, many', many1, take, anyWord8, (<?>))
import qualified Data.Attoparsec.Internal.Types as ApIntern
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord, digitToInt)
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
import System.Posix (CMode(..))
import Text.Printf (printf)

import Git.Types
  (Sha1, Blob(..), Tree(..), TreeRow(..), Commit(..), Tag(..), toZonedTime
  , ObjectType(..), Object(..))
import qualified Git.Types.Sha1 as Sha1
import Git.Types.SizedByteString (SizedByteString)
import qualified Git.Types.SizedByteString as SBS

encodeObjectType :: IsString str => ObjectType -> str
encodeObjectType objTy = case objTy of
  ObjTyBlob -> "blob"
  ObjTyTree -> "tree"
  ObjTyCommit -> "commit"
  ObjTyTag -> "tag"

encodeLooseObject
  :: forall a. GitObject a => a -> (Tagged a Sha1, SizedByteString)
encodeLooseObject obj =
  let
    body = encodeObject obj
    encodedWithHeader =
        encodeObjectType (objectType $ Proxy @a) <> " "
        <> (SBS.fromStrictByteString $ Char8.pack $ show $ SBS.length body)
        <> "\NUL" <> body
    -- FIXME: are we concerned about encoding large files using too much memory?
    bodySha1 = Tagged $ Sha1.hashSbs body
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

decodeObject :: (MonadFail m, GitObject a) => SizedByteString -> m a
decodeObject sbs = parseSbs (objectParser (SBS.length sbs) <* endOfInput) sbs


class GitObject a where
  objectType :: proxy a -> ObjectType
  encodeObject :: a -> SizedByteString
  objectParser :: Word64 -> Parser a

  wrap :: a -> Object
  unwrap :: MonadFail m => Object -> m a

instance GitObject Blob where
  objectType _ = ObjTyBlob
  encodeObject = blobData
  objectParser size =
    Blob . SBS.takeFromLazyByteString size <$> takeLazyByteString
  wrap = ObjBlob
  unwrap (ObjBlob blob) = return blob
  unwrap _ = fail "Incorrect object type"

sha1ByteStringP :: Parser Sha1
sha1ByteStringP = take 20 >>= Sha1.fromByteString

instance GitObject Tree where
  objectType _ = ObjTyTree
  encodeObject = SBS.fromStrictByteString . Builder.toByteString .
      mconcat . fmap rowB . unTree
    where
      sha1ByteStringB = b . Sha1.unSha1
      fileModeB (CMode o) = b $ toOctBS o
      nameB = b . encodeUtf8
      rowB (TreeRow mode name sha1) =
        fileModeB mode <> nameB name <> sha1ByteStringB sha1
  objectParser _ = Tree <$> many1 rowP
    where
      fileModeP = CMode . fromIntegral <$> oct
      nameP = decodeUtf8 <$> takeTill' (== '\NUL')
      rowP = TreeRow <$> (fileModeP <* char ' ') <*> nameP <*> sha1ByteStringP
  wrap = ObjTree
  unwrap (ObjTree t) = return t
  unwrap _ = fail "Incorrect object type"


instance GitObject Commit where
  objectType _ = ObjTyCommit
  encodeObject c = metadata <> "\n" <> commitMsg c
    where
      metadata = SBS.fromStrictByteString $ Builder.toByteString $
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
      treeSha1 <- treeRowP
      parentSha1s <- many' parentRowP
      (authName, authEmail, authAt, authTz) <- contributorRowP "author"
      (commName, commEmail, commAt, commTz) <- contributorRowP "committer"
      char_ '\n'
      pos <- tellParsePos
      msg <- SBS.takeFromLazyByteString (size - fromIntegral  pos) <$>
        takeLazyByteString
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
  wrap = ObjCommit
  unwrap (ObjCommit c) = return c
  unwrap _ = fail "Incorrect object type"

instance GitObject Tag where
  objectType _ = ObjTyTag
  encodeObject = undefined
  objectParser = undefined
  wrap = ObjTag
  unwrap (ObjTag t) = return t
  unwrap _ = fail "Incorrect object type"

lazyParseOnly :: MonadFail m => Parser a -> LBS.ByteString -> m a
lazyParseOnly p bs = case parse p bs of
    Fail _ [] err -> fail err
    Fail _ ctxs err -> fail $ intercalate " > " ctxs ++ ": " ++ err
    Done _ r -> return r

tellParsePos :: Parser Int
tellParsePos = ApIntern.Parser $ \t pos more _lose success ->
  success t pos more (ApIntern.fromPos pos)

parseSbs :: MonadFail m => Parser a -> SizedByteString -> m a
parseSbs p = lazyParseOnly p . SBS.toLazyByteString

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
