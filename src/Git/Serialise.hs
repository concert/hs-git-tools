module Git.Serialise where

import Prelude hiding (fail, take)

import Control.Monad (void)
import Control.Monad.Fail (MonadFail(..))
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import Data.Attoparsec.ByteString.Char8
  (char, signed, takeTill, anyChar, decimal)
import Data.Attoparsec.ByteString.Lazy
  ( Parser, Result(..), parse, string, endOfInput
  , takeLazyByteString, many', many1, take, anyWord8, (<?>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Monoid ((<>))
import Data.List (intercalate, foldl')
import Data.Proxy (Proxy(..))
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
  (minutesToTimeZone, timeZoneMinutes, ZonedTime(..), zonedTimeToUTC)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Posix (CMode(..))

import Git.Types (Blob(..), Tree(..), TreeRow(..), Commit(..), toZonedTime)
import qualified Git.Types.Sha1 as Sha1
import Git.Types.SizedByteString (SizedByteString)
import qualified Git.Types.SizedByteString as SBS

class GitObject a where
  encodeObject :: a -> SizedByteString
  encodeObject obj = header <> objectBody obj
    where
      header = SBS.fromStrictByteString $
           (objectName $ Proxy @a)
        <> " "
        <> (Char8.pack $ show $ SBS.length $ objectBody obj)
        <> (BS.singleton 0)

  decodeObject :: MonadFail m => LBS.ByteString -> m a
  decodeObject = lazyParseOnly ((parseHeader >>= objectParser) <* endOfInput)
    where
      parseHeader :: Parser Integer
      parseHeader = do
        _ <- (string $ objectName $ Proxy @a) <?> "object name"
        char_ ' ' >> decimal <* char_ '\NUL' <?> "object size"

  objectName :: proxy a -> BS.ByteString
  objectBody :: a -> SizedByteString
  objectParser :: Integer -> Parser a

instance GitObject Blob where
  objectName _ = "blob"
  objectBody = blobData
  objectParser size =
    Blob . SBS.takeFromLazyByteString size <$> takeLazyByteString


instance GitObject Tree where
  objectName _ = "tree"
  objectBody = SBS.fromStrictByteString . Builder.toByteString .
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
      sha1ByteStringP = take 20 >>= Sha1.fromByteString
      rowP = TreeRow <$> (fileModeP <* char ' ') <*> nameP <*> sha1ByteStringP


instance GitObject Commit where
  objectName _ = "commit"
  objectBody c = metadata <> commitMsg c
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
      contributorRowB r n e t = b r <> b " " <> b (encodeUtf8 n) <> b "<"
        <> b (encodeUtf8 e) <> b ">" <> tB t
      iB :: Show a => a -> Builder
      iB = b . encodeUtf8 . Text.pack . show
      tB t = let m = timeZoneMinutes $ zonedTimeZone t in
           iB (utcTimeToPOSIXSeconds $ zonedTimeToUTC t)
        <> (if m < 0 then b "-" else b "+") <> iB m
  objectParser size = do
      treeSha1 <- treeRowP
      parentSha1s <- many' parentRowP
      (authName, authEmail, authAt, authTz) <- contributorRowP "author"
      (commName, commEmail, commAt, commTz) <- contributorRowP "committer"
      char_ '\n'
      msg <- SBS.takeFromLazyByteString size <$> takeLazyByteString
      return $ Commit
        treeSha1
        parentSha1s
        authName authEmail (toZonedTime authAt authTz)
        commName commEmail (toZonedTime commAt commTz)
        msg
    where
      sha1StringP = take 40 >>= Sha1.fromHexString . Char8.unpack
      sha1RowP role = string role >> char ' ' >> sha1StringP <* char '\n'
      treeRowP = sha1RowP "tree"
      parentRowP = sha1RowP "parent"
      emailP = char '<' >> takeTill' (== '>')
      contributorRowP role = do
        _ <- string role
        char_ ' '
        name <- decodeUtf8 . BS.init <$> takeTill (== '<')
        email <- decodeUtf8 <$> emailP
        char_ ' '
        posixTime <- fromIntegral @Int <$> decimal
        char_ ' '
        tz <- minutesToTimeZone <$> signed decimal
        char_ '\n'
        return (name, email, posixTime, tz)

lazyParseOnly :: MonadFail m => Parser a -> LBS.ByteString -> m a
lazyParseOnly p bs = case parse p bs of
    Fail _ [] err -> fail err
    Fail _ ctxs err -> fail $ intercalate " > " ctxs ++ ": " ++ err
    Done _ r -> return r

takeTill' :: (Char -> Bool) -> Parser BS.ByteString
takeTill' p = takeTill p <* anyWord8

char_ :: Char -> Parser ()
char_ = void . char

satisfyMap :: String -> (Char -> Maybe a) -> Parser a
satisfyMap errStr f = anyChar >>= maybe (fail errStr) return . f

oct :: Parser Int
oct = numberValue <$> many1 (satisfyMap "digit" digitToInt) <?> "octal"
  where
    digitToInt c = let dig = ord c - ord '0' in
      if (fromIntegral dig :: Word) < 8 then Just dig else Nothing
    numberValue = foldl' (\acc -> ((8 * acc) +)) 0


b :: BS.ByteString -> Builder
b = Builder.fromByteString

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
