module Git.Serialise where

import Prelude hiding (fail, take)

import Control.Monad (void)
import Control.Monad.Fail (MonadFail(..))
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import Data.Attoparsec.ByteString.Char8 (char, decimal, signed, takeTill)
import Data.Attoparsec.ByteString.Lazy
  ( Parser, Result(..), parse, string, endOfInput
  , takeLazyByteString, many', take, anyWord8, (<?>))
import qualified Data.Attoparsec.Internal.Types as ApIntern
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (minutesToTimeZone, timeZoneMinutes)
import Text.Printf (printf)

import Git.Types (Blob(..), Commit(..))
import qualified Git.Types.Sha1 as Sha1
import Git.Types.SizedByteString (SizedByteString)
import qualified Git.Types.SizedByteString as SBS

class GitObject a where
  encodeObject :: a -> SizedByteString
  encodeObject obj = header <> objectBody obj
    where
      header = SBS.fromStrictByteString $
           (objectName $ Proxy @a)
        <> (Char8.pack $ show $ SBS.length $ objectBody obj)
        <> (BS.singleton 0)

  decodeObject :: MonadFail m => LBS.ByteString -> m a
  decodeObject = lazyParseOnly ((parseHeader >>= objectParser) <* endOfInput)
    where
      parseHeader :: Parser Integer
      parseHeader = do
        _ <- (string $ objectName $ Proxy @a) <?> "object name"
        _ <- char ' '
        decimal <* char '\NUL' <?> "object size"

  objectName :: proxy a -> BS.ByteString
  objectBody :: a -> SizedByteString
  objectParser :: Integer -> Parser a

instance GitObject Blob where
  objectName _ = "blob"
  objectBody = blobData
  objectParser size =
    Blob . SBS.takeFromLazyByteString size <$> takeLazyByteString


instance GitObject Commit where
  objectName _ = "commit"
  objectBody c = metadata <> commitMsg c
    where
      metadata = SBS.fromStrictByteString $ Builder.toByteString $
           sha1RowB "tree" (commitTreeHash c)
        <> mconcat (sha1RowB "parent" <$> commitParents c)
        <> contributorRowB "author" (commitAuthor c)
             (commitAuthorEmail c) (commitAuthoredAt c)
             (commitAuthoredTz c)
        <> contributorRowB "committer" (commitCommitter c)
             (commitCommitterEmail c) (commitCommittedAt c)
             (commitCommittedTz c)
      sha1StringB = b . Char8.pack . Sha1.toHexString
      sha1RowB role sha1 = b role <> b " " <> sha1StringB sha1 <> b "\n"
      contributorRowB r n e t tz = b r <> b " " <> b (encodeUtf8 n) <> b "<"
        <> b (encodeUtf8 e) <> b ">" <> iB t <> tzB tz
      iB :: Show a => a -> Builder
      iB = b . encodeUtf8 . Text.pack . show
      tzB tz = let m = timeZoneMinutes tz in
        (if m < 0 then b "-" else b "+") <> iB m
  objectParser size = do
      treeSha1 <- treeRowP
      parentSha1s <- many' parentRowP
      (authName, authEmail, authAt, authTz) <- contributorRowP "author"
      (commName, commEmail, commAt, commTz) <- contributorRowP "committer"
      _ <- char '\n'
      msg <- SBS.takeFromLazyByteString size <$> takeLazyByteString
      return $ Commit
        treeSha1
        parentSha1s
        authName authEmail authAt authTz
        commName commEmail commAt commTz
        msg
    where
      sha1StringP = take 40 >>= Sha1.fromHexString . Char8.unpack
      sha1RowP role = string role >> char ' ' >> sha1StringP <* char '\n'
      treeRowP = sha1RowP "tree"
      parentRowP = sha1RowP "parent"
      emailP = char '<' >> takeTill' (== '>')
      contributorRowP role = do
        _ <- string role
        _ <- char ' '
        name <- decodeUtf8 . BS.init <$> takeTill (== '<')
        email <- decodeUtf8 <$> emailP
        _ <- char ' '
        posixTime <- fromIntegral @Integer <$> decimal
        _ <- char ' '
        tz <- minutesToTimeZone <$> signed decimal
        _ <- char '\n'
        return (name, email, posixTime, tz)

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

char' :: Char -> Parser ()
char' = void . char

b :: BS.ByteString -> Builder
b = Builder.fromByteString
