module Git.Objects.Commit where

import Data.Attoparsec.ByteString.Char8
  ( Parser, signed, decimal, char, string, satisfy, takeByteString, many'
  , isDigit, takeTill)
import qualified Blaze.ByteString.Builder as Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Char (digitToInt)
import Data.Monoid ((<>))
import Data.Time
  (ZonedTime(..), zonedTimeToUTC, minutesToTimeZone, timeZoneMinutes)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as Text
import Text.Printf (printf)

import Git.Internal (tellParsePos, takeTill', char_, toZonedTime)
import Git.Objects.GitObject (GitObject(..), ObjectType(..))
import Git.Sha1 (Sha1, sha1HexParser)
import qualified Git.Sha1 as Sha1


data Commit = Commit
  { commitTreeHash :: Sha1
  , commitParents :: [Sha1]
  , commitAuthor :: Text
  , commitAuthorEmail :: Text
  , commitAuthoredAt :: ZonedTime
  , commitCommitter :: Text
  , commitCommitterEmail :: Text
  , commitCommittedAt :: ZonedTime
  , commitMsg :: BS.ByteString
  }

instance Show Commit where
  show c = printf "<commit: %s <%s> %s>"
    (Text.unpack $ commitAuthor c)
    (Text.unpack $ commitAuthorEmail c)
    (show $ commitAuthoredAt c)

instance GitObject Commit where
  gitObjectType _ = ObjTyCommit
  encodeObject c = metadata <> "\n" <> commitMsg c
    where
      b = Builder.fromByteString
      bt = b . encodeUtf8
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
