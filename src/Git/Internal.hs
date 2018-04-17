{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
#-}

module Git.Internal where

import Prelude hiding (fail)

import Control.Monad (void)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Except (ExceptT(..), MonadError, throwError, runExceptT)
import Data.Attoparsec.ByteString (Parser, parseOnly, anyWord8, many1, (<?>))
import Data.Attoparsec.ByteString.Char8 (char, anyChar, takeTill)
import Data.Attoparsec.ByteString.Lazy (Result(..), parse)
import Data.Attoparsec.Binary (anyWord32be, anyWord64be)
import qualified Data.Attoparsec.Internal.Types as ApIntern
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSIntern
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (TimeZone, ZonedTime, utcToZonedTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Word
import Foreign.ForeignPtr (ForeignPtr)

import Git.Types.Error (GitError(..))
import Git.Sha1 (Sha1)
import qualified Git.Sha1 as Sha1

instance MonadFail (Either String) where
  fail = Left

-- FIXME: replace with Control.Monad.Except.liftEither when >= 2.2.2
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return

liftEitherGitError :: MonadError GitError m => Either String a -> m a
liftEitherGitError = liftEither . first GenericError

firstSuccess
  :: Monad m => (a -> ExceptT e m b) -> [a] -> ExceptT [(a, e)] m (a, b)
firstSuccess f as = ExceptT $ go (fmap (\a -> (a, f a)) as) []
  where
    go
      :: Monad m => [(a, ExceptT e m b)] -> [(a, e)] ->  m (Either [(a, e)] (a, b))
    go [] aes = return $ Left aes
    go ((a, exT):aexTs) aes = runExceptT exT >>=
      either (go aexTs . (: aes) . (a,)) (return . Right . (a,))

splitLength :: [a] -> [b] -> ([b], [b])
splitLength as bs = first reverse $ go [] as bs
  where
    go prefix [] ys = (prefix, ys)
    go prefix _ [] = (prefix, [])
    go prefix (_:xs) (y:ys) = go (y : prefix) xs ys

dropLengthMaybe :: MonadFail m => [a] -> [b] -> m [b]
dropLengthMaybe [] bs = return bs
dropLengthMaybe _ [] = fail "Ran out of bs"
dropLengthMaybe (_:as) (_:bs) = dropLengthMaybe as bs

type MmapHandle = (ForeignPtr Word8, Int, Int)

data MmapFrom = FromStart Int | FromEnd Int deriving (Show)
data MmapTo = Length Int | ToEnd deriving (Show)

mmapData :: MmapHandle -> MmapFrom -> MmapTo -> BS.ByteString
mmapData (ptr, mmapOfs, mmapSize) from to =
  let
    startOfs = case from of
      FromStart i -> mmapOfs + i
      FromEnd i -> mmapOfs + mmapSize - i
    len = case to of
      Length i -> i
      ToEnd -> mmapOfs + mmapSize - startOfs
  in
    BSIntern.fromForeignPtr ptr startOfs len

mmapWord32be :: MmapHandle -> MmapFrom -> Word32
mmapWord32be h from = either error id $ parseOnly anyWord32be $
  mmapData h from (Length 4)

mmapWord64be :: MmapHandle -> MmapFrom -> Word64
mmapWord64be h from = either error id $ parseOnly anyWord64be $
  mmapData h from (Length 8)

mmapSha1 :: MmapHandle -> MmapFrom -> Sha1
mmapSha1 h from = either error id $ Sha1.fromByteString $
  mmapData h from (Length 20)

class Wrapable w a where
  wrap :: a -> w
  unwrap :: MonadFail m => w -> m a

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

nullTermStringP :: Parser Text
nullTermStringP = decodeUtf8 <$> takeTill' (== '\NUL')

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

toZonedTime :: POSIXTime -> TimeZone -> ZonedTime
toZonedTime pt tz = utcToZonedTime tz $ posixSecondsToUTCTime pt
