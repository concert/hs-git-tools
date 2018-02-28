module Git.Serialise where

import Prelude hiding (fail)

import Control.Monad (void, unless)
import Control.Monad.Fail (MonadFail(..))
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import Data.Attoparsec.ByteString.Lazy
  ( Parser, Result(..), parse, string, word8, endOfInput, takeTill
  , takeLazyByteString)
import qualified Data.Attoparsec.Internal.Types as ApIntern
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Data.Word
import System.Posix (getFileStatus, fileSize)
import System.Directory
import Text.Printf (printf)
import Text.Read (readMaybe)

import Git.Types (Blob(..))
import Git.Types.SizedByteString (SizedByteString)
import qualified Git.Types.SizedByteString as SBS

storeObject :: GitObject a => FilePath -> a -> IO ()
storeObject storePath obj = do undefined

space :: Parser ()
space = void $ word8 32

nullByte :: Parser ()
nullByte = void $ word8 0

asciiToInt :: MonadFail m => BS.ByteString -> m Int
asciiToInt "" = fail "asciiToInt: Empty ByteString"
asciiToInt bs = maybe (fail "asciiToint: parse error") return $ readMaybe
  $ Char8.unpack bs


class GitObject a where
  encodeObject :: a -> SizedByteString
  encodeObject obj = header <> objectBody obj
    where
      header = SBS.fromStrictByteString $
           (objectName $ Proxy @a)
        <> (Char8.pack $ show $ SBS.length $ objectBody obj)
        <> (BS.singleton 0)

  decodeObject :: MonadFail m => SizedByteString -> m a
  decodeObject sbs =
      lazyParseOnly ((parseHeader >>= parseBody) <* endOfInput) $
      SBS.toLazyByteString sbs
    where
      parseHeader :: Parser (Int, Int)
      parseHeader = do
        void $ string $ objectName $ Proxy @a
        recordedSize <- takeTill (== 0) >>= asciiToInt
        nullByte
        headerSize <- tellParsePos
        return (headerSize, recordedSize)
      parseBody :: (Int, Int) -> Parser a
      parseBody (headerSize, recordedSize) =
        let actualSize = SBS.length sbs - headerSize in do
          unless (recordedSize == actualSize) $ fail $ printf
            "Incorrect header size: expected %d bytes, actual file was %d bytes"
            recordedSize actualSize
          objectParser actualSize

  objectName :: proxy a -> BS.ByteString
  objectBody :: a -> SizedByteString
  objectParser :: Int -> Parser a

instance GitObject Blob where
  objectName _ = "blob"
  objectBody = blobData
  objectParser size = Blob . SBS.fromStrictByteString . LBS.toStrict
    <$> takeLazyByteString

lazyParseOnly :: MonadFail m => Parser a -> LBS.ByteString -> m a
lazyParseOnly p bs = case parse p bs of
    Fail _ [] err -> fail err
    Fail _ ctxs err -> fail $ intercalate " > " ctxs ++ ": " ++ err
    Done _ r -> return r

tellParsePos :: Parser Int
tellParsePos = ApIntern.Parser $ \t pos more _lose success ->
  success t pos more (ApIntern.fromPos pos)
