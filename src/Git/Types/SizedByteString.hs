module Git.Types.SizedByteString
  ( SizedByteString, fromStrictByteString, fromHandle, length
  , toLazyByteString, fromLazyByteStringOfKnownLength)
where

import Prelude hiding (length)
import Data.Monoid ((<>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.IO (Handle, hSeek, SeekMode(..), hTell)

data SizedByteString
  = SizedByteString
  { sbsLength :: Integer
  , sbsBytes :: LBS.ByteString}

instance Monoid SizedByteString where
  mempty = SizedByteString 0 mempty
  SizedByteString l1 bs1 `mappend` SizedByteString l2 bs2 =
    SizedByteString (l1 + l2) (bs1 <> bs2)

fromStrictByteString :: BS.ByteString -> SizedByteString
fromStrictByteString bs = SizedByteString
  (fromIntegral $ BS.length bs) (LBS.fromStrict bs)

-- | This is an "escape hatch" where you can promise that you really do know how
--   long the LazyByteString is, and this SizedByteString will just track it.
fromLazyByteStringOfKnownLength :: Integer -> LBS.ByteString -> SizedByteString
fromLazyByteStringOfKnownLength = SizedByteString

-- | If the file handle is closed, reading from the ByteString will fail, just
--   like a regular LazyByteString.
fromHandle :: Handle -> IO SizedByteString
fromHandle h = SizedByteString <$> getFileSize h <*> LBS.hGetContents h

length :: SizedByteString -> Integer
length = sbsLength

toLazyByteString :: SizedByteString -> LBS.ByteString
toLazyByteString = sbsBytes

getFileSize :: Handle -> IO Integer
getFileSize h = do
  initialPos <- hTell h
  sizeBytes <- hSeek h SeekFromEnd 0 >> hTell h
  hSeek h AbsoluteSeek initialPos
  return sizeBytes
