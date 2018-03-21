module Git.Types.SizedByteString
  ( SizedByteString, fromStrictByteString, fromHandle, length
  , toLazyByteString, takeFromLazyByteString, take, drop)
where

import Prelude hiding (length, take, drop)
import Data.Int
import Data.Monoid ((<>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.String (IsString(..))
import Data.Word
import System.IO (Handle, hSeek, SeekMode(..), hTell)
import Text.Printf (printf)

data SizedByteString
  = SizedByteString
  { sbsLength :: Word64
  , sbsBytes :: LBS.ByteString
  } deriving Eq

instance Show SizedByteString where
  show = printf "<SizedByteString: %d>" . sbsLength

instance IsString SizedByteString where
  fromString = fromStrictByteString . fromString

instance Monoid SizedByteString where
  mempty = SizedByteString 0 mempty
  SizedByteString l1 bs1 `mappend` SizedByteString l2 bs2 =
    SizedByteString (l1 + l2) (bs1 <> bs2)

fromStrictByteString :: BS.ByteString -> SizedByteString
fromStrictByteString bs = SizedByteString
  (fromIntegral $ BS.length bs) (LBS.fromStrict bs)

takeFromLazyByteString :: Word64 -> LBS.ByteString -> SizedByteString
takeFromLazyByteString size =
  SizedByteString size . LBS.take (fromIntegral size)

-- | If the file handle is closed, reading from the ByteString will fail, just
--   like a regular LazyByteString.
fromHandle :: Handle -> IO SizedByteString
fromHandle h = SizedByteString
  <$> (fmap fromIntegral $ getRemainingFileSize h)
  <*> LBS.hGetContents h

length :: SizedByteString -> Word64
length = sbsLength

toLazyByteString :: SizedByteString -> LBS.ByteString
toLazyByteString = sbsBytes

getRemainingFileSize :: Handle -> IO Integer
getRemainingFileSize h = do
  initialPos <- hTell h
  sizeBytes <- hSeek h SeekFromEnd 0 >> hTell h
  hSeek h AbsoluteSeek initialPos
  return sizeBytes

take :: Int64 -> SizedByteString -> SizedByteString
take i (SizedByteString l lbs) =
  SizedByteString (fromIntegral $ min i $ fromIntegral l) $ LBS.take i lbs

drop :: Int64 -> SizedByteString -> SizedByteString
drop i (SizedByteString l lbs) =
  SizedByteString (fromIntegral $ max 0 $ fromIntegral l - i) $ LBS.drop i lbs
