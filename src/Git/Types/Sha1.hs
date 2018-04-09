module Git.Types.Sha1
  ( Sha1, unsafeSha1, unSha1, sha1Size, fromByteString
  , sha1HexSize
  , toHexByteString, fromHexByteString
  , toHexString, fromHexString
  , toHexText, fromHexText
  , hashLazy, hashSbs
  ) where

import Prelude hiding (fail)

import Control.Monad.Fail (MonadFail(..))
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Git.Types.SizedByteString (SizedByteString)
import qualified Git.Types.SizedByteString as SBS

newtype Sha1 = Sha1 {unSha1 :: BS.ByteString} deriving (Eq, Ord)

unsafeSha1 :: BS.ByteString -> Sha1
unsafeSha1 = Sha1

instance Show Sha1 where
  show sha1 = "sha1: " ++ toHexString sha1

sha1Size :: Int
sha1Size = 20

sha1HexSize :: Int
sha1HexSize = 40

fromByteString :: MonadFail m => BS.ByteString -> m Sha1
fromByteString bs | BS.length bs == sha1Size = return $ Sha1 bs
                  | otherwise = fail "sha1: fromByteString: bad digest length"

fromHexByteString :: MonadFail m => BS.ByteString -> m Sha1
fromHexByteString bs = case Base16.decode bs of
  (bs', "") -> fromByteString bs'
  _ -> fail $ "sha1: fromHexString: decodeError"

toHexByteString :: Sha1 -> BS.ByteString
toHexByteString = Base16.encode . unSha1

fromHexString :: MonadFail m => String -> m Sha1
fromHexString = fromHexByteString . Char8.pack

toHexString :: Sha1 -> String
toHexString = Char8.unpack . toHexByteString

fromHexText :: MonadFail m => Text -> m Sha1
fromHexText = fromHexByteString . encodeUtf8

toHexText :: Sha1 -> Text
toHexText = decodeUtf8 . toHexByteString

hashLazy :: LBS.ByteString -> Sha1
hashLazy = Sha1 . SHA1.hashlazy

hashSbs :: SizedByteString -> Sha1
hashSbs = hashLazy . SBS.toLazyByteString
