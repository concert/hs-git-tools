module Git.Types.Sha1
  ( Sha1, unSha1, fromByteString
  , toHexString, fromHexString
  , hashLazy
  ) where

import Prelude hiding (fail)

import Control.Monad.Fail (MonadFail(..))
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS

newtype Sha1 = Sha1 {unSha1 :: BS.ByteString}

instance Show Sha1 where
  show sha1 = "sha1: " ++ toHexString sha1

fromByteString :: MonadFail m => BS.ByteString -> m Sha1
fromByteString bs | BS.length bs == 20 = return $ Sha1 bs
                  | otherwise = fail "sha1: fromByteString: bad digest length"

toHexString :: Sha1 -> String
toHexString = Char8.unpack . Base16.encode . unSha1

fromHexString :: MonadFail m => String -> m Sha1
fromHexString s = case Base16.decode $ Char8.pack s of
  (bs, "") -> fromByteString bs
  _ -> fail $ "sha1: fromHexString: decodeError"


hashLazy :: LBS.ByteString -> Sha1
hashLazy = Sha1 . SHA1.hashlazy
