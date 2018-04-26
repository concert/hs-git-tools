module Git.Index.Extensions.Class where

import Prelude hiding (take)

import Blaze.ByteString.Builder
  (Builder, fromByteString, fromWord32be, toByteString)
import Control.Monad (replicateM_, void)
import Data.Attoparsec.ByteString (Parser, take, string)
import Data.Attoparsec.ByteString.Char8 (satisfy)
import Data.Attoparsec.Binary (anyWord32be)
import qualified Data.ByteString as BS
import Data.Char (isUpper)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Word


class IndexExtension a where
  extSignatureParser :: Proxy a -> Parser ()
  extSignatureParser p = void $ string $ extSignature p

  extSignature :: Proxy a -> BS.ByteString
  extEmpty :: a
  extParser :: Word32 -> Parser a

class IndexExtension a => BuildableIndexExtension a where
  extBuilder :: a -> Builder
  extNull :: a -> Bool

extensionP :: forall a. IndexExtension a => Parser a
extensionP = do
  extSignatureParser $ Proxy @a
  size <- anyWord32be
  extParser size

-- | An IndexExtension parser for an extension we don't recognise
instance IndexExtension () where
  extSignatureParser _ = replicateM_ 4 $ satisfy isUpper
  extSignature _ = ""  -- Unused
  extEmpty = ()
  extParser size = void $ take $ fromIntegral size

extensionB :: forall a. BuildableIndexExtension a => a -> Builder
extensionB ext =
    if extNull ext then mempty else
    fromByteString (extSignature $ Proxy @a) <> withLength extBytes
  where
    withLength :: BS.ByteString -> Builder
    withLength bs = fromWord32be (fromIntegral $ BS.length bs)
      <> fromByteString bs
    extBytes = toByteString $ extBuilder ext
