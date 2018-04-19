module Git.Index.Extensions.Class where

import Prelude hiding (take)

import Control.Monad (replicateM_, void)
import Data.Attoparsec.ByteString (Parser, take, string)
import Data.Attoparsec.ByteString.Char8 (satisfy)
import Data.Attoparsec.Binary (anyWord32be)
import qualified Data.ByteString as BS
import Data.Char (isUpper)
import Data.Proxy
import Data.Word


class IndexExtension a where
  extSignatureParser :: Proxy a -> Parser ()
  extSignatureParser p = void $ string $ extSignature p

  extSignature :: Proxy a -> BS.ByteString
  extParser :: Word32 -> Parser a

extensionP :: forall a. IndexExtension a => Parser a
extensionP = do
  extSignatureParser $ Proxy @a
  size <- anyWord32be
  extParser size

-- | An IndexExtension parser for an extension we don't recognise
instance IndexExtension () where
  extSignatureParser _ = replicateM_ 4 $ satisfy isUpper
  extSignature _ = ""  -- Unused
  extParser size = void $ take $ fromIntegral size
