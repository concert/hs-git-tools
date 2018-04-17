module Git.Index.Extensions.Class where

import Prelude hiding (take)

import Control.Monad (replicateM, void)
import Data.Attoparsec.ByteString (Parser, string, take)
import Data.Attoparsec.ByteString.Char8 (satisfy)
import Data.Attoparsec.Binary (anyWord32be)
import qualified Data.ByteString as BS
import Data.Char (isUpper)
import Data.Proxy


class IndexExtension a where
  extSignature :: Proxy a -> BS.ByteString
  extParser :: Parser a

extensionP :: forall a. IndexExtension a => Parser a
extensionP = do
  _ <- string $ extSignature $ Proxy @a
  _ <- anyWord32be
  extParser

voidExtensionP :: Parser ()
voidExtensionP = do
  _ <- replicateM 4 $ satisfy isUpper
  size <- anyWord32be
  void $ take $ fromIntegral size
