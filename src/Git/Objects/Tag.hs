{-# LANGUAGE
    DataKinds
#-}

module Git.Objects.Tag where

import Data.Attoparsec.ByteString (Parser)
import qualified Data.ByteString as BS
import Data.Word

import Git.Objects.Internal (Tag)

encodeTag :: Tag -> BS.ByteString
encodeTag = undefined

tagParser :: Word64 -> Parser Tag
tagParser = undefined
