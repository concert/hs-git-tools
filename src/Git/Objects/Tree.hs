{-# LANGUAGE
    DataKinds
#-}

module Git.Objects.Tree where

import qualified Blaze.ByteString.Builder as Builder
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8 (char)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Char (ord)
import qualified Data.Map as Map
import Data.Word
import qualified System.Path as Path

import Git.Internal (nullTermStringP, oct, takeFor)
import Git.Objects.Internal (TreeRow(..), Tree, Object(..))
import Git.Sha1 (sha1ByteStringParser)
import qualified Git.Sha1 as Sha1
import Git.Types (fileModeToInt, fileModeFromInt, checkPath)


encodeTree :: Tree -> BS.ByteString
encodeTree =
      Builder.toByteString . mconcat . fmap rowB . Map.toList . unTree
    where
      b = Builder.fromByteString
      sha1ByteStringB = b . Sha1.unSha1
      fileModeB = b . toOctBS . fileModeToInt
      nameB = b . Char8.pack . Path.toString
      rowB (name, TreeRow mode sha1) =
        fileModeB mode <> b " " <> nameB name <> b "\NUL"
        <> sha1ByteStringB sha1

treeParser :: Word64 -> Parser Tree
treeParser size = Tree . Map.fromList <$> takeFor (fromIntegral size) rowP
  where
    rowP = do
      fileMode <- (oct >>= fileModeFromInt) <* char ' '
      name <- Path.rel <$> nullTermStringP
      checkPath name
      sha1 <- sha1ByteStringParser
      return (name, TreeRow fileMode sha1)

toOctBS :: Integral a => a -> BS.ByteString
toOctBS = BS.pack . fmap (fromIntegral . wordToDigit) . toWords
  where
    wordToDigit = (+ (fromIntegral $ ord '0'))
    toWords = go []
    go acc o = let (q, r) = quotRem o 8 in
      case q of
        0 -> r : acc
        _ -> if q < 8
                then q : r : acc
                else go (r : acc) q
