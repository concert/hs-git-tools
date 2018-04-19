module Git.Objects.Tree where

import qualified Blaze.ByteString.Builder as Builder
import Data.Attoparsec.ByteString.Char8 (char)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Git.Internal (nullTermStringP, oct, takeFor)
import Git.Objects.GitObject (GitObject(..), ObjectType(..))
import Git.Sha1 (Sha1, sha1ByteStringParser)
import qualified Git.Sha1 as Sha1
import Git.Types (FileMode(..), fileModeToInt, fileModeFromInt)


-- FIXME: the file name should perhaps be a ByteString, because I don't believe
-- an encoding is defined:
newtype Tree = Tree {unTree :: Map Text TreeRow} deriving (Show, Eq)

data TreeRow = TreeRow
  { treeRowMode :: FileMode
  , treeRowSha1 :: Sha1
  } deriving (Show, Eq)

instance GitObject Tree where
  gitObjectType _ = ObjTyTree
  encodeObject =
      Builder.toByteString . mconcat . fmap rowB . Map.toList . unTree
    where
      b = Builder.fromByteString
      sha1ByteStringB = b . Sha1.unSha1
      fileModeB = b . toOctBS . fileModeToInt
      nameB = b . encodeUtf8
      rowB (name, TreeRow mode sha1) =
        fileModeB mode <> b " " <> nameB name <> b "\NUL"
        <> sha1ByteStringB sha1
  objectParser size = Tree . Map.fromList <$> takeFor (fromIntegral size) rowP
    where
      rowP = do
        fileMode <- (oct >>= fileModeFromInt) <* char ' '
        name <- nullTermStringP
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
