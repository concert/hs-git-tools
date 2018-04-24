module Git.Index.Extensions.CachedTree where

import Blaze.ByteString.Builder (fromByteString)
import Data.Attoparsec.ByteString.Char8 (decimal)
import qualified Data.ByteString.Char8 as Char8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Word
import qualified System.Path as Path
import Text.Printf (printf)

import Git.Index.Extensions.Class
  (IndexExtension(..), BuildableIndexExtension(..))
import Git.Internal (takeFor, nullTermStringP, char_)
import Git.Sha1 (Sha1(..), sha1ByteStringParser)


newtype CachedTree
  = CachedTree
  { unCachedTree :: Map Path.RelFileDir CachedTreeRow
  } deriving (Show)

data CachedTreeRow
  = CachedTreeRow
  { ctrEntryCount :: Word32
  , ctrSubtreeCount :: Word32
  , ctrSha1 :: Sha1
  } deriving (Show)

instance IndexExtension CachedTree where
  extSignature _ = "TREE"
  extEmpty = CachedTree mempty
  extParser size = do
      rows <- takeFor (fromIntegral size) entryP
      return $ CachedTree $ Map.fromList rows
    where
      entryP = do
        path <- Path.rel <$> nullTermStringP
        entryCount <- decimal
        char_ ' '
        subtreeCount <- decimal
        char_ '\n'
        sha1 <- sha1ByteStringParser
        return $ (path, CachedTreeRow entryCount subtreeCount sha1)

instance BuildableIndexExtension CachedTree where
  extBuilder = Map.foldMapWithKey f . unCachedTree
    where
      b = fromByteString
      p = b . Char8.pack . (++ "\n") . Path.toString
      d = b . Char8.pack . printf "%d"
      f path (CachedTreeRow ec sc sha1) =
        p path <> d ec <> b " " <> d sc <> b "\n" <> b (unSha1 sha1)
  extNull = Map.null . unCachedTree
