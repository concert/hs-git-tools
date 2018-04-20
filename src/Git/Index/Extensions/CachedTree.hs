module Git.Index.Extensions.CachedTree where

import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word
import qualified System.Path as Path

import Git.Index.Extensions.Class
  (IndexExtension(..), BuildableIndexExtension(..))
import Git.Internal (takeFor, nullTermStringP, char_)
import Git.Sha1 (Sha1, sha1HexParser)


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
        sha1 <- sha1HexParser
        return $ (path, CachedTreeRow entryCount subtreeCount sha1)

instance BuildableIndexExtension CachedTree where
  extBuilder = undefined
  extEmpty = Map.null . unCachedTree
