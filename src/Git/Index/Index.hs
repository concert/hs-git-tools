module Git.Index.Index where

import qualified Data.Map as Map
import qualified System.Path as Path
import Data.Word

import Git.Index.Extensions
  (IndexExtension(..), CachedTree(..), ResolveUndo(..))
import Git.Index.Types
  (IndexVersion, IndexEntries, IndexEntry(..), GitFileStat, Stages, stagesToMap)

data Index
  = Index
  { indexVersion :: IndexVersion
  , indexEntries :: IndexEntries
  -- Extensions:
  , exCachedTree :: CachedTree
  , exResolveUndo :: ResolveUndo
  } deriving Show

index :: IndexVersion -> Index
index v = Index v mempty extEmpty extEmpty

indexLookup :: Path.RelFileDir -> Index -> Maybe (Stages IndexEntry)
indexLookup p = Map.lookup p . indexEntries

gfsFromIndex :: Path.RelFileDir -> Index -> Maybe (Stages GitFileStat)
gfsFromIndex p idx = fmap ieGfs <$> Map.lookup p (indexEntries idx)

numEntries :: Index -> Word32
numEntries = fromIntegral . sum . fmap (Map.size . stagesToMap) . indexEntries
