module Git.Index.Index where

import qualified Data.Map as Map
import qualified System.Path as Path

import Git.Index.Extensions (CachedTree(..), ResolveUndo(..))
import Git.Index.Types
  (IndexVersion, IndexEntries, IndexEntry(..), GitFileStat, Stages)

data Index
  = Index
  { indexVersion :: IndexVersion
  , indexEntries :: IndexEntries
  -- Extensions:
  , exCachedTree :: CachedTree
  , exResolveUndo :: ResolveUndo
  } deriving Show

index :: IndexVersion -> Index
index v = Index v mempty (CachedTree mempty) (ResolveUndo mempty)

indexLookup :: Path.RelFileDir -> Index -> Maybe (Stages IndexEntry)
indexLookup p = Map.lookup p . indexEntries

statFromIndex :: Path.RelFileDir -> Index -> Maybe (Stages GitFileStat)
statFromIndex p idx = fmap ieGfs <$> Map.lookup p (indexEntries idx)
