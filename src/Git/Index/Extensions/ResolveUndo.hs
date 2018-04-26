{-# LANGUAGE
    DeriveFunctor
#-}

module Git.Index.Extensions.ResolveUndo where

import Prelude hiding (fail)

import Blaze.ByteString.Builder (fromByteString)
import Control.Monad (replicateM, zipWithM)
import Control.Monad.Fail (MonadFail(..))
import Data.Attoparsec.ByteString (Parser)
import qualified Data.ByteString.Char8 as Char8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid
import qualified System.Path as Path
import Text.Printf (printf)

import Git.Index.Extensions.Class
  (IndexExtension(..), BuildableIndexExtension(..))
import Git.Index.Types (Stages(..), mapToStages, stagesToMap)
import Git.Internal (takeFor, nullTermStringP, oct, char_)
import Git.Sha1 (Sha1(..), sha1ByteStringParser)
import Git.Types (FileMode, fileModeFromInt, fileModeToInt)


newtype ResolveUndo
  = ResolveUndo
  { unResolveUndo :: Map Path.RelFileDir (ResolveUndoStages (FileMode, Sha1))
  } deriving (Show)

data ResolveUndoStages a
  = RuBothAdded {ruSsHead :: a, ruSsIncoming :: a}
  | RuBothEdited {ruSsBase :: a, ruSsHead :: a, ruSsIncoming :: a}
  | RuRmEdited {ruSsBase :: a, ruSsIncoming :: a}
  | RuEditedRm {ruSsBase :: a, ruSsHead :: a}
  deriving (Show, Functor)

toStages :: ResolveUndoStages a -> Stages a
toStages ruSs = case ruSs of
  RuBothAdded hd inc -> BothAdded hd inc
  RuBothEdited base hd inc -> BothEdited base hd inc
  RuRmEdited base inc -> RmEdited base inc
  RuEditedRm base hd -> EditedRm base hd

fromStages :: MonadFail m => Stages a -> m (ResolveUndoStages a)
fromStages ss = case ss of
  Normal _ -> fail "Normal not an allowed ResolveUndo stage"
  BothAdded hd inc -> return $ RuBothAdded hd inc
  BothEdited base hd inc -> return $ RuBothEdited base hd inc
  RmEdited base inc -> return $ RuRmEdited base inc
  EditedRm base hd -> return $ RuEditedRm base hd

instance IndexExtension ResolveUndo where
  extSignature _ = "REUC"
  extEmpty = ResolveUndo mempty
  extParser size = do
      entries <- takeFor (fromIntegral size) entryP
      return $ ResolveUndo $ Map.fromList entries
    where
      entryP :: Parser (Path.RelFileDir, ResolveUndoStages (FileMode, Sha1))
      entryP = do
        path <- Path.rel <$> nullTermStringP
        modes <- replicateM 3 (oct <* char_ '\NUL')
        stages <- zipWithM f [succ minBound..] modes >>=
          mapToStages . Map.fromList . catMaybes >>= fromStages
        return (path, stages)
      f _ 0 = return Nothing
      f stage i = do
        mode <- fileModeFromInt i
        sha1 <- sha1ByteStringParser
        return $ Just (stage, (mode, sha1))

instance BuildableIndexExtension ResolveUndo where
  extBuilder = Map.foldMapWithKey f . fmap (stagesToMap . toStages) . unResolveUndo
    where
      b = fromByteString
      p = b . Char8.pack . (++ "\NUL") . Path.toString
      f path stages = p path <> modes stages <> sha1s stages
      defaultModes = Map.fromList [(m, 0) | m <- [succ minBound..]]
      modes stages = mconcat $ fmap (b . Char8.pack . printf "%o\NUL" . snd) $
        Map.toAscList $ fmap (fileModeToInt . fst) stages <> defaultModes
      sha1s = mconcat . fmap (b . unSha1 . snd . snd) . Map.toAscList
  extNull = Map.null . unResolveUndo
