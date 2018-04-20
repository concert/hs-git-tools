module Git.Index.Extensions.ResolveUndo where

import Blaze.ByteString.Builder (fromByteString)
import Control.Monad (replicateM, zipWithM)
import qualified Data.ByteString.Char8 as Char8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid
import qualified System.Path as Path
import Text.Printf (printf)

import Git.Index.Extensions.Class
  (IndexExtension(..), BuildableIndexExtension(..))
import Git.Index.Types (Stages, mapToStages, stagesToMap)
import Git.Internal (takeFor, nullTermStringP, oct, char_)
import Git.Sha1 (Sha1(..), sha1ByteStringParser)
import Git.Types (FileMode, fileModeFromInt, fileModeToInt)


newtype ResolveUndo
  = ResolveUndo
  { unResolveUndo :: Map Path.RelFileDir (Stages (FileMode, Sha1))
  } deriving (Show)

instance IndexExtension ResolveUndo where
  extSignature _ = "REUC"
  extParser size = do
      entries <- takeFor (fromIntegral size) entryP
      return $ ResolveUndo $ Map.fromList entries
    where
      entryP = do
        path <- Path.rel <$> nullTermStringP
        modes <- replicateM 3 (oct <* char_ '\NUL')
        stages <- zipWithM f [minBound..] modes >>=
          mapToStages . Map.fromList . catMaybes
        return (path, stages)
      f _ 0 = return Nothing
      f stage i = do
        mode <- fileModeFromInt i
        sha1 <- sha1ByteStringParser
        return $ Just (stage, (mode, sha1))

instance BuildableIndexExtension ResolveUndo where
  extBuilder = Map.foldMapWithKey f . fmap stagesToMap . unResolveUndo
    where
      b = fromByteString
      p = b . Char8.pack . (++ "\n") . Path.toString
      f path stages = p path <> modes stages <> sha1s stages
      defaultModes = Map.fromList [(m, 0) | m <- [succ minBound..]]
      modes stages = mconcat $ fmap (b . Char8.pack . printf "%o\NUL" . snd) $
        Map.toAscList $ fmap (fileModeToInt . fst) stages <> defaultModes
      sha1s = mconcat . fmap (b . unSha1 . snd . snd) . Map.toAscList
  extEmpty = Map.null . unResolveUndo
