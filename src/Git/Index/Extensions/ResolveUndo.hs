module Git.Index.Extensions.ResolveUndo where

import Control.Monad (replicateM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified System.Path as Path

import Git.Index.Extensions.Class (IndexExtension(..))
import Git.Index.Types (intToStage, Stages, mapToStages)
import Git.Internal (takeFor, nullTermStringP, oct, char_)
import Git.Sha1 (Sha1, sha1HexParser)


newtype ResolveUndo
  = ResolveUndo (Map Path.RelFileDir (Stages Sha1)) deriving (Show)

instance IndexExtension ResolveUndo where
  extSignature _ = "REUC"
  extParser size = do
      entries <- takeFor (fromIntegral size) entryP
      return $ ResolveUndo $ Map.fromList entries
    where
      entryP = do
        path <- Path.rel . Text.unpack <$> nullTermStringP
        stages <- replicateM 3 (oct <* char_ '\NUL')
          >>= mapM foo >>= mapToStages . Map.fromList . mapMaybe id
        return (path, stages)
      foo 0 = return Nothing
      foo n = do
        stage <- intToStage n
        sha1 <- sha1HexParser
        return $ Just (stage, sha1)
