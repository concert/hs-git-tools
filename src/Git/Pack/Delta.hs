{-# LANGUAGE FlexibleContexts #-}

module Git.Pack.Delta where

import Control.Monad (unless)
import Control.Monad.Except (MonadError(..))
import Data.Word
import Text.Printf (printf)

import Git.Types (ObjectType, GitError(..))
import Git.Types.SizedByteString (SizedByteString)
import qualified Git.Types.SizedByteString as SBS

data DeltaInstruction
  = Insert {insData :: SizedByteString}
  | Copy {copyOffset :: Word32, copyLength :: Word32}

instance Show DeltaInstruction where
  show (Insert dat) = printf "<Insert %d>" (SBS.length dat)
  show (Copy o l) = printf "<Copy %d %d>" o l

data DeltaBody = DeltaBody
  { dbSourceLen :: Word64
  , dbTargetLen :: Word64
  , dbInstructions :: [DeltaInstruction]
  } deriving Show

data PackObjectChain
  = PackObjectChain
  { pocTy :: ObjectType
  , pocBaseData :: SizedByteString
  , pocDeltas :: [DeltaBody]}

instance Show PackObjectChain where
  show poc = printf "<PackObjectChain: %s %d>" (show $ pocTy poc)
    (length $ pocDeltas poc)

deltaInsResultLen :: DeltaInstruction -> Word64
deltaInsResultLen di = case di of
  Insert sbs -> SBS.length sbs
  Copy _ len -> fromIntegral len

deltaBodyOk :: DeltaBody -> Bool
deltaBodyOk db =
  sum (fmap deltaInsResultLen $ dbInstructions db) == dbTargetLen db

pocAppendDelta :: DeltaBody -> PackObjectChain -> PackObjectChain
pocAppendDelta d poc = poc { pocDeltas = pocDeltas poc ++ [d]}

renderPackObjectChain
  :: MonadError GitError m
  => PackObjectChain -> m (ObjectType, SizedByteString)
renderPackObjectChain (PackObjectChain ty baseData deltas) =
    (ty,) <$> go deltas baseData
  where
    go [] b = return b
    go (d:ds) b = applyDelta b d >>= go ds

applyDelta
  :: MonadError GitError m
  => SizedByteString -> DeltaBody -> m SizedByteString
applyDelta base db@(DeltaBody srcLen targLen inss) = do
  unless (deltaBodyOk db) $
    -- This is belt-and-braces when coupled with FailedPostDeltaApp... - it's
    -- a nicer check because we can apply it without shuffling the data:
    throwError FailedDeltaConsistencyCheck
  unless (SBS.length base == srcLen) $
    throwError $ FailedPreDeltaApplicationLengthCheck (SBS.length base) srcLen
  let new = mconcat $ fmap (applyInstruction base) inss
  unless (SBS.length new == targLen) $
    throwError $ FailedPostDeltaApplicationLengthCheck (SBS.length new) targLen
  return new

applyInstruction :: SizedByteString -> DeltaInstruction -> SizedByteString
applyInstruction base i = case i of
  Insert sbs -> sbs
  Copy ofs len -> SBS.take (fromIntegral len) $ SBS.drop (fromIntegral ofs) base
