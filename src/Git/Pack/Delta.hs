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

data DeltaBody = DeltaBody
  { dbSourceLen :: Word64
  , dbTargetLen :: Word64
  , dbInstructions :: [DeltaInstruction]
  }

data PackObjectChain
  = PackObjectChain
  { pocTy :: ObjectType
  , pocBaseData :: SizedByteString
  , pocDeltas :: [DeltaBody]}

instance Show PackObjectChain where
  show poc = printf "<PackObjectChain: %s %d>" (show $ pocTy poc)
    (length $ pocDeltas poc)

pocConsDelta :: DeltaBody -> PackObjectChain -> PackObjectChain
pocConsDelta d poc = poc { pocDeltas = d : pocDeltas poc}

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
applyDelta base (DeltaBody srcLen targLen inss) = do
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
