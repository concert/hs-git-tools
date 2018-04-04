{-# LANGUAGE FlexibleContexts #-}

module Git.Pack.Delta where

import Control.Monad (unless)
import Control.Monad.Except (MonadError(..))
import qualified Data.ByteString as BS
import Data.Word
import Text.Printf (printf)

import Git.Types (ObjectType, GitError(..))

data DeltaInstruction
  = Insert {insData :: BS.ByteString}
  | Copy {copyOffset :: Word32, copyLength :: Word32}

instance Show DeltaInstruction where
  show (Insert dat) = printf "<Insert %d>" (BS.length dat)
  show (Copy o l) = printf "<Copy %d %d>" o l

data DeltaBody = DeltaBody
  { dbSourceLen :: Word64
  , dbTargetLen :: Word64
  , dbInstructions :: [DeltaInstruction]
  } deriving Show

data PackObjectChain
  = PackObjectChain
  { pocTy :: ObjectType
  , pocBaseData :: BS.ByteString
  , pocDeltas :: [DeltaBody]}

instance Show PackObjectChain where
  show poc = printf "<PackObjectChain: %s %d>" (show $ pocTy poc)
    (length $ pocDeltas poc)

deltaInsResultLen :: DeltaInstruction -> Word64
deltaInsResultLen di = case di of
  Insert dat -> fromIntegral $ BS.length dat
  Copy _ len -> fromIntegral len

deltaBodyOk :: DeltaBody -> Bool
deltaBodyOk db =
  sum (fmap deltaInsResultLen $ dbInstructions db) == dbTargetLen db

pocAppendDelta :: DeltaBody -> PackObjectChain -> PackObjectChain
pocAppendDelta d poc = poc { pocDeltas = pocDeltas poc ++ [d]}

renderPackObjectChain
  :: MonadError GitError m
  => PackObjectChain -> m (ObjectType, BS.ByteString)
renderPackObjectChain (PackObjectChain ty baseData deltas) =
    (ty,) <$> go deltas baseData
  where
    go [] b = return b
    go (d:ds) b = applyDelta b d >>= go ds

-- FIXME: we may want to make a Builder for the final body...?
applyDelta
  :: MonadError GitError m
  => BS.ByteString -> DeltaBody -> m BS.ByteString
applyDelta base db@(DeltaBody srcLen targLen inss) = do
  unless (deltaBodyOk db) $
    -- This is belt-and-braces when coupled with FailedPostDeltaApp... - it's
    -- a nicer check because we can apply it without shuffling the data:
    throwError FailedDeltaConsistencyCheck
  unless (fromIntegral (BS.length base) == srcLen) $
    throwError $ FailedPreDeltaApplicationLengthCheck (BS.length base) srcLen
  let new = mconcat $ fmap (applyInstruction base) inss
  unless (fromIntegral (BS.length new) == targLen) $
    throwError $ FailedPostDeltaApplicationLengthCheck (BS.length new) targLen
  return new

applyInstruction :: BS.ByteString -> DeltaInstruction -> BS.ByteString
applyInstruction base i = case i of
  Insert bs -> bs
  Copy ofs len -> BS.take (fromIntegral len) $ BS.drop (fromIntegral ofs) base
