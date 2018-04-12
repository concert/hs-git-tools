{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
#-}

module Git.Types.Internal where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Except (ExceptT(..), MonadError, throwError, runExceptT)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.Binary (anyWord32be, anyWord64be)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSIntern
import Data.Word
import Foreign.ForeignPtr (ForeignPtr)

import Git.Types.Error (GitError(..))
import Git.Sha1 (Sha1)
import qualified Git.Sha1 as Sha1

instance MonadFail (Either String) where
  fail = Left

-- FIXME: replace with Control.Monad.Except.liftEither when >= 2.2.2
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return

liftEitherGitError :: MonadError GitError m => Either String a -> m a
liftEitherGitError = liftEither . first GenericError

firstSuccess
  :: Monad m => (a -> ExceptT e m b) -> [a] -> ExceptT [(a, e)] m (a, b)
firstSuccess f as = ExceptT $ go (fmap (\a -> (a, f a)) as) []
  where
    go
      :: Monad m => [(a, ExceptT e m b)] -> [(a, e)] ->  m (Either [(a, e)] (a, b))
    go [] aes = return $ Left aes
    go ((a, exT):aexTs) aes = runExceptT exT >>=
      either (go aexTs . (: aes) . (a,)) (return . Right . (a,))

replaceSuffix :: (Eq a, MonadFail m) => [a] -> [a] -> [a] -> m [a]
replaceSuffix old new s = do
  delta <- dropLengthMaybe old s
  let (prefix, old') = splitLength delta s
  if old' == old then return $ prefix ++ new else fail "suffix did not match"

splitLength :: [a] -> [b] -> ([b], [b])
splitLength as bs = first reverse $ go [] as bs
  where
    go prefix [] ys = (prefix, ys)
    go prefix _ [] = (prefix, [])
    go prefix (_:xs) (y:ys) = go (y : prefix) xs ys

dropLengthMaybe :: MonadFail m => [a] -> [b] -> m [b]
dropLengthMaybe [] bs = return bs
dropLengthMaybe _ [] = fail "Ran out of bs"
dropLengthMaybe (_:as) (_:bs) = dropLengthMaybe as bs

type MmapHandle = (ForeignPtr Word8, Int, Int)

data MmapFrom = FromStart Int | FromEnd Int deriving (Show)
data MmapTo = Length Int | ToEnd deriving (Show)

mmapData :: MmapHandle -> MmapFrom -> MmapTo -> BS.ByteString
mmapData (ptr, mmapOfs, mmapSize) from to =
  let
    startOfs = case from of
      FromStart i -> mmapOfs + i
      FromEnd i -> mmapOfs + mmapSize - i
    len = case to of
      Length i -> i
      ToEnd -> mmapOfs + mmapSize - startOfs
  in
    BSIntern.fromForeignPtr ptr startOfs len

mmapWord32be :: MmapHandle -> MmapFrom -> Word32
mmapWord32be h from = either error id $ parseOnly anyWord32be $
  mmapData h from (Length 4)

mmapWord64be :: MmapHandle -> MmapFrom -> Word64
mmapWord64be h from = either error id $ parseOnly anyWord64be $
  mmapData h from (Length 8)

mmapSha1 :: MmapHandle -> MmapFrom -> Sha1
mmapSha1 h from = either error id $ Sha1.fromByteString $
  mmapData h from (Length 20)
