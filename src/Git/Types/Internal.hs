{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
#-}

module Git.Types.Internal where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Except (ExceptT(..), MonadError, throwError, runExceptT)
import Data.Bifunctor (first)

instance MonadFail (Either String) where
  fail = Left

-- FIXME: replace with Control.Monad.Except.liftEither when >= 2.2.2
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return

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
