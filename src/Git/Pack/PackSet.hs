{-# LANGUAGE FlexibleContexts #-}

module Git.Pack.PackSet where

import Control.Exception (throwIO)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (StateT(..), evalStateT)
import Control.Monad.State.Class (MonadState(get, put))
import Control.Monad.Trans (MonadTrans(..))
import qualified Data.ByteString as BS
import Data.List (isSuffixOf)
import Data.Word
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))

import Git.Internal (replaceSuffix, liftEitherGitError)
import Git.Objects (ObjectType)
import Git.Sha1 (Sha1)
import Git.Types (GitError)

import Git.Pack.Index
  (PackIndexState, openPackIndex, getPackRecordOffset)
import Git.Pack.Delta
  (PackObjectChain(..), pocAppendDelta, renderPackObjectChain)
import Git.Pack.Pack
  ( PackHandle, openPackFile, getPackObjectData, RefDelta(..), OfsDelta(..)
  , DeltaObject(..))

type PackPair = (PackIndexState, PackHandle)
type PackSet = [PackPair]
type PackSetM m r = ExceptT GitError (StateT PackSet m) r

openPackSet :: (MonadIO m, MonadError GitError m) => FilePath -> m PackSet
openPackSet packDirPath = do
  files <- liftIO $ listDirectory packDirPath
  let indexNames = filter (".idx" `isSuffixOf`) files
  packNames <- liftEitherGitError $ mapM (replaceSuffix "idx" "pack") indexNames
  indexStates <- mapM (openPackIndex . (packDirPath </>)) indexNames
  packHandles <- mapM (openPackFile . (packDirPath </>)) packNames
  return $ zip indexStates packHandles

-- This is a bit of a hack just to make it easier to use this code from Store...
withPackSet :: MonadIO m => FilePath -> PackSetM m r -> m r
withPackSet path m = do
    ps <- except $ runExceptT $ openPackSet path
    except $ evalStateT (runExceptT m) ps
  where
    except f = f >>= either (liftIO . throwIO) return

packSetGetObjectData
  :: Monad m
  => Sha1 -> ExceptT GitError (StateT PackSet m) (ObjectType, BS.ByteString)
packSetGetObjectData sha1 = getObjectChain sha1 >>= renderPackObjectChain

getObjectChain
  :: Monad m => Sha1 -> ExceptT GitError (StateT PackSet m) PackObjectChain
getObjectChain sha1 = do
    packSetGetOffset sha1 >>= uncurry getObjectChainFromOffset
  where
    handleEntity _ _ (Left (ty, objData)) = return $
      PackObjectChain ty objData []
    handleEntity ph offset (Right delta) = case delta of
      RefDelta' rd ->
        pocAppendDelta (rdBody rd) <$>
        getObjectChain (rdBaseSha1 rd)
      OfsDelta' od ->
        pocAppendDelta (odBody od) <$>
        getObjectChainFromOffset ph (offset - odBaseNegOfs od)

    getObjectChainFromOffset ph offset =
      getPackObjectData ph offset >>= handleEntity ph offset

packSetGetOffset
  :: Monad m
  => Sha1
  -> ExceptT GitError (StateT PackSet m) (PackHandle, Word64)
packSetGetOffset sha1 = withExceptT last $ statefulFirstSuccess go
  where
    go = do
      offset <- liftToPackPair $ getPackRecordOffset sha1
      (_, ph) <- get
      return (ph, offset)

liftToPackPair
  :: Monad m
  => ExceptT GitError (StateT PackIndexState m) a
  -> ExceptT GitError (StateT PackPair m) a
liftToPackPair m = ExceptT $ StateT $ \(pis, ph) ->
  fmap (\(ea, pis') -> (ea, (pis', ph))) $ runStateT (runExceptT m) pis

statefulFirstSuccess
  :: Monad m => ExceptT e (StateT s m) a -> ExceptT [e] (StateT [s] m) a
statefulFirstSuccess m = do
    states <- lift get
    (result, states') <- lift $ lift $
      firstSuccess' (\s -> runStateT (runExceptT m) s) states
    put states'
    either throwError return result
  where
    firstSuccess'
      :: Monad m => (s -> m (Either e a, s)) -> [s] -> m (Either [e] a, [s])
    firstSuccess' = go [] []
    go
      :: Monad m => [e] -> [s] -> (s -> m (Either e a, s)) -> [s]
      -> m (Either [e] a, [s])
    go errs states' _ [] = return (Left $ reverse errs, reverse states')
    go errs states' f (state:states) = do
      (res, state') <- f state
      case res of
        Left e -> go (e : errs) (state' : states') f states
        Right a -> return (Right a, (reverse $ state' : states') ++ states)
