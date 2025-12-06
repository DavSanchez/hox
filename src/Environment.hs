module Environment
  ( Environment,
    Frame,
    newFrame,
    declareInFrame,
    findInFrame,
    assignInFrame,
    findInEnv,
    assignInEnv,
    pushFrame,
    popFrame,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (($>))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map qualified as M

type Frame a = IORef (M.Map String a)

type Environment a = [Frame a]

newFrame :: (MonadIO m) => m (Frame a)
newFrame = liftIO $ newIORef mempty

declareInFrame :: (MonadIO m) => String -> a -> Frame a -> m ()
declareInFrame name val frame = liftIO $ modifyIORef' frame (M.insert name val)

findInFrame :: (MonadIO m) => String -> Frame a -> m (Maybe a)
findInFrame name frame = do
  m <- liftIO $ readIORef frame
  pure $ M.lookup name m

assignInFrame :: (MonadIO m) => String -> a -> Frame a -> m Bool
assignInFrame name val frame = do
  m <- liftIO $ readIORef frame
  if M.member name m
    then liftIO (modifyIORef' frame (M.insert name val)) $> True
    else pure False

findInEnv :: (MonadIO m) => String -> Environment a -> m (Maybe a)
findInEnv _ [] = pure Nothing
findInEnv name (f : fs) = do
  res <- findInFrame name f
  case res of
    Just v -> pure (Just v)
    Nothing -> findInEnv name fs

assignInEnv :: (MonadIO m) => String -> a -> Environment a -> m Bool
assignInEnv _ _ [] = pure False
assignInEnv name val (f : fs) = do
  done <- assignInFrame name val f
  if done
    then pure True
    else assignInEnv name val fs

pushFrame :: (MonadIO m) => Environment a -> m (Environment a)
pushFrame env = do
  f <- newFrame
  pure (f : env)

popFrame :: Environment a -> Environment a
popFrame [] = []
popFrame (_ : xs) = xs
