module Runtime.Environment
  ( Environment,
    Frame,
    newFrame,
    declareInFrame,
    findInFrame,
    assignInFrame,
    pushFrame,
    popFrame,
    getAtDistance,
    assignAtDistance,
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

pushFrame :: (MonadIO m) => Environment a -> m (Environment a)
pushFrame env = do
  f <- newFrame
  pure (f : env)

popFrame :: Environment a -> Environment a
popFrame [] = []
popFrame (_ : xs) = xs

-- Interaction with distances gotten from the resolver
getAtDistance :: (MonadIO m) => Int -> String -> Environment a -> m (Maybe a)
getAtDistance _ _ [] = pure Nothing
getAtDistance 0 name (f : _) = findInFrame name f
getAtDistance n name (_ : fs) = getAtDistance (n - 1) name fs

assignAtDistance :: (MonadIO m) => Int -> String -> a -> Environment a -> m Bool
assignAtDistance _ _ _ [] = pure False
assignAtDistance 0 name val (f : _) = assignInFrame name val f
assignAtDistance n name val (_ : fs) = assignAtDistance (n - 1) name val fs
