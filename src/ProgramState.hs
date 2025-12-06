module ProgramState
  ( ProgramState (..),
    newProgramState,
    declare,
    assign,
    find,
    pushScope,
    pushClosureScope,
    popScope,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Map qualified as M
import Environment
  ( Environment,
    Frame,
    assignInEnv,
    assignInFrame,
    declareInFrame,
    findInEnv,
    findInFrame,
    newFrame,
    popFrame,
    pushFrame,
  )

data ProgramState a = ProgramState
  { environment :: Environment a,
    globals :: Frame a,
    locals :: M.Map String Int
  }

newProgramState :: (MonadIO m) => m (ProgramState a)
newProgramState = do
  g <- newFrame
  pure $ ProgramState {environment = [], globals = g, locals = mempty}

declare :: (MonadIO m) => String -> a -> ProgramState a -> m ()
declare name val state = do
  case environment state of
    [] -> declareInFrame name val (globals state)
    (top : _) -> declareInFrame name val top

assign :: (MonadIO m) => String -> a -> ProgramState a -> m Bool
assign name val state = do
  foundInLocals <- assignInEnv name val (environment state)
  if foundInLocals
    then pure True
    else assignInFrame name val (globals state)

find :: (MonadIO m) => String -> ProgramState a -> m (Maybe a)
find name state = do
  inLocals <- findInEnv name (environment state)
  case inLocals of
    Just v -> pure (Just v)
    Nothing -> findInFrame name (globals state)

pushScope :: (MonadIO m) => ProgramState a -> m (ProgramState a)
pushScope state = do
  newEnv <- pushFrame (environment state)
  pure $ state {environment = newEnv}

pushClosureScope :: (MonadIO m) => Environment a -> ProgramState a -> m (ProgramState a)
pushClosureScope closure state = do
  newEnv <- pushFrame closure
  pure $ state {environment = newEnv}

popScope :: ProgramState a -> ProgramState a
popScope state = state {environment = popFrame (environment state)}
