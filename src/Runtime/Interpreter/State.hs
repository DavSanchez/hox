module Runtime.Interpreter.State
  ( ProgramState (..),
    newProgramState,
    declare,
    pushScope,
    pushClosureScope,
    popScope,
    getVariable,
    assignVariable,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Language.Syntax.Expression (LocalResolution (LocalResolution), Resolution (..))
import Runtime.Environment
  ( Environment,
    Frame,
    assignAtDistance,
    assignInFrame,
    declareInFrame,
    findInFrame,
    getAtDistance,
    newFrame,
    popFrame,
    pushFrame,
  )

data ProgramState a = ProgramState
  { environment :: Environment a,
    globals :: Frame a
  }

newProgramState :: (MonadIO m) => m (ProgramState a)
newProgramState = do
  g <- newFrame
  pure $ ProgramState {environment = [], globals = g}

declare :: (MonadIO m) => String -> a -> ProgramState a -> m ()
declare name val state = do
  case environment state of
    [] -> declareInFrame name val (globals state)
    (top : _) -> declareInFrame name val top

getVariable :: (MonadIO m) => String -> Resolution -> ProgramState a -> m (Maybe a)
getVariable name distance st =
  let env' = environment st
      globals' = globals st
   in case distance of
        Local (LocalResolution d) -> getAtDistance d name env'
        Global -> findInFrame name globals'

assignVariable :: (MonadIO m) => String -> Resolution -> a -> ProgramState a -> m Bool
assignVariable name distance val st =
  let env' = environment st
      globals' = globals st
   in case distance of
        Local (LocalResolution d) -> assignAtDistance d name val env'
        Global -> assignInFrame name val globals'

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
