module Runtime.Interpreter.State
  ( ProgramState (..),
    newProgramState,
    declare,
    pushScope,
    popScope,
    getVariable,
    assignVariable,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Language.Syntax.Expression (LocalResolution (LocalResolution), Resolution (..))
import Runtime.Environment
  ( Environment,
    Globals,
    assignAtDistance,
    assignGlobal,
    declareGlobal,
    getAtDistance,
    lookupGlobal,
    newFrame,
    newGlobals,
    popFrame,
    writeSlot,
  )

data ProgramState a = ProgramState
  { environment :: !(Environment a),
    globals :: !(Globals a)
  }

newProgramState :: (MonadIO m) => m (ProgramState a)
newProgramState = do
  g <- newGlobals
  pure $ ProgramState {environment = [], globals = g}

-- | Declares a variable in the innermost scope, at the slot the resolver
-- assigned it: in the table of globals at the top level, otherwise in the
-- current frame.
declare :: (MonadIO m) => Int -> a -> ProgramState a -> m ()
declare slot val state = do
  case environment state of
    [] -> declareGlobal slot val (globals state)
    (top : _) -> writeSlot top slot val
{-# INLINE declare #-}

-- | 'Nothing' only for an undefined global: a resolved local always exists.
getVariable :: (MonadIO m) => Resolution -> ProgramState a -> m (Maybe a)
getVariable distance st =
  case distance of
    Local (LocalResolution d slot) -> Just <$> getAtDistance d slot (environment st)
    Global slot -> lookupGlobal slot (globals st)
{-# INLINE getVariable #-}

-- | 'False' only for an undefined global.
assignVariable :: (MonadIO m) => Resolution -> a -> ProgramState a -> m Bool
assignVariable distance val st =
  case distance of
    Local (LocalResolution d slot) -> assignAtDistance d slot val (environment st) >> pure True
    Global slot -> assignGlobal slot val (globals st)
{-# INLINE assignVariable #-}

-- | Pushes a fresh frame with the given number of slots.
pushScope :: (MonadIO m) => Int -> a -> ProgramState a -> m (ProgramState a)
pushScope size filler state = do
  frame <- newFrame size filler
  pure $ state {environment = frame : environment state}

popScope :: ProgramState a -> ProgramState a
popScope state = state {environment = popFrame (environment state)}
