{-# LANGUAGE OverloadedStrings #-}

module Runtime.Interpreter.StdEnv (mkStdEnv) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Runtime.Interpreter.State (ProgramState, declare, newProgramState)
import Runtime.Value (Callable (..), CallableType (..), Value (..))

-- | Build the standard environment with built-in functions and variables.
mkStdEnv :: (MonadIO m) => m (ProgramState Value)
mkStdEnv = do
  let clockCallable = VCallable (Callable (NativeFunction 0 "clock" clock))
  state <- newProgramState
  declare "clock" clockCallable state
  pure state

clock :: forall m. (MonadIO m) => [Value] -> m Value
clock = const $ liftIO getPOSIXTime <&> (VNumber . (fromRational . toRational . nominalDiffTimeToSeconds))
