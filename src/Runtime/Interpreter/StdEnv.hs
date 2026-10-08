{-# LANGUAGE OverloadedStrings #-}

module Runtime.Interpreter.StdEnv (mkStdEnv, stdGlobalNames) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Runtime.Interpreter.State (ProgramState, declare, newProgramState)
import Runtime.Value (Callable (..), CallableType (..), Value (..))

-- | Build the standard environment with built-in functions and variables.
mkStdEnv :: (MonadIO m) => m (ProgramState Value)
mkStdEnv = do
  state <- newProgramState
  -- The resolver gives these the first global indices, in the same order.
  for_ (zip [0 ..] stdGlobals) $ \(i, (_, value)) -> declare i value state
  pure state

-- | The names of the globals that exist before the program runs, in index order.
stdGlobalNames :: [Text]
stdGlobalNames = map fst stdGlobals

stdGlobals :: [(Text, Value)]
stdGlobals =
  [("clock", VCallable (Callable (NativeFunction 0 "clock" clock)))]

clock :: forall m. (MonadIO m) => [Value] -> m Value
clock = const $ liftIO getPOSIXTime <&> (VNumber . (fromRational . toRational . nominalDiffTimeToSeconds))
