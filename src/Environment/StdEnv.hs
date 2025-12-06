module Environment.StdEnv (mkStdEnv) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Environment (Environment, declareVar, newEnv)
import Value (Callable (..), FunctionType (..), Value (..))

-- | Build the standard environment with built-in functions and variables.
mkStdEnv :: IO (Environment Value)
mkStdEnv = do
  let clockCallable = VCallable (Callable (NativeFunction 0 "clock" clock))
  env <- newEnv
  declareVar "clock" clockCallable env
  pure env

clock :: forall m. (MonadIO m) => [Value] -> m Value
clock = const $ liftIO getPOSIXTime <&> (VNumber . (fromRational . toRational . nominalDiffTimeToSeconds))
