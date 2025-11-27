module Environment.StdEnv (mkStdEnv) where

import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.IORef (newIORef)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Environment (Environment)
import Value (Callable (..), Value (..))

-- | Build the standard environment with built-in functions and variables.
mkStdEnv :: IO (Environment Value)
mkStdEnv = do
  clockRef <- newIORef (VCallable clock)
  pure $ NE.singleton $ M.fromList [("clock", clockRef)]

clock :: Callable
clock =
  Callable
    { arity = 0,
      name = "clock",
      closure = Nothing,
      call = const $ liftIO getPOSIXTime <&> (VNumber . (fromRational . toRational . nominalDiffTimeToSeconds))
    }
