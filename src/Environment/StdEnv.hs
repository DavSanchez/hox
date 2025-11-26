module Environment.StdEnv (stdEnv) where

import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Environment (Environment)
import Value (Callable (..), Value (..))

-- | The standard environment with built-in functions and variables.
stdEnv :: Environment Value
stdEnv = NE.singleton $ M.fromList [("clock", VCallable clock)]

clock :: Callable
clock =
  Callable
    { arity = 0,
      name = "clock",
      closure = Nothing,
      call = const $ liftIO getPOSIXTime <&> (VNumber . (fromRational . toRational . nominalDiffTimeToSeconds))
    }
