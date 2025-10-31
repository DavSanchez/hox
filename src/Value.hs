module Value
  ( Value (..),
    displayValue,
    isTruthy,
    Callable (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Char (toLower)
import Numeric (showFFloat)

-- | Represents the values that can be produced by evaluating an expression.
data Value
  = VNumber Double
  | VBool Bool
  | VString String
  | VNil
  | VCallable Callable
  deriving stock (Eq, Show)

data Callable = Callable
  { arity :: Int,
    name :: String,
    call :: forall m. (MonadIO m) => [Value] -> m Value
  }

instance Eq Callable where
  (==) :: Callable -> Callable -> Bool
  (Callable a1 n1 _) == (Callable a2 n2 _) = a1 == a2 && n1 == n2

instance Show Callable where
  show :: Callable -> String
  show (Callable _ name _) = "<fn " ++ name ++ ">"

isTruthy :: Value -> Bool
isTruthy VNil = False
isTruthy (VBool b) = b
isTruthy _ = True

-- | Pretty prints a value according to the Crafting Interpreters book.
-- >>> displayValue (VNumber (-0.0))
-- "-0"
-- >>> displayValue (VNumber 42.5)
-- "42.5"
displayValue :: Value -> String
displayValue (VNumber n) =
  let (integer :: Integer, decimal) = properFraction n
   in if decimal == 0
        then if isNegativeZero n then "-0" else show integer
        else showFFloat Nothing n "" -- Otherwise, print as floating-point number
displayValue (VBool b) = (map toLower . show) b
displayValue (VString s) = s
displayValue VNil = "nil"
displayValue (VCallable callable) = show callable
