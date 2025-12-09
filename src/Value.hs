module Value
  ( Value (..),
    displayValue,
    isTruthy,
    Callable (..),
    FunctionType (..),
    arity,
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)
import Data.Char (toLower)
import Environment (Environment)
import Expression (Resolution)
import Interpreter.Error (InterpreterError)
import Interpreter.State (ProgramState (..))
import Numeric (showFFloat)
import Program (Function (..))

-- | Represents the values that can be produced by evaluating an expression.
data Value
  = VNumber Double
  | VBool Bool
  | VString String
  | VNil
  | VCallable Callable
  deriving stock (Eq, Show)

newtype Callable = Callable FunctionType

type Closure = Environment Value

data FunctionType
  = UserDefined (Function Resolution) Closure
  | NativeFunction
      -- | arity
      Int
      -- | name
      String
      -- | implementation
      ( forall m.
        ( MonadState (ProgramState Value) m,
          MonadError InterpreterError m,
          MonadIO m
        ) =>
        [Value] -> m Value
      )

arity :: Callable -> Int
arity (Callable (UserDefined func _)) = length (funcParams func)
arity (Callable (NativeFunction n _ _)) = n

instance Eq Callable where
  (==) :: Callable -> Callable -> Bool
  (Callable func1) == (Callable func2) =
    case (func1, func2) of
      (UserDefined f1 _, UserDefined f2 _) -> funcName f1 == funcName f2
      (NativeFunction _ name1 _, NativeFunction _ name2 _) -> name1 == name2
      _ -> False

instance Show Callable where
  show :: Callable -> String
  show (Callable (UserDefined func _)) = "<fn " ++ funcName func ++ ">"
  show (Callable (NativeFunction {})) = "<native fn>"

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
