module Runtime.Value
  ( Value (..),
    displayValue,
    isTruthy,
    Callable (..),
    CallableType (..),
    arity,
    evalLiteral,
    evalUnaryOp,
    evalBinaryOp,
    EvalError (..),
    displayEvalErr,
    LoxClass (..),
    LoxClassInstance (..),
    lookupField,
    setField,
    newClassInstance,
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState)
import Data.Char (toLower)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map qualified as M
import Language.Syntax.Expression (BinaryOperator (..), Literal (..), Resolution, UnaryOperator (..))
import Language.Syntax.Program (Class (..), Function (..))
import Numeric (showFFloat)
import Runtime.Environment (Environment)
import Runtime.Error (EvalError (..), displayEvalErr)
import Runtime.Interpreter.Error (InterpreterError)
import Runtime.Interpreter.State (ProgramState (..))

-- | Represents the values that can be produced by evaluating an expression.
data Value
  = VNumber Double
  | VBool Bool
  | VString String
  | VNil
  | VCallable Callable
  | VClassInstance LoxClassInstance
  deriving stock (Eq, Show)

data LoxClass = LoxClass
  { classDefinition :: Class Resolution,
    classClosure :: Closure
  }
  deriving stock (Eq)

data LoxClassInstance = LoxClassInstance
  { loxClass :: LoxClass,
    instanceFields :: IORef (M.Map String Value)
  }

instance Eq LoxClassInstance where
  (LoxClassInstance _ f1) == (LoxClassInstance _ f2) = f1 == f2

instance Show LoxClassInstance where
  show (LoxClassInstance {loxClass}) = className (classDefinition loxClass) ++ " instance"

newClassInstance :: (MonadIO m) => LoxClass -> m LoxClassInstance
newClassInstance cls = do
  fields <- liftIO $ newIORef mempty
  pure (LoxClassInstance cls fields)

lookupField :: (MonadIO m) => String -> LoxClassInstance -> m (Maybe Value)
lookupField fieldName (LoxClassInstance {instanceFields}) = do
  fields <- liftIO $ readIORef instanceFields
  pure $ M.lookup fieldName fields

setField :: (MonadIO m) => String -> Value -> LoxClassInstance -> m ()
setField fieldName value (LoxClassInstance {instanceFields}) =
  liftIO $ modifyIORef' instanceFields (M.insert fieldName value)

newtype Callable = Callable CallableType

type Closure = Environment Value

type MonadCallable m =
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  [Value] -> m Value

data CallableType
  = UserDefinedFunction (Function Resolution) Closure Bool
  | NativeFunction
      -- | arity
      Int
      -- | name
      String
      -- | implementation
      (forall m. MonadCallable m)
  | ClassConstructor LoxClass

arity :: Callable -> Int
arity (Callable (UserDefinedFunction func _ _)) = length . funcParams $ func
arity (Callable (NativeFunction n _ _)) = n
arity (Callable (ClassConstructor cls)) =
  let cMethods = (classMethods . classDefinition) cls
      ctor = M.lookup "init" cMethods
   in maybe 0 (length . funcParams) ctor

instance Eq Callable where
  (==) :: Callable -> Callable -> Bool
  (Callable func1) == (Callable func2) =
    case (func1, func2) of
      (UserDefinedFunction f1 _ _, UserDefinedFunction f2 _ _) -> funcName f1 == funcName f2
      (NativeFunction _ name1 _, NativeFunction _ name2 _) -> name1 == name2
      (ClassConstructor c1, ClassConstructor c2) -> c1 == c2
      _ -> False

instance Show Callable where
  show :: Callable -> String
  show (Callable (UserDefinedFunction func _ _)) = "<fn " ++ funcName func ++ ">"
  show (Callable (NativeFunction {})) = "<native fn>"
  show (Callable (ClassConstructor c)) = className (classDefinition c)

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
displayValue (VClassInstance instance') = show instance'

evalUnaryOp :: Int -> UnaryOperator -> Value -> Either EvalError Value
evalUnaryOp _ UMinus (VNumber n) = Right $ VNumber (negate n)
evalUnaryOp line UMinus _ = Left $ EvalError line "Operand must be a number."
evalUnaryOp _ Bang v = (Right . VBool . not . isTruthy) v

evalBinaryOp :: Int -> BinaryOperator -> Value -> Value -> Either EvalError Value
-- Greater than
evalBinaryOp _ Greater (VNumber n1) (VNumber n2) = Right $ VBool (n1 > n2)
evalBinaryOp line Greater _ _ = Left $ EvalError line "Operands must be numbers."
-- Greater than or equal to
evalBinaryOp _ GreaterEqual (VNumber n1) (VNumber n2) = Right $ VBool (n1 >= n2)
evalBinaryOp line GreaterEqual _ _ = Left $ EvalError line "Operands must be numbers."
-- Less than
evalBinaryOp _ Less (VNumber n1) (VNumber n2) = Right $ VBool (n1 < n2)
evalBinaryOp line Less _ _ = Left $ EvalError line "Operands must be numbers."
-- Less than or equal to
evalBinaryOp _ LessEqual (VNumber n1) (VNumber n2) = Right $ VBool (n1 <= n2)
evalBinaryOp line LessEqual _ _ = Left $ EvalError line "Operands must be numbers."
-- Subtraction only works for two numbers
evalBinaryOp _ BMinus (VNumber n1) (VNumber n2) = Right $ VNumber (n1 - n2)
evalBinaryOp line BMinus _ _ = Left $ EvalError line "Operands must be numbers."
-- Summation only works for two numbers (sum) or two strings (concatenation)
evalBinaryOp _ Plus (VNumber n1) (VNumber n2) = Right $ VNumber (n1 + n2)
evalBinaryOp _ Plus (VString s1) (VString s2) = Right $ VString (s1 ++ s2)
evalBinaryOp line Plus _ _ = Left $ EvalError line "Operands must be two numbers or two strings."
-- Division and multiplication, with error handling for division by zero
evalBinaryOp line Slash (VNumber n1) (VNumber n2)
  | n2 == 0 = Left $ EvalError line "Division by zero"
  | otherwise = Right $ VNumber (n1 / n2)
evalBinaryOp line Slash _ _ = Left $ EvalError line "Operands must be numbers."
-- Multiplication
evalBinaryOp _ Star (VNumber n1) (VNumber n2) = Right $ VNumber (n1 * n2)
evalBinaryOp line Star _ _ = Left $ EvalError line "Operands must be numbers."
-- Equality and inequality checks
evalBinaryOp _ EqualEqual v1 v2 = Right $ VBool (v1 == v2)
evalBinaryOp _ BangEqual v1 v2 = Right $ VBool (v1 /= v2)

evalLiteral :: Literal -> Value
evalLiteral (Number n) = VNumber n
evalLiteral (String s) = VString s
evalLiteral (Bool b) = VBool b
evalLiteral Nil = VNil
