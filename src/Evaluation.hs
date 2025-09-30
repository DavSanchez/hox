module Evaluation (evalExpr, EvalError, printValue, prettyPrintEvalErr, Value) where

import Data.Char (toLower)
import Expression
  ( BinaryOperator (..),
    Expression (..),
    Literal (..),
    UnaryOperator (..),
  )
import Numeric (showFFloat)

-- | Represents the values that can be produced by evaluating an expression.
data Value
  = VNumber Double
  | VBool Bool
  | VString String
  | VNil
  deriving stock (Show, Eq)

data EvalError = EvalError
  { errorLine :: Int,
    errorMessage :: String
  }
  deriving stock (Show, Eq)

prettyPrintEvalErr :: EvalError -> String
prettyPrintEvalErr (EvalError line msg) = msg <> "\n[line " <> show line <> "]"

-- | Pretty prints a value according to the Crafting Interpreters book.
-- >>> printValue <$> evalExpr (Literal (Number (-0.0)))
-- Right "-0"
printValue :: Value -> String
printValue (VNumber n) =
  let (integer :: Integer, decimal) = properFraction n
   in if decimal == 0
        then if isNegativeZero n then "-0" else show integer
        else showFFloat Nothing n "" -- Otherwise, print as floating-point number
printValue (VBool b) = (map toLower . show) b
printValue (VString s) = s
printValue VNil = "nil"

-- | Evaluates an expression and returns a value or an error message.
-- If the evaluation is successful, it returns a `Value`.
-- If there is an error,it returns an `EvalError` describing it.
-- >>> evalExpr (Literal (Number (-0.0)))
-- Right (VNumber (-0.0))
evalExpr :: Expression -> Either EvalError Value
evalExpr (Literal lit) = Right $ evalLiteral lit
evalExpr (Grouping expr) = evalExpr expr
evalExpr (UnaryOperation line op e) = evalExpr e >>= evalUnaryOp line op
evalExpr (BinaryOperation line op e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  evalBinaryOp line op v1 v2
evalExpr (Variable line name) = Left $ EvalError line ("Undefined variable: " <> name)

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

isTruthy :: Value -> Bool
isTruthy VNil = False
isTruthy (VBool b) = b
isTruthy _ = True
