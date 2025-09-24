module Evaluation (evalExpr, printValue, Value) where

import Data.Char (toLower)
import Expression.AST
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

-- | Pretty prints a value according to the Crafting Interpreters book.
printValue :: Value -> String
printValue (VNumber n) =
  let (integer :: Integer, decimal) = properFraction n
   in if decimal == 0
        then show integer -- Print as integer if no decimal part
        else showFFloat Nothing n "" -- Otherwise, print as floating-point number
printValue (VBool b) = (map toLower . show) b
printValue (VString s) = s
printValue VNil = "nil"

-- | Evaluates an expression and returns a value or an error message.
-- If the evaluation is successful, it returns a `Value`.
-- If there is an error,it returns a `String` describing it. (TODO: Get a better error type)
evalExpr :: Expression -> Either String Value
evalExpr (Literal lit) = Right $ evalLiteral lit
evalExpr (Grouping expr) = evalExpr expr
evalExpr (Unary op e) = evalExpr e >>= evalUnaryOp op
evalExpr (Binary op e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  evalBinaryOp op v1 v2

evalUnaryOp :: UnaryOperator -> Value -> Either String Value
evalUnaryOp UMinus (VNumber n) = Right $ VNumber (negate n)
evalUnaryOp UMinus _ = Left "Operand must be a number."
evalUnaryOp Bang v = (Right . VBool . not . isTruthy) v

evalBinaryOp :: BinaryOperator -> Value -> Value -> Either String Value
-- Greater than
evalBinaryOp Greater (VNumber n1) (VNumber n2) = Right $ VBool (n1 > n2)
evalBinaryOp Greater _ _ = Left "Operands must be numbers."
-- Greater than or equal to
evalBinaryOp GreaterEqual (VNumber n1) (VNumber n2) = Right $ VBool (n1 >= n2)
evalBinaryOp GreaterEqual _ _ = Left "Operands must be numbers."
-- Less than
evalBinaryOp Less (VNumber n1) (VNumber n2) = Right $ VBool (n1 < n2)
evalBinaryOp Less _ _ = Left "Operands must be numbers."
-- Less than or equal to
evalBinaryOp LessEqual (VNumber n1) (VNumber n2) = Right $ VBool (n1 <= n2)
evalBinaryOp LessEqual _ _ = Left "Operands must be numbers."
-- Subtraction only works for two numbers
evalBinaryOp BMinus (VNumber n1) (VNumber n2) = Right $ VNumber (n1 - n2)
evalBinaryOp BMinus _ _ = Left "Operands must be numbers."
-- Summation only works for two numbers (sum) or two strings (concatenation)
evalBinaryOp Plus (VNumber n1) (VNumber n2) = Right $ VNumber (n1 + n2)
evalBinaryOp Plus (VString s1) (VString s2) = Right $ VString (s1 ++ s2)
evalBinaryOp Plus _ _ = Left "Operands must be two numbers or two strings."
-- Division and multiplication, with error handling for division by zero
evalBinaryOp Slash (VNumber n1) (VNumber n2)
  | n2 == 0 = Left "Division by zero" -- Handle or just fail?
  | otherwise = Right $ VNumber (n1 / n2)
evalBinaryOp Slash _ _ = Left "Operands must be numbers."
-- Multiplication
evalBinaryOp Star (VNumber n1) (VNumber n2) = Right $ VNumber (n1 * n2)
evalBinaryOp Star _ _ = Left "Operands must be numbers."
-- Equality and inequality checks
evalBinaryOp EqualEqual v1 v2 = Right $ VBool (v1 == v2)
evalBinaryOp BangEqual v1 v2 = Right $ VBool (v1 /= v2)

evalLiteral :: Literal -> Value
evalLiteral (Number n) = VNumber n
evalLiteral (String s) = VString s
evalLiteral (Bool b) = VBool b
evalLiteral Nil = VNil

isTruthy :: Value -> Bool
isTruthy VNil = False
isTruthy (VBool b) = b
isTruthy _ = True
