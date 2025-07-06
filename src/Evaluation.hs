module Evaluation (evalExpr, printValue) where

import Data.Char (toLower)
import Expression.AST
  ( BinaryOperator (..),
    Expression (..),
    Literal (..),
    UnaryOperator (..),
  )
import GHC.Float (int2Double)

-- | Represents the values that can be produced by evaluating an expression.
data Value
  = VNumber Double
  | VBool Bool
  | VString String
  | VNil
  deriving stock (Show, Eq)

-- | Pretty prints a value according to the Crafting Interpreters book.
printValue :: Value -> String
printValue (VNumber n)
  | n == int2Double (round n) = show (round n :: Int)
  | otherwise = show n
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
evalUnaryOp Bang v = (Right . VBool . not . isTruthy) v
evalUnaryOp op v = Left $ "Invalid unary operation: " <> show op <> " with " <> show v

evalBinaryOp :: BinaryOperator -> Value -> Value -> Either String Value
-- Numeric operations
evalBinaryOp Greater (VNumber n1) (VNumber n2) = Right $ VBool (n1 > n2)
evalBinaryOp GreaterEqual (VNumber n1) (VNumber n2) = Right $ VBool (n1 >= n2)
evalBinaryOp Less (VNumber n1) (VNumber n2) = Right $ VBool (n1 < n2)
evalBinaryOp LessEqual (VNumber n1) (VNumber n2) = Right $ VBool (n1 <= n2)
evalBinaryOp BMinus (VNumber n1) (VNumber n2) = Right $ VNumber (n1 - n2)
evalBinaryOp Plus (VNumber n1) (VNumber n2) = Right $ VNumber (n1 + n2)
-- Concatenation for strings
evalBinaryOp Plus (VString s1) (VString s2) = Right $ VString (s1 ++ s2)
-- Division and multiplication, with error handling for division by zero
evalBinaryOp Slash (VNumber n1) (VNumber n2)
  | n2 == 0 = Left "Division by zero" -- Handle or just fail?
  | otherwise = Right $ VNumber (n1 / n2)
evalBinaryOp Star (VNumber n1) (VNumber n2) = Right $ VNumber (n1 * n2)
-- Equality and inequality checks
evalBinaryOp EqualEqual v1 v2 = Right $ VBool (v1 == v2)
evalBinaryOp BangEqual v1 v2 = Right $ VBool (v1 /= v2)
-- Unsupported operations, or mismatched types
evalBinaryOp op v1 v2 = Left $ "Invalid binary operation: " <> show op <> " with " <> show v1 <> " and " <> show v2

evalLiteral :: Literal -> Value
evalLiteral (Number n) = VNumber n
evalLiteral (String s) = VString s
evalLiteral (Bool b) = VBool b
evalLiteral Nil = VNil

isTruthy :: Value -> Bool
isTruthy VNil = False
isTruthy (VBool b) = b
isTruthy _ = True
