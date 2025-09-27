module Expression.PrettyPrinter (prettyPrintExpr) where

import Data.Char (toLower)
import Expression.AST (BiOp (..), BinaryOperator (..), Expression (..), Literal (..), UnOp (..), UnaryOperator (..))

prettyPrintBinOp :: BinaryOperator -> String
prettyPrintBinOp (BinaryOperator _ EqualEqual) = "=="
prettyPrintBinOp (BinaryOperator _ BangEqual) = "!="
prettyPrintBinOp (BinaryOperator _ Less) = "<"
prettyPrintBinOp (BinaryOperator _ LessEqual) = "<="
prettyPrintBinOp (BinaryOperator _ Greater) = ">"
prettyPrintBinOp (BinaryOperator _ GreaterEqual) = ">="
prettyPrintBinOp (BinaryOperator _ Plus) = "+"
prettyPrintBinOp (BinaryOperator _ BMinus) = "-"
prettyPrintBinOp (BinaryOperator _ Star) = "*"
prettyPrintBinOp (BinaryOperator _ Slash) = "/"

prettyPrintUnOp :: UnaryOperator -> String
prettyPrintUnOp (UnaryOperator _ UMinus) = "-"
prettyPrintUnOp (UnaryOperator _ Bang) = "!"

prettyPrintLit :: Literal -> String
prettyPrintLit (Number n) = show n
prettyPrintLit (String s) = "String " <> s
prettyPrintLit (Bool b) = (map toLower . show) b
prettyPrintLit Nil = "nil"

-- | Prints and expression in the format expected by the Crafting Interpreters book.
-- >>> prettyPrintExpr (Binary (BinaryOperator 1 Plus) (Literal (Number 1)) (Literal (Number 2)))
-- "(+ 1.0 2.0)"
-- >>> prettyPrintExpr (Binary (BinaryOperator 1 Star) (Unary (UnaryOperator 1 UMinus) (Literal (Number 123))) (Literal (Number 45.67)))
-- "(* (- 123.0) 45.67)"
-- >>> prettyPrintExpr (Binary (BinaryOperator 1 Star) (Unary (UnaryOperator 1 UMinus) (Literal (Number 123))) (Grouping (Literal (Number 45.67))))
-- "(* (- 123.0) (group 45.67))"
prettyPrintExpr :: Expression -> String
prettyPrintExpr (Literal lit) = prettyPrintLit lit
prettyPrintExpr (Unary op expr) = "(" <> prettyPrintUnOp op <> " " <> prettyPrintExpr expr <> ")"
prettyPrintExpr (Binary op e1 e2) = "(" <> prettyPrintBinOp op <> " " <> prettyPrintExpr e1 <> " " <> prettyPrintExpr e2 <> ")"
prettyPrintExpr (Grouping expr) = "(group " <> prettyPrintExpr expr <> ")"
