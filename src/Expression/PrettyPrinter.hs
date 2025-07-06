module Expression.PrettyPrinter (prettyPrintExpr) where

import Data.Char (toLower)
import Expression.AST (BinaryOperator (..), Expression (..), Literal (..), UnaryOperator (..))

prettyPrintBinOp :: BinaryOperator -> String
prettyPrintBinOp EqualEqual = "=="
prettyPrintBinOp BangEqual = "!="
prettyPrintBinOp Less = "<"
prettyPrintBinOp LessEqual = "<="
prettyPrintBinOp Greater = ">"
prettyPrintBinOp GreaterEqual = ">="
prettyPrintBinOp Plus = "+"
prettyPrintBinOp BMinus = "-"
prettyPrintBinOp Star = "*"
prettyPrintBinOp Slash = "/"

prettyPrintUnOp :: UnaryOperator -> String
prettyPrintUnOp UMinus = "-"
prettyPrintUnOp Bang = "!"

prettyPrintLit :: Literal -> String
prettyPrintLit (Number n) = show n
prettyPrintLit (String s) = "String " <> s
prettyPrintLit (Bool b) = (map toLower . show) b
prettyPrintLit Nil = "nil"

-- | Prints and expression in the format expected by the Crafting Interpreters book.
-- >>> prettyPrintExpr (Binary Plus (Literal (Number 1)) (Literal (Number 2)))
-- "(+ 1.0 2.0)"
-- >>> prettyPrintExpr (Binary Star (Unary UMinus (Literal (Number 123))) (Literal (Number 45.67)))
-- "(* (- 123.0) 45.67)"
-- >>> prettyPrintExpr (Binary Star (Unary UMinus (Literal (Number 123))) (Grouping (Literal (Number 45.67))))
-- "(* (- 123.0) (group 45.67))"
prettyPrintExpr :: Expression -> String
prettyPrintExpr (Literal lit) = prettyPrintLit lit
prettyPrintExpr (Unary op expr) = "(" <> prettyPrintUnOp op <> " " <> prettyPrintExpr expr <> ")"
prettyPrintExpr (Binary op e1 e2) = "(" <> prettyPrintBinOp op <> " " <> prettyPrintExpr e1 <> " " <> prettyPrintExpr e2 <> ")"
prettyPrintExpr (Grouping expr) = "(group " <> prettyPrintExpr expr <> ")"
