module Representation (prettyPrintExpr) where

import Data.Char (toLower)

{-
  This module would contain the types representing the AST of Lox.

  This is the grammar (for now):

  expression     → literal
                 | unary
                 | binary
                 | grouping ;

  literal        → NUMBER | STRING | "true" | "false" | "nil" ;
  grouping       → "(" expression ")" ;
  unary          → ( "-" | "!" ) expression ;
  binary         → expression operator expression ;
  operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
                 | "+"  | "-"  | "*" | "/" ;

  This naturally translates to Haskell ADTs, so that's what we will use.
-}

data Expression
  = Literal Literal
  | Unary UnaryOperator Expression
  | Binary Expression BinaryOperator Expression
  | Grouping Expression
  deriving stock (Show)

data Literal = Number Double | String String | Bool Bool | Nil
  deriving stock (Show)

data UnaryOperator = UMinus | Bang deriving stock (Show)

data BinaryOperator
  = EqualEqual
  | BangEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Plus
  | BMinus
  | Star
  | Slash
  deriving stock (Show)

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

prettyPrintExpr :: Expression -> String
prettyPrintExpr (Literal lit) = prettyPrintLit lit
prettyPrintExpr (Unary op expr) = "(" <> prettyPrintUnOp op <> " " <> prettyPrintExpr expr <> ")"
prettyPrintExpr (Binary e1 op e2) = "(" <> prettyPrintBinOp op <> " " <> prettyPrintExpr e1 <> " " <> prettyPrintExpr e2 <> ")"
prettyPrintExpr (Grouping expr) = "(group " <> prettyPrintExpr expr <> ")"
