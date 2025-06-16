module Representation () where

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
