module Expression.AST
  ( Expression (..),
    BinaryOperator (..),
    BiOp (..),
    UnaryOperator (..),
    UnOp (..),
    Literal (..),
  )
where

{-
  This module would contain the types representing the AST of Lox.

  This is the grammar:

expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;

  This naturally translates to Haskell ADTs, so that's what we will use.
-}

data Expression
  = Literal Literal
  | Unary UnaryOperator Expression
  | Binary BinaryOperator Expression Expression
  | Grouping Expression
  deriving stock (Show)

data Literal = Number Double | String String | Bool Bool | Nil
  deriving stock (Show)

data UnaryOperator = UnaryOperator {uOpLine :: Int, uOp :: UnOp} deriving stock (Show)

data UnOp = UMinus | Bang deriving stock (Show)

data BinaryOperator = BinaryOperator {bOpLine :: Int, bOp :: BiOp} deriving stock (Show)

data BiOp
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
