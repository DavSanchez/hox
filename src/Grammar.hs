module Grammar () where

import Expression.AST (Expression)

newtype Program = Program [Statement] deriving stock (Show)

data Statement
  = ExprStmt Expression
  | PrintStmt Expression
  deriving stock (Show)
