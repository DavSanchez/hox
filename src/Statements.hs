module Statements (parseStatement, Statement) where

import Control.Applicative (Alternative ((<|>)))
import Expression (Expression, expression)
import Parser (TokenParser, matchTokenType)
import Token qualified as T

data Statement
  = ExprStmt Expression
  | PrintStmt Expression
  deriving stock (Show)

parseStatement :: TokenParser Statement
parseStatement = parseExprStmt <|> parsePrintStmt

parseExprStmt :: TokenParser Statement
parseExprStmt = ExprStmt <$> expression

parsePrintStmt :: TokenParser Statement
parsePrintStmt = matchTokenType T.PRINT *> (PrintStmt <$> expression) <* matchTokenType T.SEMICOLON
