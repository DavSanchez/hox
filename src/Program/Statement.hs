module Program.Statement (statement, Statement (..)) where

import Expression (Expression, expression)
import Parser (TokenParser, matchTokenType, peekToken)
import Token (Token (..))
import Token qualified as T

data Statement
  = ExprStmt Expression
  | PrintStmt Expression
  deriving stock (Show, Eq)

statement :: TokenParser Statement
statement = do
  t <- peekToken
  case tokenType t of
    T.PRINT -> parsePrintStmt
    _ -> parseExprStmt

parseExprStmt :: TokenParser Statement
parseExprStmt = ExprStmt <$> expression <* matchTokenType T.SEMICOLON

parsePrintStmt :: TokenParser Statement
parsePrintStmt = matchTokenType T.PRINT *> (PrintStmt <$> expression) <* matchTokenType T.SEMICOLON
