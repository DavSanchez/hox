module Statements (parseStatement, Statement, evaluate) where

import Data.Bifunctor (Bifunctor (first))
import Error (InterpreterError (Eval), handleErr)
import Evaluation (evalExpr, printValue)
import Expression (Expression, expression)
import Parser (TokenParser, matchTokenType, peekToken)
import Token (Token (..))
import Token qualified as T

data Statement
  = ExprStmt Expression
  | PrintStmt Expression
  deriving stock (Show, Eq)

evaluate :: Statement -> IO ()
evaluate (ExprStmt expr) = either handleErr (const $ pure ()) $ first Eval (evalExpr expr)
evaluate (PrintStmt expr) = either handleErr (putStrLn . printValue) $ first Eval (evalExpr expr)

parseStatement :: TokenParser Statement
parseStatement = do
  t <- peekToken
  case tokenType t of
    T.PRINT -> parsePrintStmt
    _ -> parseExprStmt

parseExprStmt :: TokenParser Statement
parseExprStmt = ExprStmt <$> expression <* matchTokenType T.SEMICOLON

parsePrintStmt :: TokenParser Statement
parsePrintStmt = matchTokenType T.PRINT *> (PrintStmt <$> expression) <* matchTokenType T.SEMICOLON
