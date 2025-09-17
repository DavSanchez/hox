module Statements (parseStatement, Statement, evaluate) where

import Control.Applicative (Alternative ((<|>)))
import Data.Bifunctor (Bifunctor (first))
import Error (InterpreterError (Eval), handleErr)
import Evaluation (evalExpr, printValue)
import Expression.AST (Expression)
import Expression.Parser (expression)
import Parser (TokenParser, matchTokenType)
import Token qualified as T

data Statement
  = ExprStmt Expression
  | PrintStmt Expression
  deriving stock (Show)

evaluate :: Statement -> IO ()
evaluate (ExprStmt expr) = either handleErr (const $ pure ()) $ first Eval (evalExpr expr)
evaluate (PrintStmt expr) = either handleErr (putStrLn . printValue) $ first Eval (evalExpr expr)

parseStatement :: TokenParser Statement
parseStatement = parseExprStmt <|> parsePrintStmt

-- TODO add error message to `matchTokenType` calls so I can set what I want on failure?
-- matchTokenType :: TokenType -> String -> TokenParser Token
-- or (to fall back to a default message)
-- matchTokenTypeWithError :: TokenType -> Maybe String -> TokenParser Token
parseExprStmt :: TokenParser Statement
parseExprStmt = ExprStmt <$> expression <* matchTokenType T.SEMICOLON

parsePrintStmt :: TokenParser Statement
parsePrintStmt = matchTokenType T.PRINT *> (PrintStmt <$> expression) <* matchTokenType T.SEMICOLON
