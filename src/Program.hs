module Program (Program, parseProgram, evaluate) where

import Data.Either (lefts, rights)
import Data.Void (Void)
import Parser (ParseError, Parser (runParser))
import Statements (Statement, parseStatement)
import Statements qualified as S
import Token (Token (..))
import Token qualified as T

newtype Program = Program [Statement] deriving stock (Show)

data Declaration = VarDecl Void | Statement Statement deriving stock (Show, Eq)

evaluate :: Program -> IO ()
evaluate (Program stmts) = mapM_ S.evaluate stmts

parseProgram :: [Token] -> Either [ParseError] Program
parseProgram tokens =
  let results = parseProgram' tokens
      errors = lefts results -- Collection of errors
      stmts = rights results -- Parsed program
   in if null errors
        then Right (Program stmts)
        else Left errors

parseProgram' :: [Token] -> [Either ParseError Statement]
parseProgram' [] = [] -- Should not happen, as we always expect at least EOF
parseProgram' [Token {tokenType = T.EOF}] = []
parseProgram' tokens = case runParser parseStatement tokens of
  Left err -> Left err : parseProgram' (synchronize tokens)
  Right (stmt, rest) -> Right stmt : parseProgram' rest

-- | Drop the current token and keep going until we find a statement start
synchronize :: [Token] -> [Token]
synchronize s = dropWhile (not . isStmtStart) (drop 1 s)
  where
    isStmtStart :: Token -> Bool
    isStmtStart Token {tokenType = T.CLASS} = True
    isStmtStart Token {tokenType = T.FUN} = True
    isStmtStart Token {tokenType = T.VAR} = True
    isStmtStart Token {tokenType = T.FOR} = True
    isStmtStart Token {tokenType = T.IF} = True
    isStmtStart Token {tokenType = T.WHILE} = True
    isStmtStart Token {tokenType = T.PRINT} = True
    isStmtStart Token {tokenType = T.RETURN} = True
    isStmtStart _ = False
