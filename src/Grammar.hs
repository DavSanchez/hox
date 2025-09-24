module Grammar (Program, program, evaluate) where

import Control.Applicative (Alternative (many))
import Data.Functor (void)
import Parser (TokenParser, matchTokenType)
import Statements (Statement, parseStatement)
import Statements qualified as S
import Token qualified as T

newtype Program = Program [Statement] deriving stock (Show)

evaluate :: Program -> IO ()
evaluate (Program stmts) = mapM_ S.evaluate stmts

-- This one does not work as expected because `many` will succeed if zero or more statements
-- are parsed successfully, this means that we do not have any error reporting and what is more,
-- we do not have any error recovery!! Synchronizing is not possible as of now.
program :: TokenParser Program
program = do
  stmts <- many parseStatement
  void $ matchTokenType T.EOF
  pure (Program stmts)
