module Grammar (Program, program, evaluate) where

import Control.Applicative (Alternative (many))
import Parser (TokenParser)
import Statements (Statement, parseStatement)
import Statements qualified as S

newtype Program = Program [Statement] deriving stock (Show)

evaluate :: Program -> IO ()
evaluate (Program stmts) = mapM_ S.evaluate stmts

program :: TokenParser Program
program = Program <$> many parseStatement
