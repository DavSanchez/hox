module Grammar (Program, program) where

import Control.Applicative (Alternative (many))
import Parser (TokenParser)
import Statements (Statement, parseStatement)

newtype Program = Program [Statement] deriving stock (Show)

program :: TokenParser Program
program = Program <$> many parseStatement
