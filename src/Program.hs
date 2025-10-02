module Program (Program, parseProgram, interpret) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT)
import Data.Either (lefts, rights)
import Environment (Environment)
import Parser (ParseError, Parser (runParser))
import Program.Declaration (Declaration, declaration)
import Token (Token (..))
import Token qualified as T

newtype Interpreter m a = Interpreter
  { runInterpreter :: StateT Environment m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadState Environment, MonadIO)

newtype Program = Program [Declaration] deriving stock (Show)

interpret :: (MonadIO m) => Program -> m ()
interpret (Program stmts) = pure () -- mapM_ S.evaluate stmts

parseProgram :: [Token] -> Either [ParseError] Program
parseProgram tokens =
  let results = parseProgram' tokens
      errors = lefts results -- Collection of errors
      declarations = rights results -- Parsed program
   in if null errors
        then Right (Program declarations)
        else Left errors

parseProgram' :: [Token] -> [Either ParseError Declaration]
parseProgram' [] = [] -- Should not happen, as we always expect at least EOF
parseProgram' [Token {tokenType = T.EOF}] = []
parseProgram' tokens = case runParser declaration tokens of
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
