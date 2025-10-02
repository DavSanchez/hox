module Program (Program, parseProgram, interpret) where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT, get, modify)
import Data.Either (lefts, rights)
import Environment (Environment, define)
import Error (InterpreterError (Eval))
import Evaluation (evalExpr)
import Parser (ParseError, Parser (runParser))
import Program.Declaration (Declaration (VarDecl), Variable (..), declaration)
import Token (Token (..))
import Token qualified as T
import Value (Value (..))

newtype Interpreter m a = Interpreter
  { runInterpreter :: ExceptT InterpreterError (StateT Environment m) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadState Environment, MonadIO, MonadError InterpreterError)

newtype Program = Program [Declaration] deriving stock (Show)

interpret :: (MonadIO m, MonadState Environment m, MonadError InterpreterError m) => Program -> m ()
interpret (Program decls) = mapM_ interpretDecl decls

interpretDecl :: (MonadIO m, MonadState Environment m, MonadError InterpreterError m) => Declaration -> m ()
interpretDecl (VarDecl (Variable {varName, varInitializer})) = do
  env <- get
  value <- case varInitializer of
    Just expr -> either (throwError . Eval) pure (evalExpr env expr)
    Nothing -> pure VNil -- Assuming VNil is the default uninitialized value
  modify (define varName value)

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
