module Interpreter (Interpreter, runInterpreter, programInterpreter, interpreterFailure) where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get), StateT, evalStateT, modify)
import Environment (Environment, define)
import Error (InterpreterError (Eval))
import Evaluation (evalExpr)
import Expression (Expression)
import Program (Program (..))
import Program.Declaration
import Program.Statement (Statement (..))
import Value (Value (VNil), printValue)

type Interpreter = InterpreterT IO

runInterpreter :: Interpreter a -> IO (Either InterpreterError a)
runInterpreter interpreter = evalStateT (runExceptT (runInterpreterT interpreter)) mempty

interpreterFailure :: InterpreterError -> Interpreter a
interpreterFailure = throwError

newtype InterpreterT m a = Interpreter
  { runInterpreterT :: ExceptT InterpreterError (StateT Environment m) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState Environment,
      MonadIO,
      MonadError InterpreterError
    )

programInterpreter ::
  ( MonadIO m,
    MonadState Environment m,
    MonadError InterpreterError m
  ) =>
  Program -> m ()
programInterpreter (Program decls) = mapM_ interpretDecl decls

interpretDecl ::
  ( MonadIO m,
    MonadState Environment m,
    MonadError InterpreterError m
  ) =>
  Declaration -> m ()
interpretDecl (VarDecl var) = declareVariable var
interpretDecl (Statement stmt) = interpretStatement stmt

declareVariable ::
  ( MonadState Environment m,
    MonadError InterpreterError m
  ) =>
  Variable -> m ()
declareVariable (Variable {varName, varInitializer}) = do
  env <- get
  value <- case varInitializer of
    Just expr -> either (throwError . Eval) pure (evalExpr env expr)
    Nothing -> pure VNil -- Assuming VNil is the default uninitialized value
  modify (define varName value)

interpretStatement ::
  ( MonadState Environment m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Statement -> m ()
interpretStatement (PrintStmt expr) = interpretPrint expr
interpretStatement (ExprStmt expr) = do
  env <- get
  _ <- either (throwError . Eval) pure (evalExpr env expr)
  pure ()

interpretPrint ::
  ( MonadState Environment m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Expression -> m ()
interpretPrint expr = do
  env <- get
  value <- either (throwError . Eval) pure (evalExpr env expr)
  liftIO $ putStrLn $ printValue value
