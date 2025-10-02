module Interpreter (Interpreter, runInterpreter, programInterpreter, interpreterFailure, runNoIOInterpreter, evaluateExpr) where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (get, put), StateT, evalStateT, modify)
import Data.Functor (void)
import Environment qualified as Env
import Error (InterpreterError (Eval))
import Evaluation (EvalError (EvalError), evalBinaryOp, evalLiteral, evalUnaryOp)
import Expression (Expression (..))
import Program (Declaration (..), Program (..), Statement (..), Variable (..))
import Value (Value (VNil), printValue)

type Interpreter = InterpreterT IO

runInterpreter :: Interpreter a -> IO (Either InterpreterError a)
runInterpreter interpreter = evalStateT (runExceptT (runInterpreterT interpreter)) mempty

interpreterFailure :: InterpreterError -> Interpreter a
interpreterFailure = throwError

newtype InterpreterT m a = Interpreter
  { runInterpreterT :: ExceptT InterpreterError (StateT Env.Environment m) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState Env.Environment,
      MonadIO,
      MonadError InterpreterError
    )

-- | A version of the interpreter that runs without any IO capabilities, useful for testing.
-- Used for evaluating expressions on previous chapters.
type NoIOInterpreter = InterpreterT Identity

-- | Runs a no-IO interpreter with an initial empty environment.
-- Returns either an interpreter error or the computed value.
-- Used for evaluating expressions on previous chapters.
runNoIOInterpreter :: NoIOInterpreter a -> Either InterpreterError a
runNoIOInterpreter interpreter = runIdentity $ evalStateT (runExceptT (runInterpreterT interpreter)) mempty

programInterpreter ::
  ( MonadIO m,
    MonadState Env.Environment m,
    MonadError InterpreterError m
  ) =>
  Program -> m ()
programInterpreter (Program decls) = mapM_ interpretDecl decls

interpretDecl ::
  ( MonadIO m,
    MonadState Env.Environment m,
    MonadError InterpreterError m
  ) =>
  Declaration -> m ()
interpretDecl (VarDecl var) = declareVariable var
interpretDecl (Statement stmt) = interpretStatement stmt

declareVariable ::
  ( MonadState Env.Environment m,
    MonadError InterpreterError m
  ) =>
  Variable -> m ()
declareVariable (Variable {varName, varInitializer}) = do
  value <- case varInitializer of
    Just expr -> evaluateExpr expr
    Nothing -> pure VNil -- Assuming VNil is the default uninitialized value
  modify (Env.define varName value)

interpretStatement ::
  ( MonadState Env.Environment m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Statement -> m ()
interpretStatement (PrintStmt expr) = interpretPrint expr
interpretStatement (ExprStmt expr) = void $ evaluateExpr expr
-- How does this receive a copy of the environment that does not clash with the parent one?
interpretStatement (BlockStmt decls) = do
  -- Get parent environment
  parentEnv <- get
  -- Run the actions (TODO: are blocks expressions that resolve to a value?)
  mapM_ interpretDecl decls
  -- Restore the environment
  put parentEnv

interpretPrint ::
  ( MonadState Env.Environment m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Expression -> m ()
interpretPrint expr = evaluateExpr expr >>= liftIO . putStrLn . printValue

-- | Evaluates an expression and returns a value or an error message in the monad.
evaluateExpr ::
  ( MonadState Env.Environment m,
    MonadError InterpreterError m
  ) =>
  Expression -> m Value
evaluateExpr (Literal lit) = pure $ evalLiteral lit
evaluateExpr (Grouping expr) = evaluateExpr expr
evaluateExpr (UnaryOperation line op e) = do
  v <- evaluateExpr e
  either (throwError . Eval) pure (evalUnaryOp line op v)
evaluateExpr (BinaryOperation line op e1 e2) = do
  v1 <- evaluateExpr e1
  v2 <- evaluateExpr e2
  either (throwError . Eval) pure (evalBinaryOp line op v1 v2)
evaluateExpr (VariableExpr line name) = do
  env <- get
  case Env.get name env of
    Nothing ->
      throwError $
        Eval $
          EvalError line ("Undefined variable '" <> name <> "'.")
    Just value -> pure value
evaluateExpr (VariableAssignment line name expr) = do
  env <- get
  value <- evaluateExpr expr
  case Env.get name env of
    Nothing ->
      throwError $
        Eval $
          EvalError line ("Undefined variable '" <> name <> "'.")
    Just _ -> modify (Env.define name value) >> pure value
