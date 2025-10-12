module Interpreter
  ( Interpreter,
    buildTreeWalkInterpreter,
    runInterpreter,
    programInterpreter,
    interpreterFailure,
    runNoIOInterpreter,
    evaluateExpr,
  )
where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT, evalStateT, modify)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Environment
  ( Environment,
    assignVar,
    declareVar,
    getVar,
    newEnv,
    popFrame,
    pushFrame,
  )
import Error (InterpreterError (Eval, Parse))
import Evaluation (EvalError (EvalError), evalBinaryOp, evalLiteral, evalUnaryOp)
import Expression (Expression (..), LogicalOperator (..))
import Program (Declaration (..), Program (..), Statement (..), Variable (..), parseProgram)
import Token (Token)
import Value (Value (VNil), displayValue, isTruthy)

type Interpreter = InterpreterT IO

buildTreeWalkInterpreter :: Either InterpreterError [Token] -> Interpreter ()
buildTreeWalkInterpreter (Left err) = interpreterFailure err
buildTreeWalkInterpreter (Right tokens) = case parseProgram tokens of
  Left errs -> interpreterFailure (Parse errs)
  Right prog -> programInterpreter prog

runInterpreter :: Interpreter a -> IO (Either InterpreterError a)
runInterpreter interpreter = runExceptT (evalStateT (runInterpreterT interpreter) newEnv)

interpreterFailure :: InterpreterError -> Interpreter a
interpreterFailure = throwError

newtype InterpreterT m a = Interpreter
  { runInterpreterT :: StateT Environment (ExceptT InterpreterError m) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState Environment,
      MonadError InterpreterError,
      MonadPrinter
    )

class (Monad m) => MonadPrinter m where
  printLn :: String -> m ()

-- Real world
instance MonadPrinter IO where
  printLn :: String -> IO ()
  printLn = putStrLn

instance (MonadPrinter m) => MonadPrinter (StateT s m) where
  printLn :: String -> StateT s m ()
  printLn = lift . printLn

instance (MonadPrinter m) => MonadPrinter (ExceptT e m) where
  printLn :: String -> ExceptT e m ()
  printLn = lift . printLn

-- | A version of the interpreter that runs without any IO capabilities, useful for testing.
-- Used for evaluating expressions on previous chapters.
type NoIOInterpreter = InterpreterT Identity

-- | Runs a no-IO interpreter with an initial empty environment.
-- Returns either an interpreter error or the computed value.
-- Used for evaluating expressions on previous chapters.
runNoIOInterpreter :: NoIOInterpreter a -> Either InterpreterError a
runNoIOInterpreter interpreter = runIdentity $ runExceptT (evalStateT (runInterpreterT interpreter) newEnv)

programInterpreter ::
  ( MonadState Environment m,
    MonadError InterpreterError m,
    MonadPrinter m
  ) =>
  Program -> m ()
programInterpreter (Program decls) = mapM_ interpretDecl decls

interpretDecl ::
  ( MonadState Environment m,
    MonadError InterpreterError m,
    MonadPrinter m
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
  value <- case varInitializer of
    Just expr -> evaluateExpr expr
    Nothing -> pure VNil -- Assuming VNil is the default uninitialized value
  modify (declareVar varName value)

interpretStatement ::
  ( MonadState Environment m,
    MonadError InterpreterError m,
    MonadPrinter m
  ) =>
  Statement -> m ()
interpretStatement (PrintStmt expr) = interpretPrint expr
interpretStatement (ExprStmt expr) = void $ evaluateExpr expr
interpretStatement (IfStmt expr thenBranch elseBranch) = do
  cond <- isTruthy <$> evaluateExpr expr
  if cond
    then interpretStatement thenBranch
    else traverse_ interpretStatement elseBranch
interpretStatement (BlockStmt decls) =
  modify pushFrame
    -- TODO: are blocks expressions that resolve to a value?
    >> mapM_ interpretDecl decls `catchError` (\e -> modify popFrame >> throwError e)
    >> modify popFrame
interpretStatement while@(WhileStmt expr stmt) =
  evaluateExpr expr
    >>= flip when (interpretStatement stmt >> interpretStatement while) . isTruthy

interpretPrint ::
  ( MonadState Environment m,
    MonadError InterpreterError m,
    MonadPrinter m
  ) =>
  Expression -> m ()
interpretPrint expr = evaluateExpr expr >>= printLn . displayValue

-- | Evaluates an expression and returns a value or an error message in the monad.
evaluateExpr ::
  ( MonadState Environment m,
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
  case getVar name env of
    Nothing ->
      throwError $
        Eval $
          EvalError line ("Undefined variable '" <> name <> "'.")
    Just value -> pure value
evaluateExpr (VariableAssignment line name expr) = do
  -- The variable expression needs to be evaluated *before* we retrieve the environment,
  -- else the environment will not reflect the changes made by evaluating the expression, and
  -- the right-associativity property of this operation will be broken.
  -- It's broken because I will update the environment at the end without including the changes
  -- applied to it by evaluating the expression first.
  value <- evaluateExpr expr
  env <- get
  case assignVar name value env of
    Nothing ->
      -- Variable is not defined in any scope
      throwError $ Eval $ EvalError line ("Undefined variable '" <> name <> "'.")
    Just env' -> put env' >> pure value
evaluateExpr (Logical _ op e1 e2) =
  evaluateExpr e1
    >>= \b -> if shortCircuits op b then pure b else evaluateExpr e2
  where
    shortCircuits Or expr = isTruthy expr
    shortCircuits And expr = (not . isTruthy) expr
