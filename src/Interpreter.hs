module Interpreter
  ( Interpreter,
    buildTreeWalkInterpreter,
    runInterpreter,
    programInterpreter,
    interpreterFailure,
    evaluateExpr,
    InterpreterError (..),
    handleErr,
  )
where

import Control.Monad (foldM, when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify)
import Data.Functor (($>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map qualified as M
import Environment (declareVarRef, findVarRef, newFromEnv)
import Environment.StdEnv (mkStdEnv)
import Evaluation (evalBinaryOp, evalLiteral, evalUnaryOp)
import Evaluation.Error (EvalError (EvalError))
import Expression (Expression (..), LogicalOperator (..))
import Interpreter.ControlFlow (ControlFlow (Break, Continue))
import Interpreter.Error (InterpreterError (..), handleErr)
import Program (Declaration (..), Function (..), Program (..), Statement (..), Variable (..), parseProgram)
import ProgramState qualified as PS
import Token (Token)
import Value (Callable (..), FunctionType (..), Value (VCallable, VNil), arity, displayValue, isTruthy)

type Interpreter = InterpreterT IO

buildTreeWalkInterpreter :: Either InterpreterError [Token] -> Interpreter ()
buildTreeWalkInterpreter (Left err) = interpreterFailure err
buildTreeWalkInterpreter (Right tokens) = case parseProgram tokens of
  Left errs -> interpreterFailure (Parse errs)
  Right prog -> programInterpreter prog

runInterpreter :: Interpreter a -> IO (Either InterpreterError a)
runInterpreter interpreter = do
  env <- mkStdEnv
  let programState = PS.ProgramState env env M.empty
  runInterpreter' programState interpreter

runInterpreter' :: (Monad m) => ProgramState -> InterpreterT m a -> m (Either InterpreterError a)
runInterpreter' programState interpreter = runExceptT (evalStateT (runInterpreterT interpreter) programState)

interpreterFailure :: InterpreterError -> Interpreter a
interpreterFailure = throwError

evalError :: (MonadError InterpreterError m) => Int -> String -> m a
evalError line msg = throwError (Eval (EvalError line msg))

type ProgramState = PS.ProgramState Value

newtype InterpreterT m a = Interpreter
  { runInterpreterT :: StateT ProgramState (ExceptT InterpreterError m) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState ProgramState,
      MonadError InterpreterError,
      MonadIO
    )

programInterpreter ::
  ( MonadState ProgramState m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Program -> m ()
programInterpreter (Program decls) = mapM_ interpretDecl decls

interpretDecl ::
  ( MonadState ProgramState m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Declaration -> m ()
interpretDecl (Fun function) = declareFunction function
interpretDecl (VarDecl var) = declareVariable var
interpretDecl (Statement stmt) = interpretStatement stmt

declareFunction ::
  (MonadState ProgramState m, MonadIO m) =>
  Function -> m ()
declareFunction (Function {funcName, funcParams, funcBody}) = do
  ref <- liftIO (newIORef VNil)
  modify (PS.declareEnvVarRef funcName ref)
  env <- gets PS.environment
  when (length env == 1) $ modify (PS.declareGlobalVarRef funcName ref)
  closure <- gets PS.environment
  let callable = Callable (UserDefined (Function funcName funcParams funcBody) closure)
  liftIO (writeIORef ref (VCallable callable))
  pure ()

runFunctionBody ::
  ( MonadState ProgramState m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  [Declaration] -> m Value
runFunctionBody [] = pure VNil
runFunctionBody (d : ds) =
  interpretDeclF d >>= \case
    Break v -> pure v
    Continue () -> runFunctionBody ds

interpretDeclF ::
  ( MonadState ProgramState m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Declaration -> m (ControlFlow Value ())
interpretDeclF (Statement s) = interpretStatementCF s
interpretDeclF (VarDecl v) = declareVariable v $> Continue ()
interpretDeclF (Fun f) = declareFunction f $> Continue ()

declareVariable ::
  ( MonadState ProgramState m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Variable -> m ()
declareVariable (Variable {varName, varInitializer}) = do
  value <- case varInitializer of
    Just expr -> evaluateExpr expr
    Nothing -> pure VNil -- Assuming VNil is the default uninitialized value
  ref <- liftIO (newIORef value)
  modify (PS.declareEnvVarRef varName ref)
  env <- gets PS.environment
  when (length env == 1) $ modify (PS.declareGlobalVarRef varName ref)

interpretStatement ::
  ( MonadState ProgramState m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Statement -> m ()
interpretStatement s =
  interpretStatementCF s >>= \case
    Continue () -> pure ()
    Break _ -> evalError 0 "Return statement outside of function."

interpretStatementCF ::
  ( MonadState ProgramState m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Statement -> m (ControlFlow Value ())
interpretStatementCF (PrintStmt expr) = interpretPrint expr $> Continue ()
interpretStatementCF (ExprStmt expr) = evaluateExpr expr $> Continue ()
interpretStatementCF (IfStmt expr thenBranch elseBranch) = do
  cond <- isTruthy <$> evaluateExpr expr
  if cond
    then interpretStatementCF thenBranch
    else maybe (pure (Continue ())) interpretStatementCF elseBranch
interpretStatementCF (BlockStmt decls) = do
  -- modify PS.pushEnvFrame
  previousEnv <- gets PS.environment
  modify PS.pushEnvFrame
  r <- go decls
  modify (\ps -> ps {PS.environment = previousEnv})
  pure r
  where
    go [] = pure (Continue ())
    go (d : ds) = do
      cf <- interpretDeclF d
      case cf of
        Break v -> pure (Break v)
        Continue () -> go ds
interpretStatementCF (WhileStmt expr stmt) = loop
  where
    loop = do
      c <- isTruthy <$> evaluateExpr expr
      if not c
        then pure (Continue ())
        else do
          cf <- interpretStatementCF stmt
          case cf of
            Break v -> pure (Break v)
            Continue () -> loop
interpretStatementCF (ReturnStmt maybeExpr) = Break <$> maybe (pure VNil) evaluateExpr maybeExpr

interpretPrint ::
  ( MonadState ProgramState m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Expression -> m ()
interpretPrint expr = evaluateExpr expr >>= liftIO . putStrLn . displayValue

-- | Evaluates an expression and returns a value or an error message in the monad.
evaluateExpr ::
  ( MonadState ProgramState m,
    MonadError InterpreterError m,
    MonadIO m
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
  env <- gets PS.environment
  case findVarRef name env of
    Just ref -> liftIO (readIORef ref)
    Nothing -> do
      globs <- gets PS.globals
      case findVarRef name globs of
        Just ref -> liftIO (readIORef ref)
        Nothing -> evalError line ("Undefined variable '" <> name <> "'.")
evaluateExpr (VariableAssignment line name expr) = do
  -- The variable expression needs to be evaluated *before* we retrieve the environment,
  -- else the environment will not reflect the changes made by evaluating the expression, and
  -- the right-associativity property of this operation will be broken.
  -- It's broken because I will update the environment at the end without including the changes
  -- applied to it by evaluating the expression first.
  value <- evaluateExpr expr
  env <- gets PS.environment
  case findVarRef name env of
    Just ref -> liftIO (writeIORef ref value) >> pure value
    Nothing -> do
      globs <- gets PS.globals
      case findVarRef name globs of
        Just ref -> liftIO (writeIORef ref value) >> pure value
        Nothing -> evalError line ("Undefined variable '" <> name <> "'.")
evaluateExpr (Logical _ op e1 e2) =
  evaluateExpr e1
    >>= \b -> if shortCircuits op b then pure b else evaluateExpr e2
  where
    shortCircuits Or expr = isTruthy expr
    shortCircuits And expr = not $ isTruthy expr
evaluateExpr (Call line calleeExpr argExprs) = do
  callee <- evaluateExpr calleeExpr
  args <- mapM evaluateExpr argExprs
  case callee of
    VCallable callable -> callCallable line callable args
    _ -> evalError line "Can only call functions and classes."

callCallable ::
  ( MonadError InterpreterError m,
    MonadState ProgramState m,
    MonadIO m
  ) =>
  Int ->
  Callable ->
  [Value] ->
  m Value
callCallable line callable args = do
  let expectedArity = arity callable
      actualArity = length args
   in if actualArity /= expectedArity
        then evalError line ("Expected " <> show expectedArity <> " arguments but got " <> show (length args) <> ".")
        else call callable args

call :: Callable -> [Value] -> forall m. (MonadState ProgramState m, MonadError InterpreterError m, MonadIO m) => m Value
call (Callable (UserDefined func closure)) args = do
  -- trace "Calling user-defined function" (pure ())
  -- env' <- gets PS.environment
  -- envStr <- liftIO $ renderEnvBoxes env'
  -- trace ("Current env: \n" ++ envStr) (pure ())

  let functionEnv = newFromEnv closure
      paramWithArgs = zip (funcParams func) args
  -- Set variables for the params and args in the function's environment
  updatedEnv <-
    foldM
      ( \env (paramName, argValue) -> do
          arg <- liftIO (newIORef argValue)
          pure $ declareVarRef paramName arg env
      )
      functionEnv
      paramWithArgs
  -- Save the current environment
  currentEnv <- gets PS.environment
  -- Set the function's environment as the current environment
  modify (\ps -> ps {PS.environment = updatedEnv})

  -- trace "Calling user-defined function" (pure ())
  -- env' <- gets PS.environment
  -- envStr <- liftIO $ renderEnvBoxes env'
  -- trace ("Current env: \n" ++ envStr) (pure ())

  -- Run the function body
  result <- runFunctionBody (funcBody func)
  -- Restore the previous environment
  modify (\ps -> ps {PS.environment = currentEnv})
  pure result
call (Callable (NativeFunction _ _ implementation)) args = implementation args
