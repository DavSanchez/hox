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

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Environment (findVarRef, newEnv)
import Environment.StdEnv (mkStdEnv)
import Evaluation (evalBinaryOp, evalLiteral, evalUnaryOp)
import Evaluation.Error (EvalError (EvalError))
import Expression (Expression (..), LogicalOperator (..))
import Interpreter.ControlFlow (ControlFlow (Break, Continue))
import Interpreter.Error (InterpreterError (..), handleErr)
import Program (Declaration (..), Function (..), Program (..), Statement (..), Variable (..), parseProgram)
import ProgramState qualified as PS
import Token (Token)
import Value (Callable (..), Value (VCallable, VNil), displayValue, isTruthy)

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
  envAtDecl <- gets PS.environment
  closureRef <- liftIO (newIORef envAtDecl)
  let callable =
        Callable
          { arity = length funcParams,
            name = funcName,
            closure = Just closureRef,
            call = \args -> do
              callerEnv <- gets PS.environment
              -- Switch to the function's closure environment
              env0 <- liftIO (readIORef closureRef)
              -- If the function was declared at global scope (single frame),
              -- refresh its frame from the current caller's global frame
              -- so mutually recursive globals are visible.
              -- TODO this means that mutually recursive functions declared in nested scopes won't work
              let envForCall =
                    if NE.length env0 == 1
                      then NE.last callerEnv :| []
                      else env0
              modify $ PS.updateEnv envForCall
              modify PS.pushEnvFrame
              -- Bind parameters as new refs in the function frame
              refs <- mapM (const (liftIO (newIORef VNil))) funcParams
              traverse_ (modify . uncurry PS.declareEnvVarRef) (zip funcParams refs)
              -- Assign passed argument values into parameter refs
              traverse_ (\(ref, val) -> liftIO (writeIORef ref val)) (zip refs args)
              r <- runFunctionBody funcBody
              modify PS.popEnvFrame
              -- Persist changes to the closure environment
              envAfter <- gets PS.environment
              _ <- liftIO (writeIORef closureRef envAfter)
              -- Restore caller environment (refs share state, no merge needed)
              modify $ PS.updateEnv callerEnv
              pure r
          }
  -- Declare the function in the current environment, then update the closure to include it
  -- Function value must be stored as a ref in the environment
  fnRef <- liftIO (newIORef (VCallable callable))
  modify (PS.declareEnvVarRef funcName fnRef)
  envWithSelf <- gets PS.environment
  _ <- liftIO (writeIORef closureRef envWithSelf)
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
  modify PS.pushEnvFrame
  r <- go decls
  modify PS.popEnvFrame
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
    Nothing -> evalError line ("Undefined variable '" <> name <> "'.")
    Just ref -> liftIO (readIORef ref)
evaluateExpr (VariableAssignment line name expr) = do
  -- The variable expression needs to be evaluated *before* we retrieve the environment,
  -- else the environment will not reflect the changes made by evaluating the expression, and
  -- the right-associativity property of this operation will be broken.
  -- It's broken because I will update the environment at the end without including the changes
  -- applied to it by evaluating the expression first.
  value <- evaluateExpr expr
  env <- gets PS.environment
  case findVarRef name env of
    Nothing -> evalError line ("Undefined variable '" <> name <> "'.")
    Just ref -> liftIO (writeIORef ref value) >> pure value
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
