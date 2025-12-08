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
import Control.Monad.State (MonadState, StateT, evalStateT, get, gets, modify, put)
import Data.Functor (($>))
import Environment (assignAtDistance, assignInFrame, findInFrame, getAtDistance)
import Environment.StdEnv (mkStdEnv)
import Evaluation (evalBinaryOp, evalLiteral, evalUnaryOp)
import Evaluation.Error (EvalError (EvalError))
import Expression (Expression (..), LogicalOperator (..))
import Interpreter.ControlFlow (ControlFlow (Break, Continue))
import Interpreter.Error (InterpreterError (..), handleErr)
import Program (Declaration (..), Function (..), Program (..), Statement (..), Variable (..), parseProgram)
import ProgramState qualified as PS
import Resolver (programResolver, runResolver)
import Token (Token)
import Value (Callable (..), FunctionType (..), Value (VCallable, VNil), arity, displayValue, isTruthy)

type Interpreter = InterpreterT IO

buildTreeWalkInterpreter :: Either InterpreterError [Token] -> Interpreter ()
buildTreeWalkInterpreter (Left err) = interpreterFailure err
buildTreeWalkInterpreter (Right tokens) = case parseProgram tokens of
  Left errs -> interpreterFailure (Parse errs)
  Right prog -> programInterpreter prog

runInterpreter :: (MonadIO m) => Interpreter a -> m (Either InterpreterError a)
runInterpreter interpreter = do
  programState <- mkStdEnv
  liftIO $ runInterpreter' programState interpreter

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
programInterpreter prog = do
  let (resolverResult, _) = runResolver (programResolver prog)
  case resolverResult of
    Left err -> throwError (Resolve err)
    Right (Program decls) -> mapM_ interpretDecl decls

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
declareFunction func = do
  env <- gets PS.environment
  let callable = Callable (UserDefined func env)
  state <- get
  PS.declare (funcName func) (VCallable callable) state

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
  state <- get
  PS.declare varName value state

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
    else case elseBranch of
      Just elseStmt -> interpretStatementCF elseStmt
      Nothing -> pure (Continue ())
interpretStatementCF (BlockStmt decls) = do
  state <- get
  newState <- PS.pushScope state
  put newState
  r <- go decls
  modify PS.popScope
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
interpretStatementCF (ReturnStmt _ maybeExpr) = Break <$> maybe (pure VNil) evaluateExpr maybeExpr

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
evaluateExpr (VariableExpr line name dist) = do
  state <- get
  val <- lookupVariable name dist state
  case val of
    Just v -> pure v
    Nothing -> evalError line ("Undefined variable '" <> name <> "'.")
evaluateExpr (VariableAssignment line name expr dist) = do
  -- The variable expression needs to be evaluated *before* we retrieve the environment,
  -- else the environment will not reflect the changes made by evaluating the expression, and
  -- the right-associativity property of this operation will be broken.
  -- It's broken because I will update the environment at the end without including the changes
  -- applied to it by evaluating the expression first.
  value <- evaluateExpr expr
  state <- get
  found <- assignVariable name dist value state
  if found
    then pure value
    else evalError line ("Undefined variable '" <> name <> "'.")
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

lookupVariable :: (MonadIO m) => String -> Maybe Int -> ProgramState -> m (Maybe Value)
lookupVariable name distance st =
  let env' = PS.environment st
      globals' = PS.globals st
   in case distance of
        Just d -> getAtDistance d name env'
        Nothing -> findInFrame name globals'

assignVariable :: (MonadIO m) => String -> Maybe Int -> Value -> ProgramState -> m Bool
assignVariable name distance value st =
  let env' = PS.environment st
      globals' = PS.globals st
   in case distance of
        Just d -> assignAtDistance d name value env'
        Nothing -> assignInFrame name value globals'

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
  state <- get
  newState <- PS.pushClosureScope closure state
  put newState
  let paramWithArgs = zip (funcParams func) args
  -- Set variables for the params and args in the function's environment
  mapM_
    ( \((paramName, _), argValue) -> do
        s <- get
        PS.declare paramName argValue s
    )
    paramWithArgs
  -- Run the function body
  result <- runFunctionBody (funcBody func)
  -- Restore the previous environment
  modify (\ps -> ps {PS.environment = PS.environment state})
  pure result
call (Callable (NativeFunction _ _ implementation)) args = implementation args
