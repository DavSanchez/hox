module Runtime.Interpreter
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
import Language.Analysis.Resolver (programResolver, runResolver)
import Language.Syntax.Expression
  ( BinaryOperator,
    Expression (..),
    LogicalOperator (..),
    Resolution (..),
    UnaryOperator,
    Unresolved (..),
  )
import Language.Syntax.Program
  ( Class (..),
    Declaration (..),
    Function (..),
    Program (..),
    Statement (..),
    Variable (..),
    lookupMethod,
    parseProgram,
  )
import Language.Syntax.Token (Token)
import Runtime.Environment (newFrame)
import Runtime.Environment qualified as Env
import Runtime.Interpreter.ControlFlow (ControlFlow (Break, Continue))
import Runtime.Interpreter.Error (InterpreterError (..), handleErr)
import Runtime.Interpreter.State
  ( ProgramState (environment),
    assignVariable,
    declare,
    getVariable,
    popScope,
    pushClosureScope,
    pushScope,
  )
import Runtime.Interpreter.StdEnv (mkStdEnv)
import Runtime.Value
  ( Callable (..),
    CallableType (..),
    EvalError (EvalError),
    LoxClass (..),
    LoxClassInstance (..),
    Value (..),
    arity,
    displayValue,
    evalBinaryOp,
    evalLiteral,
    evalUnaryOp,
    isTruthy,
    lookupField,
    newClassInstance,
    setField,
  )

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

runInterpreter' :: (Monad m) => ProgramState Value -> InterpreterT m a -> m (Either InterpreterError a)
runInterpreter' programState interpreter = runExceptT (evalStateT (runInterpreterT interpreter) programState)

interpreterFailure :: InterpreterError -> Interpreter a
interpreterFailure = throwError

evalError :: (MonadError InterpreterError m) => Int -> String -> m a
evalError line msg = throwError (Eval (EvalError line msg))

newtype InterpreterT m a = Interpreter
  { runInterpreterT :: StateT (ProgramState Value) (ExceptT InterpreterError m) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState (ProgramState Value),
      MonadError InterpreterError,
      MonadIO
    )

programInterpreter ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Program Unresolved -> m ()
programInterpreter prog = do
  let (resolverResult, _) = runResolver (programResolver prog)
  case resolverResult of
    Left err -> throwError (Resolve err)
    Right (Program decls) -> mapM_ interpretDecl decls

interpretDecl ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Declaration Resolution -> m ()
interpretDecl (ClassDecl cls) = declareClass cls
interpretDecl (Fun function) = declareFunction function
interpretDecl (VarDecl var) = declareVariable var
interpretDecl (Statement stmt) = interpretStatement stmt

declareClass ::
  ( MonadState (ProgramState Value) m,
    MonadIO m
  ) =>
  Class Resolution -> m ()
declareClass cls@(Class className _ _) = do
  state <- get
  declare className VNil state
  -- Build class object
  let env = environment state
      loxClass = LoxClass cls env
      callable = Callable (ClassConstructor loxClass)
  declare className (VCallable callable) state

declareFunction ::
  ( MonadState (ProgramState Value) m,
    MonadIO m
  ) =>
  Function Resolution -> m ()
declareFunction func = do
  env <- gets environment
  let callable = Callable (UserDefinedFunction func env False)
  state <- get
  declare (funcName func) (VCallable callable) state

runFunctionBody ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  [Declaration Resolution] -> m Value
runFunctionBody [] = pure VNil
runFunctionBody (d : ds) =
  interpretDeclF d >>= \case
    Break v -> pure v
    Continue () -> runFunctionBody ds

interpretDeclF ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Declaration Resolution -> m (ControlFlow Value ())
interpretDeclF (ClassDecl c) = declareClass c $> Continue ()
interpretDeclF (Statement s) = interpretStatementCF s
interpretDeclF (VarDecl v) = declareVariable v $> Continue ()
interpretDeclF (Fun f) = declareFunction f $> Continue ()

declareVariable ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Variable Resolution -> m ()
declareVariable (Variable {varName, varInitializer}) = do
  value <- case varInitializer of
    Just expr -> evaluateExpr expr
    Nothing -> pure VNil -- Assuming VNil is the default uninitialized value
  state <- get
  declare varName value state

interpretStatement ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Statement Resolution -> m ()
interpretStatement s =
  interpretStatementCF s >>= \case
    Continue () -> pure ()
    Break _ -> evalError 0 "Return statement outside of function."

interpretStatementCF ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Statement Resolution -> m (ControlFlow Value ())
interpretStatementCF (PrintStmt expr) = interpretPrint expr $> Continue ()
interpretStatementCF (ExprStmt expr) = evaluateExpr expr $> Continue ()
interpretStatementCF (IfStmt expr thenBranch elseBranch) = executeIf expr thenBranch elseBranch
interpretStatementCF (BlockStmt decls) = executeBlock decls
interpretStatementCF (WhileStmt expr stmt) = executeWhile expr stmt
interpretStatementCF (ReturnStmt _ maybeExpr) = Break <$> maybe (pure VNil) evaluateExpr maybeExpr

executeIf ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Expression Resolution ->
  Statement Resolution ->
  Maybe (Statement Resolution) ->
  m (ControlFlow Value ())
executeIf expr thenBranch elseBranch = do
  cond <- isTruthy <$> evaluateExpr expr
  if cond
    then interpretStatementCF thenBranch
    else case elseBranch of
      Just elseStmt -> interpretStatementCF elseStmt
      Nothing -> pure (Continue ())

executeBlock ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  [Declaration Resolution] ->
  m (ControlFlow Value ())
executeBlock decls = do
  state <- get
  newState <- pushScope state
  put newState
  r <- go decls
  modify popScope
  pure r
  where
    go [] = pure (Continue ())
    go (d : ds) = do
      cf <- interpretDeclF d
      case cf of
        Break v -> pure (Break v)
        Continue () -> go ds

executeWhile ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Expression Resolution ->
  Statement Resolution ->
  m (ControlFlow Value ())
executeWhile expr stmt = loop
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

interpretPrint ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Expression Resolution -> m ()
interpretPrint expr = evaluateExpr expr >>= liftIO . putStrLn . displayValue

-- | Evaluates an expression and returns a value or an error message in the monad.
evaluateExpr ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Expression Resolution -> m Value
evaluateExpr (Literal lit) = pure $ evalLiteral lit
evaluateExpr (Grouping expr) = evaluateExpr expr
evaluateExpr (UnaryOperation line op e) = executeUnary line op e
evaluateExpr (BinaryOperation line op e1 e2) = executeBinary line op e1 e2
evaluateExpr (VariableExpr line name dist) = executeVariable line name dist
evaluateExpr (VariableAssignment line name expr dist) = evaluateVarAssignment line name expr dist
evaluateExpr (Logical _ op e1 e2) = evaluateLogical op e1 e2
evaluateExpr (Call line calleeExpr argExprs) = executeCall line calleeExpr argExprs
evaluateExpr (Get line objectExpr propName) = executeGet line objectExpr propName
evaluateExpr (Set line objectExpr propName valueExpr) = executeSet line objectExpr propName valueExpr
evaluateExpr (This line dist) = executeVariable line "this" dist

executeUnary ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Int ->
  UnaryOperator ->
  Expression Resolution ->
  m Value
executeUnary line op e = do
  v <- evaluateExpr e
  either (throwError . Eval) pure (evalUnaryOp line op v)

executeBinary ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Int ->
  BinaryOperator ->
  Expression Resolution ->
  Expression Resolution ->
  m Value
executeBinary line op e1 e2 = do
  v1 <- evaluateExpr e1
  v2 <- evaluateExpr e2
  either (throwError . Eval) pure (evalBinaryOp line op v1 v2)

executeVariable ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Int ->
  String ->
  Resolution ->
  m Value
executeVariable line name dist = do
  state <- get
  val <- getVariable name dist state
  case val of
    Just v -> pure v
    Nothing -> evalError line ("Undefined variable '" <> name <> "'.")

evaluateVarAssignment ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Int ->
  String ->
  Expression Resolution ->
  Resolution ->
  m Value
evaluateVarAssignment line name expr dist = do
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

evaluateLogical ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  LogicalOperator ->
  Expression Resolution ->
  Expression Resolution ->
  m Value
evaluateLogical op e1 e2 =
  evaluateExpr e1
    >>= \b -> if shortCircuits op b then pure b else evaluateExpr e2
  where
    shortCircuits Or expr = isTruthy expr
    shortCircuits And expr = not $ isTruthy expr

executeCall ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Int ->
  Expression Resolution ->
  [Expression Resolution] ->
  m Value
executeCall line calleeExpr argExprs = do
  callee <- evaluateExpr calleeExpr
  args <- mapM evaluateExpr argExprs
  case callee of
    VCallable callable -> callCallable line callable args
    _ -> evalError line "Can only call functions and classes."

executeGet ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Int ->
  Expression Resolution ->
  String ->
  m Value
executeGet line objectExpr propName = do
  objectValue <- evaluateExpr objectExpr
  case objectValue of
    VClassInstance instance' -> do
      field <- lookupField propName instance'
      case field of
        Just f -> pure f
        Nothing -> VCallable <$> bindMethod instance' propName line
    _ -> evalError line "Only instances have properties."

bindMethod ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  LoxClassInstance ->
  String ->
  Int ->
  m Callable
bindMethod instance' method line = do
  let cls = loxClass instance'
  case lookupMethod method (classDefinition cls) of
    Just func -> do
      -- Create environment with 'this' bound to instance
      newFrame' <- newFrame
      Env.declareInFrame "this" (VClassInstance instance') newFrame'
      let closure = classClosure cls
          newEnv = newFrame' : closure
          isInit = method == "init"
      pure (Callable (UserDefinedFunction func newEnv isInit))
    Nothing -> evalError line ("Undefined property '" <> method <> "'.")

executeSet ::
  ( MonadState (ProgramState Value) m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Int -> Expression Resolution -> String -> Expression Resolution -> m Value
executeSet line objectExpr propName valueExpr = do
  objectValue <- evaluateExpr objectExpr
  case objectValue of
    VClassInstance instance' -> do
      value <- evaluateExpr valueExpr
      setField propName value instance'
      pure value
    _ -> evalError line "Only instances have fields."

callCallable ::
  ( MonadError InterpreterError m,
    MonadState (ProgramState Value) m,
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

call :: Callable -> [Value] -> forall m. (MonadState (ProgramState Value) m, MonadError InterpreterError m, MonadIO m) => m Value
call (Callable (UserDefinedFunction func closure isInit)) args = do
  state <- get
  newState <- pushClosureScope closure state
  put newState
  let paramWithArgs = zip (funcParams func) args
  -- Set variables for the params and args in the function's environment
  mapM_
    ( \((paramName, _), argValue) -> do
        s <- get
        declare paramName argValue s
    )
    paramWithArgs
  -- Run the function body
  result <- runFunctionBody (funcBody func)
  -- Restore the previous environment
  modify (\ps -> ps {environment = environment state})
  if isInit
    then do
      case closure of
        (thisFrame : _) -> do
          maybeThis <- Env.findInFrame "this" thisFrame
          case maybeThis of
            Just thisVal -> pure thisVal
            Nothing -> pure result
        [] -> pure result
    else pure result
call (Callable (NativeFunction _ _ implementation)) args = implementation args
call (Callable (ClassConstructor loxClass)) args = do
  instance' <- newClassInstance loxClass
  let cls = classDefinition loxClass
  case lookupMethod "init" cls of
    Just func -> do
      -- Create environment with 'this' bound to instance
      newFrame' <- newFrame
      Env.declareInFrame "this" (VClassInstance instance') newFrame'
      let closure = classClosure loxClass
          newEnv = newFrame' : closure
          -- isInit is True for initializer
          callable = Callable (UserDefinedFunction func newEnv True)
      call callable args
    Nothing -> pure (VClassInstance instance')
