{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Runtime.Interpreter
  ( Interpreter,
    buildTreeWalkInterpreter,
    runInterpreter,
    programInterpreter,
    interpreterFailure,
    evaluateExpr,
    InterpreterError (..),
  )
where

import Control.Exception (Exception, catch, throwIO, try)
import Control.Monad ((>=>))
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (MonadState (get, put), gets, modify)
import Data.Functor (($>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text, pack)
import Language.Analysis.Resolver (programResolver, runResolver)
import Language.Syntax.Expression
  ( BinaryOperator (..),
    Expression (..),
    LogicalOperator (..),
    Phase (..),
    Resolution (..),
    UnaryOperator,
  )
import Language.Syntax.Program
  ( Class (..),
    Declaration (..),
    Function (..),
    Program (..),
    Statement (..),
    Variable (..),
    parseProgram,
  )
import Language.Syntax.Token (Token)
import Runtime.Environment (newFrame, readSlot, writeSlot)
import Runtime.Environment qualified as Env
import Runtime.Interpreter.ControlFlow (ControlFlow (Continue), pattern Return)
import Runtime.Interpreter.Error (InterpreterError (..))
import Runtime.Interpreter.State
  ( ProgramState (environment),
    assignVariable,
    declare,
    getVariable,
    popScope,
    pushScope,
  )
import Runtime.Interpreter.StdEnv (mkStdEnv, stdGlobalNames)
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
    lookupMethod,
    newClassInstance,
    setField,
  )

buildTreeWalkInterpreter :: Either InterpreterError [Token] -> Interpreter ()
buildTreeWalkInterpreter (Left err) = interpreterFailure err
buildTreeWalkInterpreter (Right tokens) = case parseProgram tokens of
  Left errs -> interpreterFailure (Parse errs)
  Right prog -> programInterpreter prog

runInterpreter :: (MonadIO m) => Interpreter a -> m (Either InterpreterError a)
runInterpreter interpreter = do
  programState <- mkStdEnv
  liftIO $ do
    ref <- newIORef programState
    r <- try (runInterpreterT interpreter ref)
    pure $ case r of
      Left (InterpreterException err) -> Left err
      Right a -> Right a

interpreterFailure :: InterpreterError -> Interpreter a
interpreterFailure = throwError

evalError :: (MonadError InterpreterError m) => Int -> Text -> m a
evalError line msg = throwError (Eval (EvalError line msg))

-- | The interpreter monad: a reader of the mutable 'ProgramState' over 'IO',
-- with errors raised as exceptions.
--
-- This replaced @StateT (ProgramState Value) (ExceptT InterpreterError IO)@.
-- That stack allocated an @Either@, a result pair and a lazy thunk for every
-- single bind, which dominated the allocation profile of the evaluator.
newtype Interpreter a = Interpreter
  { runInterpreterT :: IORef (ProgramState Value) -> IO a
  }

-- | Carries an 'InterpreterError' through 'IO'. Never escapes 'runInterpreter'.
newtype InterpreterException = InterpreterException InterpreterError
  deriving stock (Show)

instance Exception InterpreterException

instance Functor Interpreter where
  fmap f (Interpreter m) = Interpreter $ \r -> fmap f (m r)
  {-# INLINE fmap #-}

instance Applicative Interpreter where
  pure a = Interpreter $ \_ -> pure a
  {-# INLINE pure #-}
  Interpreter f <*> Interpreter a = Interpreter $ \r -> f r <*> a r
  {-# INLINE (<*>) #-}
  Interpreter a *> Interpreter b = Interpreter $ \r -> a r *> b r
  {-# INLINE (*>) #-}

instance Monad Interpreter where
  Interpreter m >>= k = Interpreter $ \r -> m r >>= \a -> runInterpreterT (k a) r
  {-# INLINE (>>=) #-}

instance MonadIO Interpreter where
  liftIO = Interpreter . const
  {-# INLINE liftIO #-}

instance MonadState (ProgramState Value) Interpreter where
  get = Interpreter readIORef
  {-# INLINE get #-}
  put st = Interpreter $ \r -> st `seq` writeIORef r st
  {-# INLINE put #-}

-- | Note that, unlike with the old @StateT@-over-@ExceptT@ stack, the handler
-- sees the state as it was when the error was raised, not when 'catchError'
-- was entered. Nothing in the interpreter recovers from errors, so this is
-- unobservable.
instance MonadError InterpreterError Interpreter where
  throwError err = Interpreter $ \_ -> throwIO (InterpreterException err)
  catchError (Interpreter m) handler = Interpreter $ \r ->
    m r `catch` \(InterpreterException err) -> runInterpreterT (handler err) r

programInterpreter ::
  Program 'Unresolved -> Interpreter ()
programInterpreter prog = do
  let (resolvedProg, errors) = runResolver stdGlobalNames (programResolver prog)
  if null errors
    then interpretProgram resolvedProg
    else throwError (Resolve errors)

interpretProgram ::
  Program 'Resolved -> Interpreter ()
interpretProgram (Program decls) = mapM_ interpretDecl decls

interpretDecl ::
  Declaration 'Resolved -> Interpreter ()
interpretDecl (ClassDecl cls) = declareClass cls
interpretDecl (Fun function) = declareFunction function
interpretDecl (VarDecl var) = declareVariable var
interpretDecl (Statement stmt) = interpretStatement stmt

declareClass ::
  Class 'Resolved -> Interpreter ()
declareClass cls@(Class _ _ l superClass slot) = do
  superClass' <- mapM (evaluateExpr >=> asClass) superClass
  state <- get
  declare slot VNil state
  -- Build class object
  env <- case superClass' of
    Just sC -> do
      -- The scope that binds `super`: a single slot.
      frame <- newFrame 1 VNil
      writeSlot frame 0 (VCallable (Callable (ClassConstructor sC Nothing)))
      pure (frame : environment state)
    Nothing -> pure $ environment state
  let loxClass = LoxClass cls env superClass'
      callable = Callable (ClassConstructor loxClass superClass')
  declare slot (VCallable callable) state
  where
    asClass (VCallable (Callable (ClassConstructor superC _))) = pure superC
    asClass _ = evalError l "Superclass must be a class."

declareFunction ::
  Function 'Resolved -> Interpreter ()
declareFunction func = do
  env <- gets environment
  let callable = Callable (UserDefinedFunction func env False)
  state <- get
  declare (funcSlot func) (VCallable callable) state

runFunctionBody ::
  [Declaration 'Resolved] -> Interpreter Value
runFunctionBody [] = pure VNil
runFunctionBody (d : ds) =
  interpretDeclF d >>= \case
    Return v -> pure v
    Continue () -> runFunctionBody ds

interpretDeclF ::
  Declaration 'Resolved -> Interpreter (ControlFlow Value ())
interpretDeclF (ClassDecl c) = declareClass c $> Continue ()
interpretDeclF (Statement s) = interpretStatementCF s
interpretDeclF (VarDecl v) = declareVariable v $> Continue ()
interpretDeclF (Fun f) = declareFunction f $> Continue ()

declareVariable ::
  Variable 'Resolved -> Interpreter ()
declareVariable (Variable {varInitializer, varSlot}) = do
  value <- case varInitializer of
    Just expr -> evaluateExpr expr
    Nothing -> pure VNil -- Assuming VNil is the default uninitialized value
  state <- get
  declare varSlot value state

interpretStatement ::
  Statement 'Resolved -> Interpreter ()
interpretStatement s =
  interpretStatementCF s >>= \case
    Continue () -> pure ()
    Return _ -> evalError 0 "Return statement outside of function."

interpretStatementCF ::
  Statement 'Resolved -> Interpreter (ControlFlow Value ())
interpretStatementCF (PrintStmt expr) = interpretPrint expr $> Continue ()
interpretStatementCF (ExprStmt expr) = evaluateExpr expr $> Continue ()
interpretStatementCF (IfStmt expr thenBranch elseBranch) = executeIf expr thenBranch elseBranch
interpretStatementCF (BlockStmt size decls) = executeBlock size decls
interpretStatementCF (WhileStmt expr stmt) = executeWhile expr stmt
interpretStatementCF (ReturnStmt _ maybeExpr) = Return <$> maybe (pure VNil) evaluateExpr maybeExpr

executeIf ::
  Expression 'Resolved ->
  Statement 'Resolved ->
  Maybe (Statement 'Resolved) ->
  Interpreter (ControlFlow Value ())
executeIf expr thenBranch elseBranch = do
  cond <- isTruthy <$> evaluateExpr expr
  if cond
    then interpretStatementCF thenBranch
    else case elseBranch of
      Just elseStmt -> interpretStatementCF elseStmt
      Nothing -> pure (Continue ())

executeBlock ::
  Int ->
  [Declaration 'Resolved] ->
  Interpreter (ControlFlow Value ())
executeBlock size decls = do
  state' <- get
  newState <- pushScope size VNil state'
  put newState
  -- No handler to pop the scope on error: an error aborts the whole run and
  -- the state is discarded, so there is nothing left to keep consistent.
  r <- go decls
  modify popScope
  pure r
  where
    go [] = pure (Continue ())
    go (d : ds) = do
      cf <- interpretDeclF d
      case cf of
        Return v -> pure (Return v)
        Continue () -> go ds

executeWhile ::
  Expression 'Resolved ->
  Statement 'Resolved ->
  Interpreter (ControlFlow Value ())
executeWhile expr stmt = loop
  where
    loop = do
      c <- isTruthy <$> evaluateExpr expr
      if not c
        then pure (Continue ())
        else do
          cf <- interpretStatementCF stmt
          case cf of
            Return v -> pure (Return v)
            Continue () -> loop

interpretPrint ::
  Expression 'Resolved -> Interpreter ()
interpretPrint expr = evaluateExpr expr >>= liftIO . putStrLn . displayValue

-- | Evaluates an expression and returns a value or an error message in the monad.
evaluateExpr ::
  Expression 'Resolved -> Interpreter Value
evaluateExpr (Literal lit) = pure $! evalLiteral lit
evaluateExpr (Grouping expr) = evaluateExpr expr
evaluateExpr (UnaryOperation line op e) = executeUnary line op e
evaluateExpr (BinaryOperation line op e1 e2) = executeBinary line op e1 e2
evaluateExpr (VariableExpr line name dist) = executeVariable line name dist
evaluateExpr (VariableAssignment line name expr dist) = evaluateVarAssignment line name expr dist
evaluateExpr (Logical _ op e1 e2) = evaluateLogical op e1 e2
evaluateExpr (Call line calleeExpr argExprs) = executeCall line calleeExpr argExprs
evaluateExpr (Get line objectExpr propName) = executeGet line objectExpr propName
evaluateExpr (Set line objectExpr propName valueExpr) = executeSet line objectExpr propName valueExpr
evaluateExpr (This line dist) = executeVariable line "this" (Local dist)
evaluateExpr (Super line method distSuper distThis) = do
  superClass <- executeVariable line "super" (Local distSuper)
  object <- executeVariable line "this" (Local distThis)

  case (superClass, object) of
    (VCallable (Callable (ClassConstructor cls _)), VClassInstance ins) -> do
      case lookupMethod method cls of
        Just (m, definingClass) -> VCallable <$> bindMethod m ins definingClass
        Nothing -> evalError line $ "Undefined property '" <> method <> "'."
    _ -> evalError line "Invalid use of 'super' (object or subclass mismatch)." -- flaw in my type model

executeUnary ::
  Int ->
  UnaryOperator ->
  Expression 'Resolved ->
  Interpreter Value
executeUnary line op e = do
  v <- evaluateExpr e
  either (throwError . Eval) pure (evalUnaryOp line op v)

executeBinary ::
  Int ->
  BinaryOperator ->
  Expression 'Resolved ->
  Expression 'Resolved ->
  Interpreter Value
executeBinary line op e1 e2 = do
  v1 <- evaluateExpr e1
  v2 <- evaluateExpr e2
  -- Fast path for the overwhelmingly common number-number case, which avoids
  -- allocating an Either per operation. Anything that can fail (e.g. division
  -- by zero) or isn't listed goes through 'evalBinaryOp', which defines the
  -- semantics.
  case v1 of
    VNumber a | VNumber b <- v2 -> case op of
      Plus -> pure (VNumber (a + b))
      BMinus -> pure (VNumber (a - b))
      Star -> pure (VNumber (a * b))
      Less -> pure (VBool (a < b))
      LessEqual -> pure (VBool (a <= b))
      Greater -> pure (VBool (a > b))
      GreaterEqual -> pure (VBool (a >= b))
      EqualEqual -> pure (VBool (a == b))
      BangEqual -> pure (VBool (a /= b))
      Slash -> slow v1 v2
    _ -> slow v1 v2
  where
    slow v1 v2 = either (throwError . Eval) pure (evalBinaryOp line op v1 v2)

executeVariable ::
  Int ->
  Text ->
  Resolution ->
  Interpreter Value
executeVariable line name dist = do
  state <- get
  val <- getVariable dist state
  case val of
    Just v -> pure v
    Nothing -> evalError line ("Undefined variable '" <> name <> "'.")

evaluateVarAssignment ::
  Int ->
  Text ->
  Expression 'Resolved ->
  Resolution ->
  Interpreter Value
evaluateVarAssignment line name expr dist = do
  -- The variable expression needs to be evaluated *before* we retrieve the environment,
  -- else the environment will not reflect the changes made by evaluating the expression, and
  -- the right-associativity property of this operation will be broken.
  -- It's broken because I will update the environment at the end without including the changes
  -- applied to it by evaluating the expression first.
  value <- evaluateExpr expr
  state <- get
  found <- assignVariable dist value state
  if found
    then pure value
    else evalError line ("Undefined variable '" <> name <> "'.")

evaluateLogical ::
  LogicalOperator ->
  Expression 'Resolved ->
  Expression 'Resolved ->
  Interpreter Value
evaluateLogical op e1 e2 =
  evaluateExpr e1
    >>= \b -> if shortCircuits op b then pure b else evaluateExpr e2
  where
    shortCircuits Or expr = isTruthy expr
    shortCircuits And expr = not $ isTruthy expr

executeCall ::
  Int ->
  Expression 'Resolved ->
  [Expression 'Resolved] ->
  Interpreter Value
executeCall line calleeExpr argExprs = do
  callee <- evaluateExpr calleeExpr
  args <- evaluateArgs argExprs
  case callee of
    VCallable callable -> callCallable line callable args
    _ -> evalError line "Can only call functions and classes."

-- | Evaluates call arguments left to right.
evaluateArgs :: [Expression 'Resolved] -> Interpreter [Value]
evaluateArgs [] = pure []
evaluateArgs (e : es) = do
  v <- evaluateExpr e
  vs <- evaluateArgs es
  pure (v : vs)

executeGet ::
  Int ->
  Expression 'Resolved ->
  Text ->
  Interpreter Value
executeGet line objectExpr propName = do
  objectValue <- evaluateExpr objectExpr
  case objectValue of
    VClassInstance instance' -> do
      field <- lookupField propName instance'
      case field of
        Just f -> pure f
        Nothing -> do
          let LoxClassInstance {loxClass = cls} = instance'
          case lookupMethod propName cls of
            Just (func, definingClass) -> VCallable <$> bindMethod func instance' definingClass
            Nothing -> evalError line $ "Undefined property '" <> propName <> "'."
    _ -> evalError line "Only instances have properties."

bindMethod :: Function Resolved -> LoxClassInstance -> LoxClass -> Interpreter Callable
bindMethod func clsInstance definingClass = do
  -- The scope that binds `this`: a single slot.
  newFrame' <- newFrame 1 VNil
  writeSlot newFrame' 0 (VClassInstance clsInstance)
  let closure = classClosure definingClass
      newEnv = newFrame' : closure
      isInit = funcName func == "init"
  pure (Callable (UserDefinedFunction func newEnv isInit))

-- bindMethod ::
--   ( MonadState (ProgramState Value) m,
--     MonadError InterpreterError m,
--     MonadIO m
--   ) =>
--   LoxClassInstance ->
--   String ->
--   Int ->
--   m Callable
-- bindMethod instance' method line = do
--   let LoxClassInstance {loxClass = cls, superClass = sCls} = instance'
--   case lookupMethod method (classDefinition cls) (classDefinition <$> sCls) of
--     Just func -> do
--       -- Create environment with 'this' bound to instance
--       newFrame' <- newFrame
--       Env.declareInFrame "this" (VClassInstance instance') newFrame'
--       let closure = classClosure cls
--           newEnv = newFrame' : closure
--           isInit = method == "init"
--       pure (Callable (UserDefinedFunction func newEnv isInit))
--     Nothing -> evalError line ("Undefined property '" <> method <> "'.")

executeSet ::
  Int -> Expression 'Resolved -> Text -> Expression 'Resolved -> Interpreter Value
executeSet line objectExpr propName valueExpr = do
  objectValue <- evaluateExpr objectExpr
  case objectValue of
    VClassInstance instance' -> do
      value <- evaluateExpr valueExpr
      setField propName value instance'
      pure value
    _ -> evalError line "Only instances have fields."

callCallable ::
  Int ->
  Callable ->
  [Value] ->
  Interpreter Value
callCallable line callable args
  | actualArity /= expectedArity =
      evalError line ("Expected " <> pack (show expectedArity) <> " arguments but got " <> pack (show actualArity) <> ".")
  | otherwise = call callable args
  where
    expectedArity = arity callable
    actualArity = length args

-- | Declares each parameter in the (fresh) call frame. Arity has been checked.
bindParams :: [(Text, Int)] -> [Value] -> Env.Frame Value -> Interpreter ()
bindParams params args frame = go 0 params args
  where
    -- Parameters take the first slots of the frame, in order.
    go !slot (_ : ps) (a : as) = writeSlot frame slot a >> go (slot + 1) ps as
    go _ _ _ = pure ()

call :: Callable -> [Value] -> Interpreter Value
call (Callable (UserDefinedFunction func closure isInit)) args = do
  state <- get
  frame <- newFrame (funcFrameSize func) VNil
  put state {environment = frame : closure}
  -- Set variables for the params and args in the function's environment
  bindParams (funcParams func) args frame
  -- Run the function body
  result <- runFunctionBody (funcBody func)
  -- Restore the previous environment
  modify (\ps -> ps {environment = environment state})
  if isInit
    then do
      case closure of
        -- An initializer returns `this`, which is the only slot of its frame.
        (thisFrame : _) -> readSlot thisFrame 0
        [] -> pure result
    else pure result
call (Callable (NativeFunction _ _ implementation)) args = implementation args
call (Callable (ClassConstructor loxClass superClass)) args = do
  instance' <- newClassInstance loxClass superClass
  case lookupMethod "init" loxClass of
    Just (func, _) -> do
      -- Create environment with 'this' bound to instance
      newFrame' <- newFrame 1 VNil
      writeSlot newFrame' 0 (VClassInstance instance')
      let closure = classClosure loxClass
          newEnv = newFrame' : closure
          -- isInit is True for initializer
          callable = Callable (UserDefinedFunction func newEnv True)
      call callable args
    Nothing -> pure (VClassInstance instance')
