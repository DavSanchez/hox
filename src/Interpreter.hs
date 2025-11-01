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

import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get, put), StateT, evalStateT, modify)
import Data.Foldable (traverse_)
import Data.Functor (void, ($>))
import Environment
  ( Environment,
    assignVar,
    declareVar,
    getVar,
    newEnv,
    popFrame,
    pushFrame,
  )
import Evaluation (EvalError (EvalError), evalBinaryOp, evalLiteral, evalUnaryOp)
import Expression (Expression (..), LogicalOperator (..))
import Interpreter.Error (InterpreterError (..), handleErr)
import Program (Declaration (..), Function (..), Program (..), Statement (..), Variable (..), parseProgram)
import StdEnv (stdEnv)
import Token (Token)
import Value (Callable (..), Value (VCallable, VNil), displayValue, isTruthy)

type Interpreter = InterpreterT IO

buildTreeWalkInterpreter :: Either InterpreterError [Token] -> Interpreter ()
buildTreeWalkInterpreter (Left err) = interpreterFailure err
buildTreeWalkInterpreter (Right tokens) = case parseProgram tokens of
  Left errs -> interpreterFailure (Parse errs)
  Right prog -> programInterpreter prog

runInterpreter :: Interpreter a -> IO (Either InterpreterError a)
runInterpreter = runInterpreter' newEnv

runInterpreter' :: (Monad m) => Environment -> InterpreterT m a -> m (Either InterpreterError a)
runInterpreter' env interpreter = runExceptT (evalStateT (runInterpreterT interpreter) env)

interpreterFailure :: InterpreterError -> Interpreter a
interpreterFailure = throwError

evalError :: (MonadError InterpreterError m) => Int -> String -> m a
evalError line msg = throwError (Eval (EvalError line msg))

newtype InterpreterT m a = Interpreter
  { runInterpreterT :: StateT Environment (ExceptT InterpreterError m) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState Environment,
      MonadError InterpreterError,
      MonadIO
    )

programInterpreter ::
  ( MonadState Environment m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Program -> m ()
programInterpreter (Program decls) = mapM_ interpretDecl decls

interpretDecl ::
  ( MonadState Environment m,
    MonadError InterpreterError m,
    MonadIO m
  ) =>
  Declaration -> m ()
interpretDecl (Fun function) = declareFunction function
interpretDecl (VarDecl var) = declareVariable var
interpretDecl (Statement stmt) = interpretStatement stmt

declareFunction :: (MonadState Environment m) => Function -> m ()
declareFunction (Function {funcName, funcParams, funcBody}) = do
  let callable =
        Callable
          { arity = length funcParams,
            name = funcName,
            call = \args -> do
              let paramBindings = zip funcParams args -- assume param length and arg positions match
              let funcEnv = foldr (uncurry declareVar) stdEnv paramBindings
              runInterpreter' funcEnv (mapM_ interpretDecl funcBody) $> VNil
          }
  modify (declareVar funcName (VCallable callable))

declareVariable ::
  ( MonadState Environment m,
    MonadError InterpreterError m,
    MonadIO m
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
    MonadIO m
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
    MonadIO m
  ) =>
  Expression -> m ()
interpretPrint expr = evaluateExpr expr >>= liftIO . putStrLn . displayValue

-- | Evaluates an expression and returns a value or an error message in the monad.
evaluateExpr ::
  ( MonadState Environment m,
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
  env <- get
  case getVar name env of
    Nothing -> evalError line ("Undefined variable '" <> name <> "'.")
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
    Nothing -> evalError line ("Undefined variable '" <> name <> "'.") -- Variable not defined in any scope
    Just env' -> put env' >> pure value
evaluateExpr (Logical _ op e1 e2) =
  evaluateExpr e1
    >>= \b -> if shortCircuits op b then pure b else evaluateExpr e2
  where
    shortCircuits Or expr = isTruthy expr
    shortCircuits And expr = (not . isTruthy) expr
evaluateExpr (Call line calleeExpr argExprs) = do
  callee <- evaluateExpr calleeExpr
  args <- mapM evaluateExpr argExprs
  case callee of
    VCallable callable -> callCallable line callable args
    _ -> evalError line "Can only call functions and classes."

callCallable ::
  ( MonadError InterpreterError m,
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
