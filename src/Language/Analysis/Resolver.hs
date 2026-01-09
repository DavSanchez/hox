module Language.Analysis.Resolver
  ( programResolver,
    runResolver,
    Resolver,
    ResolverState (..),
    ResolveError,
    displayResolveError,
  )
where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState (..), State, gets, modify, runState)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (isJust)
import Language.Analysis.Error (ResolveError (..), displayResolveError)
import Language.Syntax.Expression (Expression (..), Phase (..), Resolution (..))
import Language.Syntax.Program (Class (..), Declaration (..), Function (..), Program (..), Statement (..), Variable (..))

data ResolverState = ResolverState
  { scopes :: NE.NonEmpty Scope,
    currentFunction :: FunctionType,
    currentClass :: ClassType
  }
  deriving stock (Show, Eq)

data FunctionType = FTypeNone | FTypeFunction | FTypeMethod | FTypeInitializer
  deriving stock (Show, Eq)

data ClassType = CTypeNone | CTypeClass | CTypeSubclass
  deriving stock (Show, Eq)

type Scope = M.Map String Bool

newtype Resolver a = Resolver {runResolverT :: ExceptT ResolveError (State ResolverState) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState ResolverState,
      MonadError ResolveError
    )

runResolver :: Resolver a -> (Either ResolveError a, ResolverState)
runResolver resolver =
  let stateAction = runState (runExceptT (runResolverT resolver))
   in stateAction newScope

newScope :: ResolverState
newScope = ResolverState (mempty :| []) FTypeNone CTypeNone

beginScope :: ResolverState -> ResolverState
beginScope rs = rs {scopes = mempty <| scopes rs}

endScope :: ResolverState -> ResolverState
endScope rs =
  case scopes rs of
    single@(_ :| []) -> rs {scopes = single}
    (_ :| (x : xs)) -> rs {scopes = x :| xs}

declare :: String -> Int -> ResolverState -> Either ResolveError ResolverState
declare name line rs@ResolverState {scopes = currentScope :| rest} =
  let alreadyDeclared = M.member name currentScope
      updatedScope = M.insert name False currentScope
      isGlobal = null rest
   in if alreadyDeclared && not isGlobal
        then Left $ ResolveError name line "Already a variable with this name in this scope."
        else Right $ rs {scopes = updatedScope :| rest}

define :: String -> ResolverState -> ResolverState
define name rs@ResolverState {scopes = currentScope :| rest} =
  let updatedScope = M.insert name True currentScope
   in rs {scopes = updatedScope :| rest}

resolveLocal :: String -> Resolver Resolution
resolveLocal name = do
  scopesList <- gets (NE.toList . scopes)
  let findScopeIndex :: [Scope] -> Int -> Maybe Int
      findScopeIndex [] _ = Nothing
      findScopeIndex (s : ss) i =
        if M.member name s
          then Just i
          else findScopeIndex ss (i + 1)

  case findScopeIndex scopesList 0 of
    Just distance -> do
      let isGlobal = distance == length scopesList - 1
      if not isGlobal
        then pure (Local distance)
        else pure Global
    Nothing -> pure Global

programResolver :: Program 'Unresolved -> Resolver (Program 'Resolved)
programResolver (Program decls) = Program <$> mapM resolveDeclaration decls

resolveBlock :: [Declaration 'Unresolved] -> Resolver [Declaration 'Resolved]
resolveBlock block = do
  modify beginScope
  decls <- mapM resolveDeclaration block
  modify endScope
  pure decls

resolveDeclaration :: Declaration 'Unresolved -> Resolver (Declaration 'Resolved)
resolveDeclaration (ClassDecl cls) = ClassDecl <$> resolveClassDecl cls
resolveDeclaration (VarDecl var) = VarDecl <$> resolveVarDecl var
resolveDeclaration (Fun func) = Fun <$> resolveFuncDecl FTypeFunction func
resolveDeclaration (Statement stmt) = Statement <$> resolveStatement stmt

withClassType :: ClassType -> Resolver a -> Resolver a
withClassType cType action = do
  oldType <- gets currentClass
  modify (\s -> s {currentClass = cType})
  res <- action
  modify (\s -> s {currentClass = oldType})
  pure res

resolveClassDecl :: Class 'Unresolved -> Resolver (Class 'Resolved)
resolveClassDecl (Class className methods line superClass) = do
  st <- get
  case declare className line st of
    Left err -> throwError err
    Right st' -> put st'
  modify (define className)

  when (isJust superClass) $ modify (\s -> s {currentClass = CTypeSubclass})
  resolvedSuperClass <- mapM resolveExpr superClass
  when (hasOwnClassName resolvedSuperClass) $ throwError (ResolveError className line "A class can't inherit from itself.")
  when (isJust resolvedSuperClass) $ modify (define "super" . beginScope)

  let classType = if isJust superClass then CTypeSubclass else CTypeClass
  methods' <- withClassType classType $ resolveClassMethods methods

  when (isJust resolvedSuperClass) $ modify endScope

  pure (Class className methods' line resolvedSuperClass)
  where
    hasOwnClassName superClassExpr = case superClassExpr of
      Just (VariableExpr _ name _) -> name == className
      _ -> False

resolveClassMethods :: (Traversable t) => t (Function 'Unresolved) -> Resolver (t (Function 'Resolved))
resolveClassMethods methods = do
  modify beginScope
  -- Bind `this` in the class scope
  modify (define "this")
  methods' <- mapM (\f -> resolveFuncDecl (if funcName f == "init" then FTypeInitializer else FTypeMethod) f) methods
  modify endScope
  pure methods'

resolveStatement :: Statement 'Unresolved -> Resolver (Statement 'Resolved)
resolveStatement (ExprStmt expr) = ExprStmt <$> resolveExpr expr
resolveStatement (IfStmt cond thenBranch elseBranch) = IfStmt <$> resolveExpr cond <*> resolveStatement thenBranch <*> traverse resolveStatement elseBranch
resolveStatement (PrintStmt expr) = PrintStmt <$> resolveExpr expr
resolveStatement (ReturnStmt line maybeExpr) = do
  fType <- gets currentFunction
  when (fType == FTypeNone) $
    throwError (ResolveError "return" line "Can't return from top-level code.")
  when (fType == FTypeInitializer) $
    case maybeExpr of
      Just _ -> throwError (ResolveError "return" line "Can't return a value from an initializer.")
      Nothing -> pure ()
  ReturnStmt line <$> traverse resolveExpr maybeExpr
resolveStatement (WhileStmt cond body) = WhileStmt <$> resolveExpr cond <*> resolveStatement body
resolveStatement (BlockStmt block) = BlockStmt <$> resolveBlock block

resolveVarDecl :: Variable 'Unresolved -> Resolver (Variable 'Resolved)
resolveVarDecl (Variable vName vValue vLine) = do
  st <- get
  case declare vName vLine st of
    Left err -> throwError err
    Right st' -> put st'
  vValue' <- traverse resolveExpr vValue
  modify (define vName)
  pure (Variable vName vValue' vLine)

withFunctionType :: FunctionType -> Resolver a -> Resolver a
withFunctionType t action = do
  oldType <- gets currentFunction
  modify (\s -> s {currentFunction = t})
  res <- action
  modify (\s -> s {currentFunction = oldType})
  pure res

resolveFuncDecl :: FunctionType -> Function 'Unresolved -> Resolver (Function 'Resolved)
resolveFuncDecl fType (Function fName fParams fBody fLine) = do
  st <- get
  case declare fName fLine st of
    Left err -> throwError err
    Right st' -> put st'
  modify (define fName)
  fBody' <- withFunctionType fType $ resolveFunction fParams fBody
  pure (Function fName fParams fBody' fLine)

resolveFunction ::
  (Traversable t) =>
  t (String, Int) ->
  t (Declaration 'Unresolved) ->
  Resolver (t (Declaration 'Resolved))
resolveFunction params body = do
  modify beginScope
  for_ params $ \(param, line) -> do
    st <- get
    case declare param line st of
      Left err -> throwError err
      Right st' -> put st'
    modify (define param)
  body' <- mapM resolveDeclaration body
  modify endScope
  pure body'

resolveExpr :: Expression 'Unresolved -> Resolver (Expression 'Resolved)
resolveExpr (VariableExpr line name _) = do
  scopesList <- gets scopes
  let currentScope = NE.head scopesList
      isGlobal = length scopesList == 1
  case M.lookup name currentScope of
    Just False | not isGlobal -> throwError $ ResolveError name line "Can't read local variable in its own initializer."
    _ -> do
      dist <- resolveLocal name
      pure (VariableExpr line name dist)
resolveExpr (VariableAssignment line name value _) = do
  value' <- resolveExpr value
  dist <- resolveLocal name
  pure (VariableAssignment line name value' dist)
resolveExpr (BinaryOperation line op left right) = BinaryOperation line op <$> resolveExpr left <*> resolveExpr right
resolveExpr (Call line callee args) = Call line <$> resolveExpr callee <*> mapM resolveExpr args
resolveExpr (Get line object propName) = Get line <$> resolveExpr object <*> pure propName
resolveExpr (Set line object propName value) = Set line <$> resolveExpr object <*> pure propName <*> resolveExpr value
resolveExpr (This line _) = do
  cType <- gets currentClass
  when (cType == CTypeNone) $
    throwError (ResolveError "this" line "Can't use 'this' outside of a class.")
  resolveLocal "this" >>= \dist -> pure (This line dist)
resolveExpr (Super line methodName _) = do
  cType <- gets currentClass
  when (cType == CTypeNone) $
    throwError $
      ResolveError "super" line "Can't use 'super' outside of a class."
  when (cType /= CTypeSubclass) $
    throwError $
      ResolveError "super" line "Can't use 'super' in a class with no superclass."
  Super line methodName <$> resolveLocal "super"
resolveExpr (Grouping expr) = Grouping <$> resolveExpr expr
resolveExpr (Literal lit) = pure (Literal lit)
resolveExpr (Logical line op left right) = Logical line op <$> resolveExpr left <*> resolveExpr right
resolveExpr (UnaryOperation line op operand) = UnaryOperation line op <$> resolveExpr operand
