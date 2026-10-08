{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad.State (MonadState (..), State, gets, modify, runState)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Text (Text)
import Language.Analysis.Error (ResolveError (..), displayResolveError)
import Language.Syntax.Expression
  ( Expression (..),
    LocalResolution (..),
    Phase (..),
    Resolution (..),
  )
import Language.Syntax.Program
  ( Class (..),
    Declaration (..),
    Function (..),
    Program (..),
    Statement (..),
    Variable (..),
  )

data ResolverState = ResolverState
  { scopes :: NE.NonEmpty Scope,
    currentFunction :: FunctionType,
    currentClass :: ClassType,
    resolveErrors :: [ResolveError]
  }
  deriving stock (Show, Eq)

data FunctionType = FTypeNone | FTypeFunction | FTypeMethod | FTypeInitializer
  deriving stock (Show, Eq)

data ClassType = CTypeNone | CTypeClass | CTypeSubclass
  deriving stock (Show, Eq)

-- | What the resolver knows about a name in a scope.
data VarInfo = VarInfo
  { -- | Has the declaration finished (so the name may be read)?
    varDefined :: Bool,
    -- | Index of the name in the runtime frame of this scope. Slots are handed
    -- out in declaration order, which is also the order the interpreter
    -- executes the declarations in.
    varSlotIndex :: Int
  }
  deriving stock (Show, Eq)

type Scope = M.Map Text VarInfo

newtype Resolver a = Resolver {runResolverT :: State ResolverState a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState ResolverState
    )

-- | Runs a resolver. The given names are globals that exist before the program
-- starts (the native functions), and take the first indices, in order.
runResolver :: [Text] -> Resolver a -> (a, [ResolveError])
runResolver predefined resolver =
  let (result, finalState) = runState (runResolverT resolver) (initialState predefined)
   in (result, reverse (resolveErrors finalState))

initialState :: [Text] -> ResolverState
initialState predefined = ResolverState (globalScope :| []) FTypeNone CTypeNone []
  where
    globalScope = M.fromList [(name, VarInfo True i) | (i, name) <- zip [0 ..] predefined]

reportError :: ResolveError -> Resolver ()
reportError err = modify (\s -> s {resolveErrors = err : resolveErrors s})

beginScope :: ResolverState -> ResolverState
beginScope rs = rs {scopes = mempty <| scopes rs}

endScope :: ResolverState -> ResolverState
endScope rs =
  case scopes rs of
    single@(_ :| []) -> rs {scopes = single}
    (_ :| (x : xs)) -> rs {scopes = x :| xs}

declareSafe :: Text -> Int -> Resolver ()
declareSafe name line = do
  st <- get
  case declare name line st of
    Left err -> reportError err
    Right st' -> put st'

declare :: Text -> Int -> ResolverState -> Either ResolveError ResolverState
declare name line rs@ResolverState {scopes = currentScope :| rest} =
  let existing = M.lookup name currentScope
      alreadyDeclared = isJust existing
      -- Redeclaring (only legal for globals) keeps the original slot.
      slot = maybe (M.size currentScope) varSlotIndex existing
      updatedScope = M.insert name (VarInfo False slot) currentScope
      isGlobal = null rest
   in if alreadyDeclared && not isGlobal
        then Left $ ResolveError name line "Already a variable with this name in this scope."
        else Right $ rs {scopes = updatedScope :| rest}

define :: Text -> ResolverState -> ResolverState
define name rs@ResolverState {scopes = currentScope :| rest} =
  let slot = maybe (M.size currentScope) varSlotIndex (M.lookup name currentScope)
      updatedScope = M.insert name (VarInfo True slot) currentScope
   in rs {scopes = updatedScope :| rest}

-- | Slot of a name already declared in the innermost scope.
currentSlot :: Text -> Resolver Int
currentSlot name = gets (maybe 0 varSlotIndex . M.lookup name . NE.head . scopes)

-- | Number of slots the innermost scope needs at runtime.
currentScopeSize :: Resolver Int
currentScopeSize = gets (M.size . NE.head . scopes)

-- | Index of a global in the table of globals, assigning the next free one the
-- first time a name is seen. A global can be mentioned before it is declared
-- (that is a runtime error, not a resolve error), so mentions intern it too.
globalSlot :: Text -> Resolver Int
globalSlot name = do
  rs <- get
  let globalScope = NE.last (scopes rs)
  case M.lookup name globalScope of
    Just info -> pure (varSlotIndex info)
    Nothing -> do
      let slot = M.size globalScope
      put rs {scopes = modifyLast (M.insert name (VarInfo False slot)) (scopes rs)}
      pure slot

-- | Applies a function to the last (outermost) element.
modifyLast :: (a -> a) -> NonEmpty a -> NonEmpty a
modifyLast f (x :| []) = f x :| []
modifyLast f (x :| (y : ys)) = x :| NE.toList (modifyLast f (y :| ys))

resolveLocal :: Text -> Resolver Resolution
resolveLocal name = do
  scopesList <- gets (NE.toList . scopes)
  let findScope :: [Scope] -> Int -> Maybe (Int, Int)
      findScope [] _ = Nothing
      findScope (sc : ss) i = case M.lookup name sc of
        Just info -> Just (i, varSlotIndex info)
        Nothing -> findScope ss (i + 1)

  case findScope scopesList 0 of
    Just (distance, slot) ->
      -- The outermost scope is the global one, which has its own table.
      if distance == length scopesList - 1
        then pure (Global slot)
        else pure (Local (LocalResolution distance slot))
    Nothing -> Global <$> globalSlot name

programResolver :: Program 'Unresolved -> Resolver (Program 'Resolved)
programResolver (Program decls) = Program <$> mapM resolveDeclaration decls

-- | Resolves a block in a new scope, returning the size of its frame.
resolveBlock :: [Declaration 'Unresolved] -> Resolver (Int, [Declaration 'Resolved])
resolveBlock block = do
  modify beginScope
  decls <- mapM resolveDeclaration block
  size <- currentScopeSize
  modify endScope
  pure (size, decls)

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
resolveClassDecl (Class className methods line superClass _) = do
  declareSafe className line
  modify (define className)
  slot <- currentSlot className

  when (isJust superClass) $ modify (\s -> s {currentClass = CTypeSubclass})
  resolvedSuperClass <- mapM resolveExpr superClass
  when (hasOwnClassName resolvedSuperClass) $ reportError (ResolveError className line "A class can't inherit from itself.")
  when (isJust resolvedSuperClass) $ modify (define "super" . beginScope)

  let classType = if isJust superClass then CTypeSubclass else CTypeClass
  methods' <- withClassType classType $ resolveClassMethods methods

  when (isJust resolvedSuperClass) $ modify endScope

  pure (Class className methods' line resolvedSuperClass slot)
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
    reportError (ResolveError "return" line "Can't return from top-level code.")
  when (fType == FTypeInitializer) $
    case maybeExpr of
      Just _ -> reportError (ResolveError "return" line "Can't return a value from an initializer.")
      Nothing -> pure ()
  ReturnStmt line <$> traverse resolveExpr maybeExpr
resolveStatement (WhileStmt cond body) = WhileStmt <$> resolveExpr cond <*> resolveStatement body
resolveStatement (BlockStmt _ block) = uncurry BlockStmt <$> resolveBlock block

resolveVarDecl :: Variable 'Unresolved -> Resolver (Variable 'Resolved)
resolveVarDecl (Variable vName vValue vLine _) = do
  declareSafe vName vLine
  slot <- currentSlot vName
  vValue' <- traverse resolveExpr vValue
  modify (define vName)
  pure (Variable vName vValue' vLine slot)

withFunctionType :: FunctionType -> Resolver a -> Resolver a
withFunctionType t action = do
  oldType <- gets currentFunction
  modify (\s -> s {currentFunction = t})
  res <- action
  modify (\s -> s {currentFunction = oldType})
  pure res

resolveFuncDecl :: FunctionType -> Function 'Unresolved -> Resolver (Function 'Resolved)
resolveFuncDecl fType (Function fName fParams fBody fLine _ _) = do
  -- Methods are not variables of the scope that binds `this`: they are looked
  -- up through the instance. Declaring them there would make a bare reference
  -- to a global of the same name resolve to the wrong place.
  slot <-
    if isMethod fType
      then pure 0
      else do
        declareSafe fName fLine
        modify (define fName)
        currentSlot fName
  (frameSize, fBody') <- withFunctionType fType $ resolveFunction fParams fBody
  pure (Function fName fParams fBody' fLine frameSize slot)
  where
    isMethod t = t == FTypeMethod || t == FTypeInitializer

-- | Resolves parameters and body in one scope (they share the call frame),
-- returning the size of that frame.
resolveFunction ::
  [(Text, Int)] ->
  [Declaration 'Unresolved] ->
  Resolver (Int, [Declaration 'Resolved])
resolveFunction params body = do
  modify beginScope
  for_ params $ \(param, line) -> do
    declareSafe param line
    modify (define param)
  body' <- mapM resolveDeclaration body
  size <- currentScopeSize
  modify endScope
  pure (size, body')

resolveExpr :: Expression 'Unresolved -> Resolver (Expression 'Resolved)
resolveExpr (VariableExpr line name _) = do
  scopesList <- gets scopes
  let currentScope = NE.head scopesList
      isGlobal = length scopesList == 1
  case M.lookup name currentScope of
    Just (VarInfo False _) | not isGlobal -> reportError (ResolveError name line "Can't read local variable in its own initializer.")
    _ -> pure ()

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
    reportError (ResolveError "this" line "Can't use 'this' outside of a class.")
  dist <- resolveLocal "this"
  pure (This line (toLocal dist))
resolveExpr (Super line methodName _ _) = do
  cType <- gets currentClass
  case cType of
    CTypeNone -> reportError (ResolveError "super" line "Can't use 'super' outside of a class.")
    CTypeClass -> reportError (ResolveError "super" line "Can't use 'super' in a class with no superclass.")
    CTypeSubclass -> pure ()
  distSuper <- resolveLocal "super"
  distThis <- resolveLocal "this"
  pure (Super line methodName (toLocal distSuper) (toLocal distThis))
resolveExpr (Grouping expr) = Grouping <$> resolveExpr expr
resolveExpr (Literal lit) = pure (Literal lit)
resolveExpr (Logical line op left right) = Logical line op <$> resolveExpr left <*> resolveExpr right
resolveExpr (UnaryOperation line op operand) = UnaryOperation line op <$> resolveExpr operand

-- | Coverts a computed global-aware resolution to a local-only one.
--
-- Intended to be used for `this` and `super` variants where
-- global resolution is not applicable (cannot happen).
toLocal :: Resolution -> LocalResolution
toLocal (Local n) = n
toLocal (Global _) = LocalResolution 0 0
