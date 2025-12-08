module Resolver
  ( programResolver,
    runResolver,
    Resolver,
    ResolverState (..),
    ResolveError,
    displayResolveError,
  )
where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState (..), State, gets, modify, runState)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Expression (Expression (..))
import Program (Declaration (..), Function (..), Program (..), Statement (..), Variable (..))

newtype ResolverState = ResolverState
  { scopes :: NE.NonEmpty Scope
  }
  deriving stock (Show, Eq)

type Scope = M.Map String Bool

data ResolveError
  = ResolveError
      -- | Lexeme
      String
      -- | Line number
      Int
      -- | Error message
      String
  deriving stock (Show, Eq)

displayResolveError :: ResolveError -> String
displayResolveError (ResolveError name line msg) =
  "[line " ++ show line ++ "] Error at '" ++ name ++ "': " ++ msg

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
newScope = ResolverState (mempty :| [])

beginScope :: ResolverState -> ResolverState
beginScope rs = rs {scopes = mempty <| scopes rs}

endScope :: ResolverState -> ResolverState
endScope rs =
  case scopes rs of
    single@(_ :| []) -> rs {scopes = single}
    (_ :| (x : xs)) -> rs {scopes = x :| xs}

declare :: String -> ResolverState -> Either ResolveError ResolverState
declare name rs@ResolverState {scopes = currentScope :| rest} =
  let alreadyDeclared = M.member name currentScope
      updatedScope = M.insert name False currentScope
      isGlobal = null rest
   in if alreadyDeclared && not isGlobal
        then Left $ ResolveError name 0 "Already a variable with this name in this scope."
        else Right $ rs {scopes = updatedScope :| rest}

define :: String -> ResolverState -> ResolverState
define name rs@ResolverState {scopes = currentScope :| rest} =
  let updatedScope = M.insert name True currentScope
   in rs {scopes = updatedScope :| rest}

resolveLocal :: String -> Resolver (Maybe Int)
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
        then pure (Just distance)
        else pure Nothing
    Nothing -> pure Nothing

programResolver :: Program -> Resolver Program
programResolver (Program decls) = Program <$> mapM resolveDeclaration decls

resolveBlock :: [Declaration] -> Resolver [Declaration]
resolveBlock block = do
  modify beginScope
  decls <- mapM resolveDeclaration block
  modify endScope
  pure decls

resolveDeclaration :: Declaration -> Resolver Declaration
resolveDeclaration (VarDecl var) = VarDecl <$> resolveVarDecl var
resolveDeclaration (Fun func) = Fun <$> resolveFuncDecl func
resolveDeclaration (Statement stmt) = Statement <$> resolveStatement stmt

resolveStatement :: Statement -> Resolver Statement
resolveStatement (ExprStmt expr) = ExprStmt <$> resolveExpr expr
resolveStatement (IfStmt cond thenBranch elseBranch) = IfStmt <$> resolveExpr cond <*> resolveStatement thenBranch <*> traverse resolveStatement elseBranch
resolveStatement (PrintStmt expr) = PrintStmt <$> resolveExpr expr
resolveStatement (ReturnStmt maybeExpr) = ReturnStmt <$> traverse resolveExpr maybeExpr
resolveStatement (WhileStmt cond body) = WhileStmt <$> resolveExpr cond <*> resolveStatement body
resolveStatement (BlockStmt block) = BlockStmt <$> resolveBlock block

resolveVarDecl :: Variable -> Resolver Variable
resolveVarDecl (Variable vName vValue) = do
  st <- get
  case declare vName st of
    Left err -> throwError err
    Right st' -> put st'
  vValue' <- traverse resolveExpr vValue
  modify (define vName)
  pure (Variable vName vValue')

resolveFuncDecl :: Function -> Resolver Function
resolveFuncDecl (Function fName fParams fBody) = do
  st <- get
  case declare fName st of
    Left err -> throwError err
    Right st' -> put st'
  modify (define fName)
  fBody' <- resolveFunction fParams fBody
  pure (Function fName fParams fBody')

resolveFunction :: [String] -> [Declaration] -> Resolver [Declaration]
resolveFunction params body = do
  modify beginScope
  for_ params $ \param -> do
    st <- get
    case declare param st of
      Left err -> throwError err
      Right st' -> put st'
    modify (define param)
  body' <- mapM resolveDeclaration body
  modify endScope
  pure body'

resolveExpr :: Expression -> Resolver Expression
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
resolveExpr (Grouping expr) = Grouping <$> resolveExpr expr
resolveExpr (Literal lit) = pure (Literal lit)
resolveExpr (Logical line op left right) = Logical line op <$> resolveExpr left <*> resolveExpr right
resolveExpr (UnaryOperation line op operand) = UnaryOperation line op <$> resolveExpr operand
