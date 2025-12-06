module Resolver (programResolver, runResolver, Resolver, ResolverState (..)) where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState (..), State, gets, modify, runState)
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Expression (Expression (..))
import Program (Declaration (..), Function (..), Program (..), Statement (..), Variable (..))

data ResolverState = ResolverState
  { scopes :: NE.NonEmpty Scope,
    locals :: M.Map Expression Int -- expression -> resolved depth
  }
  deriving stock (Show, Eq)

type Scope = M.Map String Bool -- variable name and whether it's defined

newtype Resolver a = Resolver {runResolverT :: ExceptT String (State ResolverState) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState ResolverState,
      MonadError String -- TODO Replace String with a proper error type
    )

runResolver :: Resolver a -> (Either String a, ResolverState)
runResolver resolver =
  let stateAction = runState (runExceptT (runResolverT resolver))
   in stateAction newScope

-- Scope manipulation

newScope :: ResolverState
newScope = ResolverState (mempty :| []) M.empty

beginScope :: ResolverState -> ResolverState
beginScope rs = rs {scopes = mempty <| scopes rs}

endScope :: ResolverState -> ResolverState
endScope rs =
  case scopes rs of
    single@(_ :| []) -> rs {scopes = single} -- cannot pop the last scope
    (_ :| (x : xs)) -> rs {scopes = x :| xs}

declare :: String -> ResolverState -> ResolverState
declare name rs@ResolverState {scopes = currentScope :| rest} =
  let updatedScope = M.insert name False currentScope
   in rs {scopes = updatedScope :| rest}

define :: String -> ResolverState -> ResolverState
define name rs@ResolverState {scopes = currentScope :| rest} =
  let updatedScope = M.insert name True currentScope
   in rs {scopes = updatedScope :| rest}

resolveLocal :: Expression -> String -> Resolver ()
resolveLocal expr name = do
  scopesList <- gets (NE.toList . scopes)
  let findScopeIndex :: [Scope] -> Int -> Maybe Int
      findScopeIndex [] _ = Nothing
      findScopeIndex (s : ss) i =
        if M.member name s
          then Just i
          else findScopeIndex ss (i + 1)

  case findScopeIndex scopesList 0 of
    Just distance -> do
      modify $ \rs -> rs {locals = M.insert expr distance (locals rs)}
    Nothing -> pure ()

-- Resolution functions

programResolver :: Program -> Resolver ()
programResolver (Program decls) = resolveBlock decls

resolveBlock :: [Declaration] -> Resolver ()
resolveBlock block = do
  modify beginScope
  mapM_ resolveDeclaration block
  modify endScope

resolveDeclaration :: Declaration -> Resolver ()
resolveDeclaration (VarDecl var) = resolveVarDecl var
resolveDeclaration (Fun func) = resolveFuncDecl func
resolveDeclaration (Statement stmt) = resolveStatement stmt

resolveStatement :: Statement -> Resolver ()
resolveStatement (ExprStmt expr) = resolveExpr expr
resolveStatement (IfStmt cond thenBranch elseBranch) = resolveIfStmt cond thenBranch elseBranch
resolveStatement (PrintStmt expr) = resolveExpr expr
resolveStatement (ReturnStmt maybeExpr) = traverse_ resolveExpr maybeExpr
resolveStatement (WhileStmt cond body) = resolveWhileStmt cond body
resolveStatement (BlockStmt block) = resolveBlock block

resolveWhileStmt :: Expression -> Statement -> Resolver ()
resolveWhileStmt cond body = do
  resolveExpr cond
  resolveStatement body

resolveIfStmt :: Expression -> Statement -> Maybe Statement -> Resolver ()
resolveIfStmt cond thenBranch elseBranch = do
  resolveExpr cond
  resolveStatement thenBranch
  traverse_ resolveStatement elseBranch

resolveVarDecl :: Variable -> Resolver ()
resolveVarDecl (Variable vName vValue) = do
  modify (declare vName)
  traverse_ resolveExpr vValue
  modify (define vName)

resolveFuncDecl :: Function -> Resolver ()
resolveFuncDecl (Function fName fParams fBody) = do
  modify (declare fName)
  modify (define fName)
  resolveFunction fParams fBody

resolveFunction :: [String] -> [Declaration] -> Resolver ()
resolveFunction params body = do
  modify beginScope
  for_ params $ \param -> do
    modify (declare param)
    modify (define param)
  mapM_ resolveDeclaration body
  modify endScope

resolveExpr :: Expression -> Resolver ()
resolveExpr expr@(VariableExpr _ name) = do
  scopesList <- gets scopes
  let currentScope = NE.head scopesList
  case M.lookup name currentScope of
    Just False -> throwError $ "Can't read local variable in its own initializer: " ++ name
    _ -> resolveLocal expr name
resolveExpr expr@(VariableAssignment _ name value) = do
  resolveExpr value
  resolveLocal expr name
resolveExpr (BinaryOperation _ _ left right) = do
  resolveExpr left
  resolveExpr right
resolveExpr (Call _ callee args) = do
  resolveExpr callee
  mapM_ resolveExpr args
resolveExpr (Grouping expr) = resolveExpr expr
resolveExpr (Literal _) = pure ()
resolveExpr (Logical _ _ left right) = do
  resolveExpr left
  resolveExpr right
resolveExpr (UnaryOperation _ _ operand) = resolveExpr operand
