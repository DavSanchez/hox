module Resolver (programResolver, runResolver, Resolver) where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState (..), State, modify, runState)
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Expression (Expression (..))
import Program (Declaration (..), Function (..), Program (..), Statement (..), Variable (..))

data ResolverState = ResolverState
  { scopes :: NE.NonEmpty Scope,
    locals :: M.Map String Int -- variable name -> resolved depth
  }

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
  resolveBlock body
  modify endScope

resolveExpr :: Expression -> Resolver ()
resolveExpr (VariableExpr _ lexeme) = resolveVariableExpr lexeme
resolveExpr (VariableAssignment _ lexeme valueExpr) = resolveVariableAssignment lexeme valueExpr
resolveExpr (BinaryOperation _ _ left right) = resolveBinaryOperation left right
resolveExpr (Call _ callee args) = resolveCall callee args
resolveExpr (Grouping expr) = resolveExpr expr
resolveExpr (Literal _) = pure ()
resolveExpr (Logical _ _ left right) = resolveBinaryOperation left right
resolveExpr (UnaryOperation _ _ operand) = resolveExpr operand

resolveCall :: Expression -> [Expression] -> Resolver ()
resolveCall callee args = do
  resolveExpr callee
  for_ args resolveExpr

resolveBinaryOperation :: Expression -> Expression -> Resolver ()
resolveBinaryOperation left right = do
  resolveExpr left
  resolveExpr right

resolveVariableExpr :: String -> Resolver ()
resolveVariableExpr varName = do
  ResolverState {scopes = currentScope :| _} <- get
  case M.lookup varName currentScope of
    Just False -> throwError "Can't read local variable in its own initializer."
    _ -> resolveLocal varName

resolveVariableAssignment :: String -> Expression -> Resolver ()
resolveVariableAssignment varName valueExpr = do
  resolveExpr valueExpr
  resolveLocal varName

resolveLocal :: String -> Resolver ()
resolveLocal varName = do
  ResolverState {scopes} <- get
  go scopes 0
  where
    go (scope :| rest) depth =
      if M.member varName scope
        then resolve varName depth
        else case NE.nonEmpty rest of
          Just nextScopes -> go nextScopes (depth + 1)
          Nothing -> pure () -- Not found, assume global

resolve :: String -> Int -> Resolver ()
resolve name depth = do
  modify $ \rs -> rs {locals = M.insert name depth (locals rs)}
