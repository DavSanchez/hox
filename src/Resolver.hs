{-# LANGUAGE LambdaCase #-}

module Resolver where

import Control.Monad.State (State, modify)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE
import Program (Declaration)

newtype ResolverState = ResolverState
  { scopes :: NE.NonEmpty Scope
  }

type Scope = [(String, Bool)] -- variable name and whether it's defined

-- Scope manipulation

newScope :: ResolverState
newScope = ResolverState (mempty :| [])

beginScope :: State ResolverState ()
beginScope = modify $ \rs -> rs {scopes = mempty <| scopes rs}

endScope :: State ResolverState ()
endScope = modify $ \rs ->
  case scopes rs of
    single@(_ :| []) -> ResolverState single -- cannot pop the last scope
    (_ :| (x : xs)) -> ResolverState (x :| xs)

resolveBlock :: [Declaration] -> State ResolverState ()
resolveBlock block = do
  beginScope
  mapM_ resolveDeclaration block
  endScope

resolveDeclaration :: Declaration -> State ResolverState ()
resolveDeclaration decl = undefined
