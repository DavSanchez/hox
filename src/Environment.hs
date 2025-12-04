module Environment
  ( Environment,
    declareVarRef,
    getVarRef,
    pushFrame,
    popFrame,
    newEnv,
    findVarRef,
    newFromEnv,
  )
where

import Control.Monad (join)
import Data.Foldable (find)
import Data.IORef (IORef)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.Map qualified as M
import Data.Maybe (isJust)

type Environment a = NonEmpty (Frame a)

type Frame a = M.Map String (IORef a)

newEnv :: Environment a
newEnv = mempty :| []

newFromEnv :: Environment a -> Environment a
newFromEnv = pushFrame

pushFrame :: Environment a -> Environment a
pushFrame = (mempty <|) -- prepend a new empty frame

popFrame :: Environment a -> Environment a
popFrame single@(_ :| []) = single -- cannot pop the last frame
popFrame (_ :| (x : xs)) = x :| xs

-- Inserts the defined variable ref in the current environment frame (i.e. top of the stack)
-- This is only for declarations
declareVarRef :: String -> IORef a -> Environment a -> Environment a
declareVarRef name ref (frame :| rest) = M.insert name ref frame :| rest

-- | Finds the variable reference in the environment.
-- Traverses down the stack until it finds a frame that contains the variable,
-- and returns its IORef. If not found, returns Nothing.
findVarRef :: String -> Environment a -> Maybe (IORef a)
findVarRef name (frame :| (r : rest))
  | M.member name frame = M.lookup name frame
  | otherwise = findVarRef name (r :| rest)
findVarRef name (frame :| []) = M.lookup name frame

getVarRef :: String -> Environment a -> Maybe (IORef a)
getVarRef name env = join $ find isJust (M.lookup name <$> env)
