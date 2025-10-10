module Environment
  ( Environment,
    declareVar,
    getVar,
    pushFrame,
    popFrame,
    newEnv,
    assignVar,
  )
where

import Control.Monad (join)
import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.Map qualified as M
import Data.Maybe (isJust)
import Value (Value)

type Environment = NonEmpty Frame

type Frame = M.Map String Value

newEnv :: Environment
newEnv = mempty :| []

pushFrame :: Environment -> Environment
pushFrame = (mempty <|) -- prepend a new empty frame

popFrame :: Environment -> Environment
popFrame single@(_ :| []) = single -- cannot pop the last frame
popFrame (_ :| (x : xs)) = x :| xs

-- Inserts the defined variable in the current environment frame (i.e. top of the stack)
-- This is only for declarations
declareVar :: String -> Value -> Environment -> Environment
declareVar name value (frame :| rest) = M.insert name value frame :| rest

-- | Assigns a variable to the environment.
--
-- This function traverses down the stack until it finds a frame that contains the variable.
--
-- If no variable is found, the environment is unchanged.
-- >>> import Data.List.NonEmpty qualified as NE
-- >>> import Data.Map qualified as M
-- >>> import Value(Value(..))
-- >>> envList = [[("x", VNumber 1), ("y", VBool True)], [], [("x", VNumber 42)], [("z", VString "hello")], []]
-- >>> env = NE.fromList $ fmap M.fromList $ envList
-- >>> env = assignVar "x" (VNumber 2) env
-- >>> (fmap M.toList . NE.toList) env

-- >>> envList = [[("x", VNumber 1), ("y", VBool True)], [], [("x", VNumber 42)], [("z", VString "hello")], []]
-- >>> env = NE.fromList $ fmap M.fromList $ envList
-- >>> env = assignVar "x" (VNumber 2) env
-- >>> (fmap M.toList . NE.toList) env
assignVar :: String -> Value -> Environment -> Environment
assignVar name value (frame :| (r : rest))
  | M.member name frame = M.insert name value frame :| (r : rest)
  | otherwise = frame <| assignVar name value (r :| rest)
assignVar name value (frame :| [])
  | M.member name frame = M.insert name value frame :| []
  | otherwise = frame :| []

getVar :: String -> Environment -> Maybe Value
getVar name env = join $ find isJust (M.lookup name <$> env)
