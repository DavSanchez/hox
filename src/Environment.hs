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

type Environment a = NonEmpty (Frame a)

type Frame a = M.Map String a

newEnv :: Environment a
newEnv = mempty :| []

pushFrame :: Environment a -> Environment a
pushFrame = (mempty <|) -- prepend a new empty frame

popFrame :: Environment a -> Environment a
popFrame single@(_ :| []) = single -- cannot pop the last frame
popFrame (_ :| (x : xs)) = x :| xs

-- Inserts the defined variable in the current environment frame (i.e. top of the stack)
-- This is only for declarations
declareVar :: String -> a -> Environment a -> Environment a
declareVar name value (frame :| rest) = M.insert name value frame :| rest

-- | Assigns a variable to the environment.
--
-- This function traverses down the stack until it finds a frame that contains the variable, then
-- updates the environment with the new value and returns it.
--
-- If the variable is not found, `Nothing` is returned as environment.
-- >>> import Data.List.NonEmpty qualified as NE
-- >>> import Data.Map qualified as M
-- >>> import Value(Value(..))
-- >>> envList = [[("x", VNumber 1), ("y", VBool True)], [], [("x", VNumber 42)], [("z", VString "hello")], []]
-- >>> env = NE.fromList $ fmap M.fromList $ envList
-- >>> env' = assignVar "x" (VNumber 2) env
-- >>> fmap (fmap M.toList . NE.toList) env'
-- Just [[("x",VNumber 2.0),("y",VBool True)],[],[("x",VNumber 42.0)],[("z",VString "hello")],[]]
-- >>> envList = [[("y", VBool True)], [], [("x", VNumber 42)], [("z", VString "hello")], []]
-- >>> env = NE.fromList $ fmap M.fromList $ envList
-- >>> env' = assignVar "x" (VNumber 2) env
-- >>> fmap (fmap M.toList . NE.toList) env'
-- Just [[("y",VBool True)],[],[("x",VNumber 2.0)],[("z",VString "hello")],[]]
-- >>> envList = [[("y", VBool True)], [], [("z", VString "hello")], []]
-- >>> env = NE.fromList $ fmap M.fromList $ envList
-- >>> env' = assignVar "x" (VNumber 2) env
-- >>> fmap (fmap M.toList . NE.toList) env'
-- Nothing
assignVar :: String -> a -> Environment a -> Maybe (Environment a)
assignVar name value (frame :| (r : rest))
  | M.member name frame = Just $ M.insert name value frame :| (r : rest)
  | otherwise = (frame <|) <$> assignVar name value (r :| rest)
assignVar name value (frame :| [])
  | M.member name frame = Just $ M.insert name value frame :| []
  | otherwise = Nothing

getVar :: String -> Environment a -> Maybe a
getVar name env = join $ find isJust (M.lookup name <$> env)
