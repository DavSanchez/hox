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
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Map qualified as M
import Data.Maybe (isJust)
import Value (Value)

type Environment = NonEmpty Frame

type Frame = M.Map String Value

newEnv :: Environment
newEnv = M.empty :| []

pushFrame :: Environment -> Environment
pushFrame = (mempty <|) -- prepend a new empty frame

popFrame :: Environment -> Environment
popFrame single@(_ :| []) = single -- cannot pop the last frame
popFrame (_ :| (x : xs)) = x :| xs

-- Inserts the defined variable in the current environment frame (i.e. top of the stack)
-- This is only for declarations
declareVar :: String -> Value -> Environment -> Environment
declareVar name value (frame :| rest) = M.insert name value frame :| rest

assignVar :: String -> Value -> Environment -> Environment
assignVar name value env = assignIfInFrame name value <$> env

assignIfInFrame :: String -> Value -> Frame -> Frame
assignIfInFrame name value frame
  | M.member name frame = M.insert name value frame
  | otherwise = frame

getVar :: String -> Environment -> Maybe Value
getVar name env = join $ find isJust (M.lookup name <$> env)
