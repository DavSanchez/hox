module Environment (Environment, define, get) where

import Data.Map.Strict qualified as M
import Value (Value)

newtype Environment = Environment (M.Map String Value)
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

define :: String -> Value -> Environment -> Environment
define name value (Environment env) = Environment (M.insert name value env)

get :: String -> Environment -> Maybe Value
get name (Environment env) = M.lookup name env
