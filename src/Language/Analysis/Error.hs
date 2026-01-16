module Language.Analysis.Error (ResolveError (..), displayResolveError) where

import Data.Text (Text, unpack)

data ResolveError
  = ResolveError
      -- | Lexeme
      Text
      -- | Line number
      Int
      -- | Error message
      String
  deriving stock (Show, Eq)

displayResolveError :: ResolveError -> String
displayResolveError (ResolveError name line msg) =
  "[line " ++ show line ++ "] Error at '" ++ unpack name ++ "': " ++ msg
