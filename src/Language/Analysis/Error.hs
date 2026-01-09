module Language.Analysis.Error (ResolveError (..), displayResolveError) where

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
