module Language.Scanner.Error (SyntaxError (..), displaySyntaxError) where

data SyntaxError = Error
  { errorMessage :: String,
    errorLine :: Int,
    errorWhere :: String
  }
  deriving stock (Show, Eq)

displaySyntaxError :: SyntaxError -> String
displaySyntaxError (Error msg line w) = "[line " <> show line <> "] Error" <> w <> ": " <> msg
