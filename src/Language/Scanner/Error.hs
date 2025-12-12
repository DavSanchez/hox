module Language.Scanner.Error (SyntaxError (..), displayErr) where

data SyntaxError = Error
  { errorMessage :: String,
    errorLine :: Int,
    errorWhere :: String
  }
  deriving stock (Show, Eq)

displayErr :: SyntaxError -> String
displayErr (Error msg line w) = "[line " <> show line <> "] Error" <> w <> ": " <> msg
