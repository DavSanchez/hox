module Scanner.Error (SyntaxError (..), prettyPrintErr) where

data SyntaxError = SyntaxError
  { errorMessage :: String,
    errorLine :: Int,
    errorWhere :: String
  }
  deriving stock (Show, Eq)

prettyPrintErr :: SyntaxError -> String
prettyPrintErr (SyntaxError msg line w) = "[line " <> show line <> "] Error" <> w <> ": " <> msg
