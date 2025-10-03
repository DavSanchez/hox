module Scanner.Error (Error (..), prettyPrintErr) where

data Error = Error
  { errorMessage :: String,
    errorLine :: Int,
    errorWhere :: String
  }
  deriving stock (Show, Eq)

prettyPrintErr :: Error -> String
prettyPrintErr (Error msg line w) = "[line " <> show line <> "] Error" <> w <> ": " <> msg
