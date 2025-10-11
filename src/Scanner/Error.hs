module Scanner.Error (Error (..), displayErr) where

data Error = Error
  { errorMessage :: String,
    errorLine :: Int,
    errorWhere :: String
  }
  deriving stock (Show, Eq)

displayErr :: Error -> String
displayErr (Error msg line w) = "[line " <> show line <> "] Error" <> w <> ": " <> msg
