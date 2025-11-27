module Evaluation.Error (EvalError (..), displayEvalErr) where

data EvalError = EvalError
  { errorLine :: Int,
    errorMessage :: String
  }
  deriving stock (Show, Eq)

displayEvalErr :: EvalError -> String
displayEvalErr (EvalError line msg) = msg <> "\n[line " <> show line <> "]"
