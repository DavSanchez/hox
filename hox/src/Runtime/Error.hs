module Runtime.Error (EvalError (..), displayEvalErr) where

import Data.Text (Text, unpack)

data EvalError = EvalError
  { errorLine :: Int,
    errorMessage :: Text
  }
  deriving stock (Show, Eq)

displayEvalErr :: EvalError -> String
displayEvalErr (EvalError line msg) = unpack msg <> "\n[line " <> show line <> "]"
