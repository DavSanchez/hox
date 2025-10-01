module Value (Value (..), printValue) where

import Data.Char (toLower)
import Numeric (showFFloat)

-- | Represents the values that can be produced by evaluating an expression.
data Value
  = VNumber Double
  | VBool Bool
  | VString String
  | VNil
  deriving stock (Show, Eq)

-- | Pretty prints a value according to the Crafting Interpreters book.
-- >>> printValue <$> evalExpr (Literal (Number (-0.0)))
-- Right "-0"
printValue :: Value -> String
printValue (VNumber n) =
  let (integer :: Integer, decimal) = properFraction n
   in if decimal == 0
        then if isNegativeZero n then "-0" else show integer
        else showFFloat Nothing n "" -- Otherwise, print as floating-point number
printValue (VBool b) = (map toLower . show) b
printValue (VString s) = s
printValue VNil = "nil"
