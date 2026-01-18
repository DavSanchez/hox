module Language.Scanner.Types (TokenResult, SyntaxError (..), displaySyntaxError) where

import Language.Scanner.Error (SyntaxError (..), displaySyntaxError)
import Language.Syntax.Token (Token)

type TokenResult = Either SyntaxError Token
