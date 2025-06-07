module Scanner.Internal where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Token (Token, TokenType (..))

data SyntaxError = SyntaxError
  { errorMessage :: String,
    errorLine :: Int,
    errorWhere :: String
  }
  deriving (Show)

prettyPrint :: SyntaxError -> String
prettyPrint (SyntaxError msg line w) = "[line " <> show line <> "] Error" <> w <> ": " <> msg

type TokenResult = Either SyntaxError Token

isAlpha :: Char -> Bool
isAlpha '_' = True
isAlpha c
  | isAsciiLower c || isAsciiUpper c =
      True
isAlpha _ = False

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

identifierOrKeyword :: String -> TokenType
identifierOrKeyword "and" = AND
identifierOrKeyword "class" = CLASS
identifierOrKeyword "else" = ELSE
identifierOrKeyword "false" = FALSE
identifierOrKeyword "fun" = FUN
identifierOrKeyword "for" = FOR
identifierOrKeyword "if" = IF
identifierOrKeyword "nil" = NIL
identifierOrKeyword "or" = OR
identifierOrKeyword "print" = PRINT
identifierOrKeyword "return" = RETURN
identifierOrKeyword "super" = SUPER
identifierOrKeyword "this" = THIS
identifierOrKeyword "true" = TRUE
identifierOrKeyword "var" = VAR
identifierOrKeyword "while" = WHILE
identifierOrKeyword ident = IDENTIFIER ident
