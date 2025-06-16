module Scanner.Internal
  ( SyntaxError (..),
    TokenResult,
    isAlpha,
    isAlphaNum,
    keywords,
    syntaxError,
    prettyPrint,
    validToken,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Token (Token (..), TokenType (..))

data SyntaxError = SyntaxError
  { errorMessage :: String,
    errorLine :: Int,
    errorWhere :: String
  }
  deriving stock (Show)

prettyPrint :: SyntaxError -> String
prettyPrint (SyntaxError msg line w) = "[line " <> show line <> "] Error" <> w <> ": " <> msg

type TokenResult = Either SyntaxError Token

validToken :: TokenType -> Int -> TokenResult
validToken tokenType line =
  Right (Token tokenType line)

syntaxError :: String -> Int -> String -> TokenResult
syntaxError msg line whereStr =
  Left (SyntaxError msg line whereStr)

isAlpha :: Char -> Bool
isAlpha '_' = True
isAlpha c
  | isAsciiLower c || isAsciiUpper c =
      True
isAlpha _ = False

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

keywords :: [(String, TokenType)]
keywords =
  [ ("and", AND),
    ("class", CLASS),
    ("else", ELSE),
    ("false", FALSE),
    ("fun", FUN),
    ("for", FOR),
    ("if", IF),
    ("nil", NIL),
    ("or", OR),
    ("print", PRINT),
    ("return", RETURN),
    ("super", SUPER),
    ("this", THIS),
    ("true", TRUE),
    ("var", VAR),
    ("while", WHILE)
  ]
