module Token (Token (..), TokenType (..)) where

data TokenType
  = -- Single character tokens
    LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | -- One or two character tokens
    Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | -- Literals
    Identifier String
  | StringToken String
  | NumberToken Double
  | -- Keywords
    And
  | Class
  | Else
  | FalseK -- Adding the K as False is already a constructor in Haskell (for Bool)
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | TrueK -- Adding the K as True is already a constructor in Haskell (for Bool)
  | Var
  | While
  | EOF
  deriving (Show)

{-
  TODO: Add lexeme and literal to Token as in the book?
  lexeme :: String,
  literal :: ()
-}
data Token = Token
  { tokenType :: TokenType,
    line :: Int
  }
  deriving (Show)
