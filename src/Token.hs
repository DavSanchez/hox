module Token
  ( Token (..),
    TokenType (..),
    prettyPrintToken,
    isNumber,
    isString,
  )
where

data TokenType
  = -- Single character tokens
    LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | -- One or two character tokens
    BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | -- Literals
    IDENTIFIER Lexeme
  | STRING Lexeme String
  | NUMBER Lexeme Double
  | -- Keywords
    AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF
  deriving stock (Show, Eq)

isNumber :: TokenType -> Bool
isNumber (NUMBER _ _) = True
isNumber _ = False

isString :: TokenType -> Bool
isString (STRING _ _) = True
isString _ = False

type Lexeme = String

data Token = Token
  { tokenType :: TokenType,
    line :: Int
  }
  deriving stock (Show, Eq)

-- |  Pretty print a token the way the book expects
-- >>> prettyPrintToken (Token (IDENTIFIER "foo") 1)
-- "IDENTIFIER foo null"
-- >>> prettyPrintToken (Token (NUMBER "42" 42.0) 1)
-- "NUMBER 42 42.0"
prettyPrintToken :: Token -> String
prettyPrintToken (Token (IDENTIFIER s) _) = "IDENTIFIER " <> s <> " null"
prettyPrintToken (Token (STRING l s) _) = "STRING " <> l <> " " <> s
prettyPrintToken (Token (NUMBER l n) _) = "NUMBER " <> l <> " " <> show n
prettyPrintToken (Token LEFT_PAREN _) = "LEFT_PAREN ( null"
prettyPrintToken (Token RIGHT_PAREN _) = "RIGHT_PAREN ) null"
prettyPrintToken (Token LEFT_BRACE _) = "LEFT_BRACE { null"
prettyPrintToken (Token RIGHT_BRACE _) = "RIGHT_BRACE } null"
prettyPrintToken (Token COMMA _) = "COMMA , null"
prettyPrintToken (Token DOT _) = "DOT . null"
prettyPrintToken (Token MINUS _) = "MINUS - null"
prettyPrintToken (Token PLUS _) = "PLUS + null"
prettyPrintToken (Token SEMICOLON _) = "SEMICOLON ; null"
prettyPrintToken (Token SLASH _) = "SLASH / null"
prettyPrintToken (Token STAR _) = "STAR * null"
prettyPrintToken (Token BANG _) = "BANG ! null"
prettyPrintToken (Token BANG_EQUAL _) = "BANG_EQUAL != null"
prettyPrintToken (Token EQUAL _) = "EQUAL = null"
prettyPrintToken (Token EQUAL_EQUAL _) = "EQUAL_EQUAL == null"
prettyPrintToken (Token GREATER _) = "GREATER > null"
prettyPrintToken (Token GREATER_EQUAL _) = "GREATER_EQUAL >= null"
prettyPrintToken (Token LESS _) = "LESS < null"
prettyPrintToken (Token LESS_EQUAL _) = "LESS_EQUAL <= null"
prettyPrintToken (Token AND _) = "AND and null"
prettyPrintToken (Token CLASS _) = "CLASS class null"
prettyPrintToken (Token ELSE _) = "ELSE else null"
prettyPrintToken (Token FALSE _) = "FALSE false null"
prettyPrintToken (Token FUN _) = "FUN fun null"
prettyPrintToken (Token FOR _) = "FOR for null"
prettyPrintToken (Token IF _) = "IF if null"
prettyPrintToken (Token NIL _) = "NIL nil null"
prettyPrintToken (Token OR _) = "OR or null"
prettyPrintToken (Token PRINT _) = "PRINT print null"
prettyPrintToken (Token RETURN _) = "RETURN return null"
prettyPrintToken (Token SUPER _) = "SUPER super null"
prettyPrintToken (Token THIS _) = "THIS this null"
prettyPrintToken (Token TRUE _) = "TRUE true null"
prettyPrintToken (Token VAR _) = "VAR var null"
prettyPrintToken (Token WHILE _) = "WHILE while null"
prettyPrintToken (Token EOF _) = "EOF  null"
