module Token (Token (..), TokenType (..), prettyPrint) where

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
  deriving stock (Show)

type Lexeme = String

data Token = Token
  { tokenType :: TokenType,
    line :: Int
  }
  deriving stock (Show)

-- |  Pretty print a token the way the book expects
-- >>> prettyPrint (Token (IDENTIFIER "foo") 1)
-- "IDENTIFIER foo null"
-- >>> prettyPrint (Token (NUMBER "42" 42.0) 1)
-- "NUMBER 42 42.0"
prettyPrint :: Token -> String
prettyPrint (Token (IDENTIFIER s) _) = "IDENTIFIER " <> s <> " null"
prettyPrint (Token (STRING l s) _) = "STRING " <> l <> " " <> s
prettyPrint (Token (NUMBER l n) _) = "NUMBER " <> l <> " " <> show n
prettyPrint (Token LEFT_PAREN _) = "LEFT_PAREN ( null"
prettyPrint (Token RIGHT_PAREN _) = "RIGHT_PAREN ) null"
prettyPrint (Token LEFT_BRACE _) = "LEFT_BRACE { null"
prettyPrint (Token RIGHT_BRACE _) = "RIGHT_BRACE } null"
prettyPrint (Token COMMA _) = "COMMA , null"
prettyPrint (Token DOT _) = "DOT . null"
prettyPrint (Token MINUS _) = "MINUS - null"
prettyPrint (Token PLUS _) = "PLUS + null"
prettyPrint (Token SEMICOLON _) = "SEMICOLON ; null"
prettyPrint (Token SLASH _) = "SLASH / null"
prettyPrint (Token STAR _) = "STAR * null"
prettyPrint (Token BANG _) = "BANG ! null"
prettyPrint (Token BANG_EQUAL _) = "BANG_EQUAL != null"
prettyPrint (Token EQUAL _) = "EQUAL = null"
prettyPrint (Token EQUAL_EQUAL _) = "EQUAL_EQUAL == null"
prettyPrint (Token GREATER _) = "GREATER > null"
prettyPrint (Token GREATER_EQUAL _) = "GREATER_EQUAL >= null"
prettyPrint (Token LESS _) = "LESS < null"
prettyPrint (Token LESS_EQUAL _) = "LESS_EQUAL <= null"
prettyPrint (Token AND _) = "AND and null"
prettyPrint (Token CLASS _) = "CLASS class null"
prettyPrint (Token ELSE _) = "ELSE else null"
prettyPrint (Token FALSE _) = "FALSE false null"
prettyPrint (Token FUN _) = "FUN fun null"
prettyPrint (Token FOR _) = "FOR for null"
prettyPrint (Token IF _) = "IF if null"
prettyPrint (Token NIL _) = "NIL nil null"
prettyPrint (Token OR _) = "OR or null"
prettyPrint (Token PRINT _) = "PRINT print null"
prettyPrint (Token RETURN _) = "RETURN return null"
prettyPrint (Token SUPER _) = "SUPER super null"
prettyPrint (Token THIS _) = "THIS this null"
prettyPrint (Token TRUE _) = "TRUE true null"
prettyPrint (Token VAR _) = "VAR var null"
prettyPrint (Token WHILE _) = "WHILE while null"
prettyPrint (Token EOF _) = "EOF  null"
