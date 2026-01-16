module Language.Syntax.Token
  ( Token (..),
    TokenType (..),
    displayToken,
    isNumber,
    isString,
    isIdentifier,
    displayTokenType,
  )
where

import Data.Text (Text, unpack)

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
  | STRING Lexeme Text
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

isIdentifier :: TokenType -> Bool
isIdentifier (IDENTIFIER _) = True
isIdentifier _ = False

type Lexeme = Text

data Token = Token
  { tokenType :: TokenType,
    line :: Int
  }
  deriving stock (Show, Eq)

-- |  Pretty print a token the way the book expects
-- >>> import Data.Text
-- >>> displayToken (Token (IDENTIFIER (pack "foo")) 1)
-- "IDENTIFIER foo null"
-- >>> displayToken (Token (NUMBER (pack "42") 42.0) 1)
-- "NUMBER 42 42.0"
displayToken :: Token -> String
displayToken (Token (IDENTIFIER s) _) = "IDENTIFIER " <> unpack s <> " null"
displayToken (Token (STRING l s) _) = "STRING " <> unpack l <> " " <> unpack s
displayToken (Token (NUMBER l n) _) = "NUMBER " <> unpack l <> " " <> show n
displayToken (Token LEFT_PAREN _) = "LEFT_PAREN ( null"
displayToken (Token RIGHT_PAREN _) = "RIGHT_PAREN ) null"
displayToken (Token LEFT_BRACE _) = "LEFT_BRACE { null"
displayToken (Token RIGHT_BRACE _) = "RIGHT_BRACE } null"
displayToken (Token COMMA _) = "COMMA , null"
displayToken (Token DOT _) = "DOT . null"
displayToken (Token MINUS _) = "MINUS - null"
displayToken (Token PLUS _) = "PLUS + null"
displayToken (Token SEMICOLON _) = "SEMICOLON ; null"
displayToken (Token SLASH _) = "SLASH / null"
displayToken (Token STAR _) = "STAR * null"
displayToken (Token BANG _) = "BANG ! null"
displayToken (Token BANG_EQUAL _) = "BANG_EQUAL != null"
displayToken (Token EQUAL _) = "EQUAL = null"
displayToken (Token EQUAL_EQUAL _) = "EQUAL_EQUAL == null"
displayToken (Token GREATER _) = "GREATER > null"
displayToken (Token GREATER_EQUAL _) = "GREATER_EQUAL >= null"
displayToken (Token LESS _) = "LESS < null"
displayToken (Token LESS_EQUAL _) = "LESS_EQUAL <= null"
displayToken (Token AND _) = "AND and null"
displayToken (Token CLASS _) = "CLASS class null"
displayToken (Token ELSE _) = "ELSE else null"
displayToken (Token FALSE _) = "FALSE false null"
displayToken (Token FUN _) = "FUN fun null"
displayToken (Token FOR _) = "FOR for null"
displayToken (Token IF _) = "IF if null"
displayToken (Token NIL _) = "NIL nil null"
displayToken (Token OR _) = "OR or null"
displayToken (Token PRINT _) = "PRINT print null"
displayToken (Token RETURN _) = "RETURN return null"
displayToken (Token SUPER _) = "SUPER super null"
displayToken (Token THIS _) = "THIS this null"
displayToken (Token TRUE _) = "TRUE true null"
displayToken (Token VAR _) = "VAR var null"
displayToken (Token WHILE _) = "WHILE while null"
displayToken (Token EOF _) = "EOF  null"

displayTokenType :: TokenType -> String
displayTokenType (IDENTIFIER s) = unpack s
displayTokenType (STRING s _) = unpack s
displayTokenType (NUMBER s _) = unpack s
displayTokenType LEFT_PAREN = "("
displayTokenType RIGHT_PAREN = ")"
displayTokenType LEFT_BRACE = "{"
displayTokenType RIGHT_BRACE = "}"
displayTokenType COMMA = ","
displayTokenType DOT = "."
displayTokenType MINUS = "-"
displayTokenType PLUS = "+"
displayTokenType SEMICOLON = ";"
displayTokenType SLASH = "/"
displayTokenType STAR = "*"
displayTokenType BANG = "!"
displayTokenType BANG_EQUAL = "!="
displayTokenType EQUAL = "="
displayTokenType EQUAL_EQUAL = "=="
displayTokenType GREATER = ">"
displayTokenType GREATER_EQUAL = ">="
displayTokenType LESS = "<"
displayTokenType LESS_EQUAL = "<="
displayTokenType AND = "and"
displayTokenType CLASS = "class"
displayTokenType ELSE = "else"
displayTokenType FALSE = "false"
displayTokenType TRUE = "true"
displayTokenType FUN = "fun"
displayTokenType FOR = "for"
displayTokenType IF = "if"
displayTokenType NIL = "nil"
displayTokenType OR = "or"
displayTokenType PRINT = "print"
displayTokenType RETURN = "return"
displayTokenType SUPER = "super"
displayTokenType THIS = "this"
displayTokenType VAR = "var"
displayTokenType WHILE = "while"
displayTokenType EOF = "EOF"
