{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted function" #-}
module Scanner.ParserCombinators where

import Data.Void (Void)
import Scanner.Internal (SyntaxError (..), TokenResult, identifierOrKeyword, isAlpha, isAlphaNum)
import Text.Megaparsec (MonadParsec (eof), ParseErrorBundle (bundlePosState), Parsec, PosState (pstateSourcePos), SourcePos (sourceLine), choice, getSourcePos, many, manyTill, runParser, satisfy, try, unPos, (<|>))
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Token (Token (..), TokenType (..))

type Parser = Parsec Void String

-- Handles whitespace, line comments, and even block comments!
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/") -- could replace with `fail` or something

-- >>> import Text.Megaparsec (parseMaybe)
-- >>> parseMaybe (symbol "hello world") "hello world"
-- Just "hello world"
symbol :: String -> Parser String
symbol = L.symbol sc

-- >>> import Text.Megaparsec (parseMaybe)
-- >>> parseMaybe (lexeme (char 'a')) "a"
-- Just 'a'
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- >>> import Text.Megaparsec (parseMaybe)
-- >>> parseMaybe (withLineN (symbol "(") (const LEFT_PAREN)) "("
-- Just (Token {tokenType = LEFT_PAREN, line = 1})
withLineN :: Parser a -> (a -> TokenType) -> Parser Token
withLineN p tokenType = do
  lineN <- unPos . sourceLine <$> getSourcePos
  value <- tokenType <$> p
  pure $ Token value lineN

rightParen :: Parser Token
rightParen = withLineN (symbol ")") (const RIGHT_PAREN)

leftParen :: Parser Token
leftParen = withLineN (symbol "(") (const LEFT_PAREN)

leftBrace :: Parser Token
leftBrace = withLineN (symbol "{") (const LEFT_BRACE)

rightBrace :: Parser Token
rightBrace = withLineN (symbol "}") (const RIGHT_BRACE)

comma :: Parser Token
comma = withLineN (symbol ",") (const COMMA)

dot :: Parser Token
dot = withLineN (symbol ".") (const DOT)

minus :: Parser Token
minus = withLineN (symbol "-") (const MINUS)

plus :: Parser Token
plus = withLineN (symbol "+") (const PLUS)

semicolon :: Parser Token
semicolon = withLineN (symbol ";") (const SEMICOLON)

star :: Parser Token
star = withLineN (symbol "*") (const STAR)

slash :: Parser Token
slash = withLineN (symbol "/") (const SLASH)

bang :: Parser Token
bang = withLineN (symbol "!") (const BANG)

bangEqual :: Parser Token
bangEqual = withLineN (symbol "!=") (const BANG_EQUAL)

equal :: Parser Token
equal = withLineN (symbol "=") (const EQUAL)

equalEqual :: Parser Token
equalEqual = withLineN (symbol "==") (const EQUAL_EQUAL)

greater :: Parser Token
greater = withLineN (symbol ">") (const GREATER)

greaterEqual :: Parser Token
greaterEqual = withLineN (symbol ">=") (const GREATER_EQUAL)

less :: Parser Token
less = withLineN (symbol "<") (const LESS)

lessEqual :: Parser Token
lessEqual = withLineN (symbol "<=") (const LESS_EQUAL)

stringLiteral :: Parser Token
stringLiteral = withLineN (lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))) (\s -> STRING (show s) s)

float :: Parser Token
float = withLineN (lexeme L.float) (\n -> NUMBER (show n) n)

decimal :: Parser Token
decimal = withLineN (lexeme @Int L.decimal) (\n -> NUMBER (show n) (fromIntegral n))

identifier :: Parser Token
identifier =
  withLineN
    ((:) <$> satisfy isAlpha <*> lexeme (many $ satisfy isAlphaNum))
    identifierOrKeyword

atEof :: Parser Token
atEof = withLineN eof (const EOF)

-- >>> import Text.Megaparsec (parseMaybe)
-- >>> parseMaybe megaparsecScanTokens' "andy formless fo _ _123 _abc ab123"
-- Just [Token {tokenType = IDENTIFIER "andy", line = 1},Token {tokenType = IDENTIFIER "formless", line = 1},Token {tokenType = IDENTIFIER "fo", line = 1},Token {tokenType = IDENTIFIER "_", line = 1},Token {tokenType = IDENTIFIER "_123", line = 1},Token {tokenType = IDENTIFIER "_abc", line = 1},Token {tokenType = IDENTIFIER "ab123", line = 1},Token {tokenType = EOF, line = 1}]
megaparsecScanTokens' :: Parser [Token]
megaparsecScanTokens' =
  (++)
    <$> many
      ( choice
          [ rightParen,
            leftParen,
            leftBrace,
            rightBrace,
            comma,
            dot,
            minus,
            plus,
            semicolon,
            star,
            slash,
            bangEqual,
            bang,
            equalEqual,
            equal,
            greaterEqual,
            greater,
            lessEqual,
            less,
            stringLiteral,
            try float <|> decimal,
            identifier
          ]
      )
    <*> (pure <$> atEof)

megaparsecScanTokens :: String -> [TokenResult]
megaparsecScanTokens input =
  let result = runParser megaparsecScanTokens' "" input
   in case result of
        Left err -> [Left (SyntaxError (show err) (unPos $ sourceLine $ pstateSourcePos $ bundlePosState err) "")]
        Right tokens -> map Right tokens
