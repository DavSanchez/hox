-- |
--  This module would contain the types representing the expression AST of Lox.
--
--  This is the grammar:
-- @
-- expression     → equality ;
-- equality       → comparison ( ( "!=" | "==" ) comparison )* ;
-- comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
-- term           → factor ( ( "-" | "+" ) factor )* ;
-- factor         → unary ( ( "/" | "*" ) unary )* ;
-- unary          → ( "!" | "-" ) unary
--               | primary ;
-- primary        → NUMBER | STRING | "true" | "false" | "nil"
--               | "(" expression ")" ;
-- @
--  This naturally translates to Haskell ADTs, so that's what we will use.
module Expression
  ( -- * Data types
    Expression (..),
    Literal (..),
    UnaryOperator (..),
    BinaryOperator (..),

    -- * Parsing
    expression,

    -- * Pretty printing
    prettyPrint,
  )
where

import Control.Applicative (many, (<|>))
import Data.Char (toLower)
import Data.Functor (void)
import Parser (TokenParser, matchTokenType, satisfy)
import Token (Token (Token, tokenType), isNumber, isString)
import Token qualified as T

-- | Represents an expression in the AST.
data Expression
  = -- | A literal value.
    Literal Literal
  | -- | A unary operation.
    UnaryOperation
      -- | Line number where the operator appears.
      Int
      -- | The unary operator.
      UnaryOperator
      -- | The operand expression.
      Expression
  | -- | A binary operation.
    BinaryOperation
      -- | Line number where the operator appears.
      Int
      -- | The binary operator.
      BinaryOperator
      -- | Left operand expression.
      Expression
      -- | Right operand expression.
      Expression
  | -- | A grouped expression, e.g., (a + b)
    Grouping Expression
  deriving stock (Show, Eq)

-- | Represents a literal value in the AST.
data Literal = Number Double | String String | Bool Bool | Nil
  deriving stock (Show, Eq)

-- | Represents a unary operator in the AST.
data UnaryOperator
  = -- | Negation (`-`) operator
    UMinus
  | -- | Logical NOT (`!`) operator
    Bang
  deriving stock (Show, Eq)

-- | Represents a binary operator in the AST.
data BinaryOperator
  = -- | Equality (`==`) operator
    EqualEqual
  | -- | Inequality (`!=`) operator
    BangEqual
  | -- | Less-than (`<`) operator
    Less
  | -- | Less-than-or-equal-to (`<=`) operator
    LessEqual
  | -- | Greater-than (`>`) operator
    Greater
  | -- | Greater-than-or-equal-to (`>=`) operator
    GreaterEqual
  | -- | Addition (`+`) operator
    Plus
  | -- | Subtraction (`-`) operator
    BMinus
  | -- | Multiplication (`*`) operator
    Star
  | -- | Division (`/`) operator
    Slash
  deriving stock (Show, Eq)

-- >>> import Token (Token (..), TokenType (..))
-- >>> tokenList = [Token {tokenType = NUMBER "1" 1.0, line = 1},Token {tokenType = EQUAL_EQUAL, line = 1},Token {tokenType = NUMBER "2" 2.0, line = 1},Token {tokenType = EQUAL_EQUAL, line = 1},Token {tokenType = NUMBER "3" 3.0, line = 1},Token {tokenType = EQUAL_EQUAL, line = 1},Token {tokenType = NUMBER "4" 4.0, line = 1},Token {tokenType = EOF, line = 1}]
-- >>> runParser expression tokenList
-- Right (Binary EqualEqual (Binary EqualEqual (Binary EqualEqual (Literal (Number 1.0)) (Literal (Number 2.0))) (Literal (Number 3.0))) (Literal (Number 4.0)),[Token {tokenType = EOF, line = 1}])
-- >>> tokenList = [Token {tokenType = LEFT_PAREN, line = 1},Token {tokenType = NUMBER "1" 1.0, line = 1},Token {tokenType = PLUS, line = 1},Token {tokenType = NUMBER "2" 2.0, line = 1},Token {tokenType = RIGHT_PAREN, line = 1},Token {tokenType = PLUS, line = 1},Token {tokenType = LEFT_PAREN, line = 1},Token {tokenType = NUMBER "3" 3.0, line = 1},Token {tokenType = PLUS, line = 1},Token {tokenType = NUMBER "4" 4.0, line = 1},Token {tokenType = RIGHT_PAREN, line = 1},Token {tokenType = EOF, line = 1}]
-- >>> runParser expression tokenList
-- Right (Binary Plus (Grouping (Binary Plus (Literal (Number 1.0)) (Literal (Number 2.0)))) (Grouping (Binary Plus (Literal (Number 3.0)) (Literal (Number 4.0)))),[Token {tokenType = EOF, line = 1}])
expression :: TokenParser Expression
expression = equality <|> fail "Expect expression."

-- Equality
equality :: TokenParser Expression
equality = leftAssociative comparison (parseEq <|> parseNeq)

parseEq :: TokenParser (Expression -> Expression -> Expression)
parseEq = matchTokenType T.EQUAL_EQUAL >>= \token -> pure (BinaryOperation (T.line token) EqualEqual)

parseNeq :: TokenParser (Expression -> Expression -> Expression)
parseNeq = matchTokenType T.BANG_EQUAL >>= \token -> pure (BinaryOperation (T.line token) BangEqual)

-- Comparison
comparison :: TokenParser Expression
comparison = leftAssociative term (parseGT <|> parseGTE <|> parseLT <|> parseLTE)

parseGT :: TokenParser (Expression -> Expression -> Expression)
parseGT = matchTokenType T.GREATER >>= \token -> pure (BinaryOperation (T.line token) Greater)

parseGTE :: TokenParser (Expression -> Expression -> Expression)
parseGTE = matchTokenType T.GREATER_EQUAL >>= \token -> pure (BinaryOperation (T.line token) GreaterEqual)

parseLT :: TokenParser (Expression -> Expression -> Expression)
parseLT = matchTokenType T.LESS >>= \token -> pure (BinaryOperation (T.line token) Less)

parseLTE :: TokenParser (Expression -> Expression -> Expression)
parseLTE = matchTokenType T.LESS_EQUAL >>= \token -> pure (BinaryOperation (T.line token) LessEqual)

-- Terms
term :: TokenParser Expression
term = leftAssociative factor (parsePlus <|> parseMinus)

parsePlus :: TokenParser (Expression -> Expression -> Expression)
parsePlus = matchTokenType T.PLUS >>= \token -> pure (BinaryOperation (T.line token) Plus)

parseMinus :: TokenParser (Expression -> Expression -> Expression)
parseMinus = matchTokenType T.MINUS >>= \token -> pure (BinaryOperation (T.line token) BMinus)

-- Factors
factor :: TokenParser Expression
factor = leftAssociative unary (parseMul <|> parseDiv)

parseMul :: TokenParser (Expression -> Expression -> Expression)
parseMul = matchTokenType T.STAR >>= \token -> pure (BinaryOperation (T.line token) Star)

parseDiv :: TokenParser (Expression -> Expression -> Expression)
parseDiv = matchTokenType T.SLASH >>= \token -> pure (BinaryOperation (T.line token) Slash)

-- Unary expressions
unary :: TokenParser Expression
unary = (parseBang <|> parseMinusUnary) <*> unary <|> primary

parseBang :: TokenParser (Expression -> Expression)
parseBang = matchTokenType T.BANG >>= \token -> pure (UnaryOperation (T.line token) Bang)

parseMinusUnary :: TokenParser (Expression -> Expression)
parseMinusUnary = matchTokenType T.MINUS >>= \token -> pure (UnaryOperation (T.line token) UMinus)

-- Primary expressions
primary :: TokenParser Expression
primary =
  parseFalse
    <|> parseTrue
    <|> parseNil
    <|> parseNumber
    <|> parseString
    <|> parseGrouping

parseFalse :: TokenParser Expression
parseFalse = matchTokenType T.FALSE >> pure (Literal (Bool False))

parseTrue :: TokenParser Expression
parseTrue = matchTokenType T.TRUE >> pure (Literal (Bool True))

parseNil :: TokenParser Expression
parseNil = matchTokenType T.NIL >> pure (Literal Nil)

parseNumber :: TokenParser Expression
parseNumber = do
  Token (T.NUMBER _ n) _ <- satisfy (isNumber . tokenType) "a number"
  pure (Literal (Number n))

parseString :: TokenParser Expression
parseString = do
  Token (T.STRING _ s) _ <- satisfy (isString . tokenType) "a string"
  pure (Literal (String s))

parseGrouping :: TokenParser Expression
parseGrouping = Grouping <$> parens expression

-- Helpers

parens :: TokenParser a -> TokenParser a
parens p = do
  void $ matchTokenType T.LEFT_PAREN
  result <- p
  void $ matchTokenType T.RIGHT_PAREN
  pure result

leftAssociative ::
  -- | Parser for the first operand (left-hand side)
  TokenParser a ->
  -- | Parser for the operator in the form of a function
  TokenParser (a -> a -> a) ->
  -- | Result
  TokenParser a
leftAssociative pOperand pOperator = do
  first <- pOperand
  rest <- many $ do
    op <- pOperator
    next <- pOperand
    pure (op, next)
  pure $ foldl' (\acc (op, next) -> op acc next) first rest

-- | Prints and expression in the format expected by the Crafting Interpreters book.
-- >>> prettyPrint (Binary (BinaryOperator 1 Plus) (Literal (Number 1)) (Literal (Number 2)))
-- "(+ 1.0 2.0)"
-- >>> prettyPrint (Binary (BinaryOperator 1 Star) (Unary (UnaryOperator 1 UMinus) (Literal (Number 123))) (Literal (Number 45.67)))
-- "(* (- 123.0) 45.67)"
-- >>> prettyPrint (Binary (BinaryOperator 1 Star) (Unary (UnaryOperator 1 UMinus) (Literal (Number 123))) (Grouping (Literal (Number 45.67))))
-- "(* (- 123.0) (group 45.67))"
prettyPrint :: Expression -> String
prettyPrint (Literal lit) = prettyPrintLit lit
prettyPrint (UnaryOperation _ op expr) = "(" <> prettyPrintUnOp op <> " " <> prettyPrint expr <> ")"
prettyPrint (BinaryOperation _ op e1 e2) = "(" <> prettyPrintBinOp op <> " " <> prettyPrint e1 <> " " <> prettyPrint e2 <> ")"
prettyPrint (Grouping expr) = "(group " <> prettyPrint expr <> ")"

prettyPrintBinOp :: BinaryOperator -> String
prettyPrintBinOp EqualEqual = "=="
prettyPrintBinOp BangEqual = "!="
prettyPrintBinOp Less = "<"
prettyPrintBinOp LessEqual = "<="
prettyPrintBinOp Greater = ">"
prettyPrintBinOp GreaterEqual = ">="
prettyPrintBinOp Plus = "+"
prettyPrintBinOp BMinus = "-"
prettyPrintBinOp Star = "*"
prettyPrintBinOp Slash = "/"

prettyPrintUnOp :: UnaryOperator -> String
prettyPrintUnOp UMinus = "-"
prettyPrintUnOp Bang = "!"

prettyPrintLit :: Literal -> String
prettyPrintLit (Number n) = show n
prettyPrintLit (String s) = "String " <> s
prettyPrintLit (Bool b) = (map toLower . show) b
prettyPrintLit Nil = "nil"
