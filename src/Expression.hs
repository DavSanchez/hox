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
    LogicalOperator (..),
    UnaryOperator (..),
    BinaryOperator (..),

    -- * Parsing
    expression,

    -- * Pretty printing
    displayExpr,
  )
where

import Control.Applicative (Alternative (many, (<|>)))
import Data.Char (toLower)
import Data.Functor (void)
import Parser (TokenParser, peekToken, satisfy)
import Token (Token (Token, tokenType), isNumber, isString, toString)
import Token qualified as AST
import Token qualified as T

-- | Represents an expression in the AST.
data Expression
  = -- | A literal value.
    Literal Literal
  | -- | Logical expression
    Logical Int LogicalOperator Expression Expression
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
  | -- | A grouped expression, e.g. @(a + b)@.
    Grouping Expression
  | -- | A variable (identifier).
    VariableExpr
      -- | The line number where the variable appears.
      Int
      -- | The name of the variable.
      String
  | VariableAssignment
      -- | The line number where the assignment happens.
      Int
      -- | The name of the variable being assigned to.
      String
      -- | The expression whose value is being assigned to the variable.
      Expression
  deriving stock (Show, Eq)

-- | Represents a literal value in the AST.
data Literal = Number Double | String String | Bool Bool | Nil
  deriving stock (Show, Eq)

-- | Represents a logical operator in the AST.
data LogicalOperator
  = Or
  | And
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
-- >>> import Parser
-- >>> tokenList = [Token {tokenType = NUMBER "1" 1.0, line = 1},Token {tokenType = EQUAL_EQUAL, line = 1},Token {tokenType = NUMBER "2" 2.0, line = 1},Token {tokenType = EQUAL_EQUAL, line = 1},Token {tokenType = NUMBER "3" 3.0, line = 1},Token {tokenType = EQUAL_EQUAL, line = 1},Token {tokenType = NUMBER "4" 4.0, line = 1},Token {tokenType = EOF, line = 1}]
-- >>> runParser expression tokenList
-- Right (BinaryOperation 1 EqualEqual (BinaryOperation 1 EqualEqual (BinaryOperation 1 EqualEqual (Literal (Number 1.0)) (Literal (Number 2.0))) (Literal (Number 3.0))) (Literal (Number 4.0)),[Token {tokenType = EOF, line = 1}])
-- >>> tokenList = [Token {tokenType = LEFT_PAREN, line = 1},Token {tokenType = NUMBER "1" 1.0, line = 1},Token {tokenType = PLUS, line = 1},Token {tokenType = NUMBER "2" 2.0, line = 1},Token {tokenType = RIGHT_PAREN, line = 1},Token {tokenType = PLUS, line = 1},Token {tokenType = LEFT_PAREN, line = 1},Token {tokenType = NUMBER "3" 3.0, line = 1},Token {tokenType = PLUS, line = 1},Token {tokenType = NUMBER "4" 4.0, line = 1},Token {tokenType = RIGHT_PAREN, line = 1},Token {tokenType = EOF, line = 1}]
-- >>> runParser expression tokenList
-- Right (BinaryOperation 1 Plus (Grouping (BinaryOperation 1 Plus (Literal (Number 1.0)) (Literal (Number 2.0)))) (Grouping (BinaryOperation 1 Plus (Literal (Number 3.0)) (Literal (Number 4.0)))),[Token {tokenType = EOF, line = 1}])
expression :: TokenParser Expression
expression = assignment

assignment :: TokenParser Expression
assignment = do
  expr <- orOp <|> fail "Expect expression."
  t <- peekToken
  if tokenType t == T.EQUAL
    then varAssign expr
    else pure expr

varAssign :: Expression -> TokenParser Expression
varAssign expr = do
  case expr of
    VariableExpr lineNum name -> do
      void $ satisfy ((T.EQUAL ==) . tokenType) ("Expect " <> toString T.EQUAL <> ".")
      VariableAssignment lineNum name <$> assignment
    _ -> fail "Invalid assignment target."

-- Logic
orOp :: TokenParser Expression
orOp = leftAssociative andOp parseOr

parseOr :: TokenParser (Expression -> Expression -> Expression)
parseOr = satisfy ((T.OR ==) . tokenType) ("Expect " <> toString T.OR <> ".") >>= \t -> pure (Logical (T.line t) Or)

parseAnd :: TokenParser (Expression -> Expression -> Expression)
parseAnd = satisfy ((T.AND ==) . tokenType) ("Expect " <> toString T.AND <> ".") >>= \t -> pure (Logical (T.line t) And)

andOp :: TokenParser Expression
andOp = leftAssociative equality parseAnd

-- Equality
equality :: TokenParser Expression
equality = leftAssociative comparison (parseEq <|> parseNeq)

parseEq :: TokenParser (Expression -> Expression -> Expression)
parseEq = satisfy ((T.EQUAL_EQUAL ==) . tokenType) ("Expect " <> toString T.EQUAL_EQUAL <> ".") >>= \token -> pure (BinaryOperation (T.line token) EqualEqual)

parseNeq :: TokenParser (Expression -> Expression -> Expression)
parseNeq = satisfy ((T.BANG_EQUAL ==) . tokenType) ("Expect " <> toString T.BANG_EQUAL <> ".") >>= \token -> pure (BinaryOperation (T.line token) BangEqual)

-- Comparison
comparison :: TokenParser Expression
comparison = leftAssociative term (parseGT <|> parseGTE <|> parseLT <|> parseLTE)

parseGT :: TokenParser (Expression -> Expression -> Expression)
parseGT = satisfy ((T.GREATER ==) . tokenType) ("Expect " <> toString T.GREATER <> ".") >>= \token -> pure (BinaryOperation (T.line token) Greater)

parseGTE :: TokenParser (Expression -> Expression -> Expression)
parseGTE = satisfy ((T.GREATER_EQUAL ==) . tokenType) ("Expect " <> toString T.GREATER_EQUAL <> ".") >>= \token -> pure (BinaryOperation (T.line token) GreaterEqual)

parseLT :: TokenParser (Expression -> Expression -> Expression)
parseLT = satisfy ((T.LESS ==) . tokenType) ("Expect " <> toString T.LESS <> ".") >>= \token -> pure (BinaryOperation (T.line token) Less)

parseLTE :: TokenParser (Expression -> Expression -> Expression)
parseLTE = satisfy ((T.LESS_EQUAL ==) . tokenType) ("Expect " <> toString T.LESS_EQUAL <> ".") >>= \token -> pure (BinaryOperation (T.line token) LessEqual)

-- Terms
term :: TokenParser Expression
term = leftAssociative factor (parsePlus <|> parseMinus)

parsePlus :: TokenParser (Expression -> Expression -> Expression)
parsePlus = satisfy ((T.PLUS ==) . tokenType) ("Expect " <> toString T.PLUS <> ".") >>= \token -> pure (BinaryOperation (T.line token) Plus)

parseMinus :: TokenParser (Expression -> Expression -> Expression)
parseMinus = satisfy ((T.MINUS ==) . tokenType) ("Expect " <> toString T.MINUS <> ".") >>= \token -> pure (BinaryOperation (T.line token) BMinus)

-- Factors
factor :: TokenParser Expression
factor = leftAssociative unary (parseMul <|> parseDiv)

parseMul :: TokenParser (Expression -> Expression -> Expression)
parseMul = satisfy ((T.STAR ==) . tokenType) ("Expect " <> toString T.STAR <> ".") >>= \token -> pure (BinaryOperation (T.line token) Star)

parseDiv :: TokenParser (Expression -> Expression -> Expression)
parseDiv = satisfy ((T.SLASH ==) . tokenType) ("Expect " <> toString T.SLASH <> ".") >>= \token -> pure (BinaryOperation (T.line token) Slash)

-- Unary expressions
unary :: TokenParser Expression
unary = (parseBang <|> parseMinusUnary) <*> unary <|> primary

parseBang :: TokenParser (Expression -> Expression)
parseBang = satisfy ((T.BANG ==) . tokenType) ("Expect " <> toString T.BANG <> ".") >>= \token -> pure (UnaryOperation (T.line token) Bang)

parseMinusUnary :: TokenParser (Expression -> Expression)
parseMinusUnary = satisfy ((T.MINUS ==) . tokenType) ("Expect " <> toString T.MINUS <> ".") >>= \token -> pure (UnaryOperation (T.line token) UMinus)

-- Primary expressions
primary :: TokenParser Expression
primary =
  parseFalse
    <|> parseTrue
    <|> parseNil
    <|> parseNumber
    <|> parseString
    <|> parseGrouping
    <|> parseVarName

parseFalse :: TokenParser Expression
parseFalse = satisfy ((T.FALSE ==) . tokenType) ("Expect " <> toString T.FALSE <> ".") >> pure (Literal (Bool False))

parseTrue :: TokenParser Expression
parseTrue = satisfy ((T.TRUE ==) . tokenType) ("Expect " <> toString T.TRUE <> ".") >> pure (Literal (Bool True))

parseNil :: TokenParser Expression
parseNil = satisfy ((T.NIL ==) . tokenType) ("Expect " <> toString T.NIL <> ".") >> pure (Literal Nil)

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

parseVarName :: TokenParser Expression
parseVarName = do
  Token {tokenType = T.IDENTIFIER name, line = lineNum} <- satisfy (T.isIdentifier . tokenType) "variable"
  pure (VariableExpr lineNum name)

-- Helpers

parens :: TokenParser a -> TokenParser a
parens p = do
  void $ satisfy ((T.LEFT_PAREN ==) . tokenType) ("Expect " <> toString T.LEFT_PAREN <> ".")
  result <- p
  void $ satisfy ((T.RIGHT_PAREN ==) . tokenType) ("Expect " <> toString T.RIGHT_PAREN <> ".")
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
-- >>> displayExpr(BinaryOperation 1 Plus (Literal (Number 1)) (Literal (Number 2)))
-- "(+ 1.0 2.0)"
-- >>> displayExpr(BinaryOperation 1 Star (UnaryOperation 1 UMinus (Literal (Number 123))) (Literal (Number 45.67)))
-- "(* (- 123.0) 45.67)"
-- >>> displayExpr(BinaryOperation 1 Star (UnaryOperation 1 UMinus (Literal (Number 123))) (Grouping (Literal (Number 45.67))))
-- "(* (- 123.0) (group 45.67))"
displayExpr :: Expression -> String
displayExpr (Literal lit) = displayLit lit
displayExpr (UnaryOperation _ op expr) = "(" <> displayUnOp op <> " " <> displayExpr expr <> ")"
displayExpr (BinaryOperation _ op e1 e2) = "(" <> displayBinOp op <> " " <> displayExpr e1 <> " " <> displayExpr e2 <> ")"
displayExpr (Grouping expr) = "(group " <> displayExpr expr <> ")"
displayExpr (VariableExpr _ name) = name
displayExpr (VariableAssignment _ name expr) = "(= " <> name <> " " <> displayExpr expr <> ")"
displayExpr (Logical _ op e1 e2) = "(" <> displayLogicOp op <> " " <> displayExpr e1 <> displayExpr e2 <> ")"

displayBinOp :: BinaryOperator -> String
displayBinOp EqualEqual = "=="
displayBinOp BangEqual = "!="
displayBinOp Less = "<"
displayBinOp LessEqual = "<="
displayBinOp Greater = ">"
displayBinOp GreaterEqual = ">="
displayBinOp Plus = "+"
displayBinOp BMinus = "-"
displayBinOp Star = "*"
displayBinOp Slash = "/"

displayUnOp :: UnaryOperator -> String
displayUnOp UMinus = "-"
displayUnOp Bang = "!"

displayLogicOp :: LogicalOperator -> String
displayLogicOp Or = "or"
displayLogicOp And = "and"

displayLit :: Literal -> String
displayLit (Number n) = show n
displayLit (String s) = "String " <> s
displayLit (Bool b) = (map toLower . show) b
displayLit Nil = "nil"
