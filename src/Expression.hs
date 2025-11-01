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
import Parser (TokenParser, peek, satisfy)
import Token (Token (..), TokenType (..), displayTokenType, isIdentifier, isNumber, isString)

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
  | -- | A function call
    Call
      -- | The line number where the call happens.
      Int
      -- | The callee expression.
      Expression
      -- | The list of argument expressions.
      [Expression]
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
  t <- peek
  if tokenType t == EQUAL
    then varAssign expr
    else pure expr

varAssign :: Expression -> TokenParser Expression
varAssign expr = do
  case expr of
    VariableExpr lineNum name -> do
      void $ satisfy ((EQUAL ==) . tokenType) ("Expect " <> displayTokenType EQUAL <> ".")
      VariableAssignment lineNum name <$> assignment
    _ -> fail "Invalid assignment target."

-- Logic
orOp :: TokenParser Expression
orOp = leftAssociative andOp parseOr

parseOr :: TokenParser (Expression -> Expression -> Expression)
parseOr = satisfy ((OR ==) . tokenType) ("Expect " <> displayTokenType OR <> ".") >>= \t -> pure (Logical (line t) Or)

parseAnd :: TokenParser (Expression -> Expression -> Expression)
parseAnd = satisfy ((AND ==) . tokenType) ("Expect " <> displayTokenType AND <> ".") >>= \t -> pure (Logical (line t) And)

andOp :: TokenParser Expression
andOp = leftAssociative equality parseAnd

-- Equality
equality :: TokenParser Expression
equality = leftAssociative comparison (parseEq <|> parseNeq)

parseEq :: TokenParser (Expression -> Expression -> Expression)
parseEq = satisfy ((EQUAL_EQUAL ==) . tokenType) ("Expect " <> displayTokenType EQUAL_EQUAL <> ".") >>= \token -> pure (BinaryOperation (line token) EqualEqual)

parseNeq :: TokenParser (Expression -> Expression -> Expression)
parseNeq = satisfy ((BANG_EQUAL ==) . tokenType) ("Expect " <> displayTokenType BANG_EQUAL <> ".") >>= \token -> pure (BinaryOperation (line token) BangEqual)

-- Comparison
comparison :: TokenParser Expression
comparison = leftAssociative term (parseGT <|> parseGTE <|> parseLT <|> parseLTE)

parseGT :: TokenParser (Expression -> Expression -> Expression)
parseGT = satisfy ((GREATER ==) . tokenType) ("Expect " <> displayTokenType GREATER <> ".") >>= \token -> pure (BinaryOperation (line token) Greater)

parseGTE :: TokenParser (Expression -> Expression -> Expression)
parseGTE = satisfy ((GREATER_EQUAL ==) . tokenType) ("Expect " <> displayTokenType GREATER_EQUAL <> ".") >>= \token -> pure (BinaryOperation (line token) GreaterEqual)

parseLT :: TokenParser (Expression -> Expression -> Expression)
parseLT = satisfy ((LESS ==) . tokenType) ("Expect " <> displayTokenType LESS <> ".") >>= \token -> pure (BinaryOperation (line token) Less)

parseLTE :: TokenParser (Expression -> Expression -> Expression)
parseLTE = satisfy ((LESS_EQUAL ==) . tokenType) ("Expect " <> displayTokenType LESS_EQUAL <> ".") >>= \token -> pure (BinaryOperation (line token) LessEqual)

-- Terms
term :: TokenParser Expression
term = leftAssociative factor (parsePlus <|> parseMinus)

parsePlus :: TokenParser (Expression -> Expression -> Expression)
parsePlus = satisfy ((PLUS ==) . tokenType) ("Expect " <> displayTokenType PLUS <> ".") >>= \token -> pure (BinaryOperation (line token) Plus)

parseMinus :: TokenParser (Expression -> Expression -> Expression)
parseMinus = satisfy ((MINUS ==) . tokenType) ("Expect " <> displayTokenType MINUS <> ".") >>= \token -> pure (BinaryOperation (line token) BMinus)

-- Factors
factor :: TokenParser Expression
factor = leftAssociative unary (parseMul <|> parseDiv)

parseMul :: TokenParser (Expression -> Expression -> Expression)
parseMul = satisfy ((STAR ==) . tokenType) ("Expect " <> displayTokenType STAR <> ".") >>= \token -> pure (BinaryOperation (line token) Star)

parseDiv :: TokenParser (Expression -> Expression -> Expression)
parseDiv = satisfy ((SLASH ==) . tokenType) ("Expect " <> displayTokenType SLASH <> ".") >>= \token -> pure (BinaryOperation (line token) Slash)

-- Unary expressions
unary :: TokenParser Expression
unary = (parseBang <|> parseMinusUnary) <*> unary <|> call

call :: TokenParser Expression
call = do
  expr <- primary
  t <- peek
  if tokenType t == LEFT_PAREN
    then finishCall expr
    else pure expr

finishCall :: Expression -> TokenParser Expression
finishCall expr = do
  t <- peek
  if tokenType t == LEFT_PAREN
    then functionArgs >>= finishCall . Call (line t) expr
    else pure expr

functionArgs :: TokenParser [Expression]
functionArgs =
  satisfy ((LEFT_PAREN ==) . tokenType) ("Expect " <> displayTokenType LEFT_PAREN <> ".")
    *> argumentList
    <* satisfy ((RIGHT_PAREN ==) . tokenType) ("Expect " <> displayTokenType RIGHT_PAREN <> ".")

argumentList :: TokenParser [Expression]
argumentList = do
  t <- peek
  if tokenType t == RIGHT_PAREN
    then pure []
    else do
      firstArg <- expression
      restArgs <- many (satisfy ((COMMA ==) . tokenType) ("Expect " <> displayTokenType COMMA <> ".") *> expression)
      let args = firstArg : restArgs
      if length args >= 255
        then fail "Cannot have more than 255 arguments."
        else pure args

parseBang :: TokenParser (Expression -> Expression)
parseBang = satisfy ((BANG ==) . tokenType) ("Expect " <> displayTokenType BANG <> ".") >>= \token -> pure (UnaryOperation (line token) Bang)

parseMinusUnary :: TokenParser (Expression -> Expression)
parseMinusUnary = satisfy ((MINUS ==) . tokenType) ("Expect " <> displayTokenType MINUS <> ".") >>= \token -> pure (UnaryOperation (line token) UMinus)

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
parseFalse = satisfy ((FALSE ==) . tokenType) ("Expect " <> displayTokenType FALSE <> ".") >> pure (Literal (Bool False))

parseTrue :: TokenParser Expression
parseTrue = satisfy ((TRUE ==) . tokenType) ("Expect " <> displayTokenType TRUE <> ".") >> pure (Literal (Bool True))

parseNil :: TokenParser Expression
parseNil = satisfy ((NIL ==) . tokenType) ("Expect " <> displayTokenType NIL <> ".") >> pure (Literal Nil)

parseNumber :: TokenParser Expression
parseNumber = do
  Token (NUMBER _ n) _ <- satisfy (isNumber . tokenType) "a number"
  pure (Literal (Number n))

parseString :: TokenParser Expression
parseString = do
  Token (STRING _ s) _ <- satisfy (isString . tokenType) "a string"
  pure (Literal (String s))

parseGrouping :: TokenParser Expression
parseGrouping = Grouping <$> parens expression

parseVarName :: TokenParser Expression
parseVarName = do
  Token {tokenType = IDENTIFIER name, line = lineNum} <- satisfy (isIdentifier . tokenType) "variable"
  pure (VariableExpr lineNum name)

-- Helpers

parens :: TokenParser a -> TokenParser a
parens p = do
  void $ satisfy ((LEFT_PAREN ==) . tokenType) ("Expect " <> displayTokenType LEFT_PAREN <> ".")
  result <- p
  void $ satisfy ((RIGHT_PAREN ==) . tokenType) ("Expect " <> displayTokenType RIGHT_PAREN <> ".")
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
displayExpr (Call _ callee args) = "(call " <> displayExpr callee <> " " <> unwords (map displayExpr args) <> ")"
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
