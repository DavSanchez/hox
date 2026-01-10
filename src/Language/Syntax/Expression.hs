{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
module Language.Syntax.Expression
  ( -- * Data types
    Expression (..),
    Literal (..),
    LogicalOperator (..),
    UnaryOperator (..),
    BinaryOperator (..),
    Resolution (..),
    LocalResolution (..),
    NotResolved (..),
    Phase (..),

    -- * Parsing
    expression,

    -- * Pretty printing
    displayExpr,
  )
where

import Control.Applicative (Alternative (many, (<|>)))
import Data.Char (toLower)
import Data.Functor (void)
import Data.Kind (Type)
import Language.Parser (TokenParser, peek, satisfy)
import Language.Syntax.Token (Token (..), TokenType (..), displayTokenType, isIdentifier, isNumber, isString)
import Numeric.Natural (Natural)

-- $setup
-- >>> import Language.Parser (runParser)
-- >>> import Language.Scanner (scanTokens)
-- >>> import Language.Syntax.Token (Token (..), TokenType (..))
-- >>> import Data.List.NonEmpty qualified as NE
-- >>> let tokensOf src = [t | Right t <- NE.toList (scanTokens src)]

-- | Represents the phase of the AST: parsed but unresolved, or resolved.
--
-- We use a type-level tag to distinguish between the two phases.
data Phase = Unresolved | Resolved
  deriving stock (Show, Eq)

type family ResolutionInfo (p :: Phase) :: Type where
  ResolutionInfo 'Unresolved = NotResolved
  ResolutionInfo 'Resolved = Resolution

type family LocalPhase (p :: Phase) :: Type where
  LocalPhase 'Unresolved = NotResolved
  LocalPhase 'Resolved = LocalResolution

-- | Represents an expression in the AST.
data Expression (p :: Phase)
  = -- | A literal value.
    Literal Literal
  | -- | Logical expression
    Logical Int LogicalOperator (Expression p) (Expression p)
  | -- | A unary operation.
    UnaryOperation
      -- | Line number where the operator appears.
      Int
      -- | The unary operator.
      UnaryOperator
      -- | The operand expression.
      (Expression p)
  | -- | A binary operation.
    BinaryOperation
      -- | Line number where the operator appears.
      Int
      -- | The binary operator.
      BinaryOperator
      -- | Left operand expression.
      (Expression p)
      -- | Right operand expression.
      (Expression p)
  | -- | A function call
    Call
      -- | The line number where the call happens.
      Int
      -- | The callee expression.
      (Expression p)
      -- | The list of argument expressions.
      [Expression p]
  | -- | A property access expression (e.g., object.property).
    Get
      -- | The line number where the property access happens.
      Int
      -- | The object expression.
      (Expression p)
      -- | The name of the property being accessed.
      String
  | -- | A property assignment expression (e.g., object.property = value).
    Set
      -- | The line number where the property assignment happens.
      Int
      -- | The object expression.
      (Expression p)
      -- | The name of the property being assigned to.
      String
      -- | The expression whose value is being assigned to the property.
      (Expression p)
  | -- | A 'this' keyword expression.
    This
      -- | The line number where 'this' appears.
      Int
      -- | The resolution distance (depth).
      (LocalPhase p)
  | -- | A `super` keyword expression.
    Super
      -- | The line number where `super` happens.
      Int
      -- | The name of the method being accessed.
      String
      -- | The resolution distance (depth) for the superclass.
      (LocalPhase p)
      -- | The resolution distance (depth) for the object (this).
      (LocalPhase p)
  | -- | A grouped expression, e.g. @(a + b)@.
    Grouping (Expression p)
  | -- | A variable (identifier).
    VariableExpr
      -- | The line number where the variable appears.
      Int
      -- | The name of the variable.
      String
      -- | The resolution distance (depth).
      (ResolutionInfo p)
  | VariableAssignment
      -- | The line number where the assignment happens.
      Int
      -- | The name of the variable being assigned to.
      String
      -- | The expression whose value is being assigned to the variable.
      (Expression p)
      -- | The resolution distance (depth).
      (ResolutionInfo p)

deriving stock instance (Show (ResolutionInfo p), Show (LocalPhase p)) => Show (Expression p)

deriving stock instance (Eq (ResolutionInfo p), Eq (LocalPhase p)) => Eq (Expression p)

-- | Represents the resolution status of a variable.
data Resolution
  = -- | The variable is global (not resolved to a specific depth).
    Global
  | -- | The variable is local, found at a specific depth (number of scopes up).
    Local LocalResolution
  deriving stock (Show, Eq)

-- | Represents a local resolution (always resolved to a specific depth).
newtype LocalResolution = LocalResolution Natural
  deriving stock (Show, Eq)
  deriving newtype (Enum)

-- | Represents an unresolved variable.
data NotResolved = NotResolved
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

-- (Token constructors imported once in $setup for doctests.)

-- >>> tokenList = [Token {tokenType = NUMBER "1" 1.0, line = 1},Token {tokenType = EQUAL_EQUAL, line = 1},Token {tokenType = NUMBER "2" 2.0, line = 1},Token {tokenType = EQUAL_EQUAL, line = 1},Token {tokenType = NUMBER "3" 3.0, line = 1},Token {tokenType = EQUAL_EQUAL, line = 1},Token {tokenType = NUMBER "4" 4.0, line = 1},Token {tokenType = EOF, line = 1}]
-- >>> runParser expression tokenList
-- Right (BinaryOperation 1 EqualEqual (BinaryOperation 1 EqualEqual (BinaryOperation 1 EqualEqual (Literal (Number 1.0)) (Literal (Number 2.0))) (Literal (Number 3.0))) (Literal (Number 4.0)),[Token {tokenType = EOF, line = 1}])
-- >>> tokenList = [Token {tokenType = LEFT_PAREN, line = 1},Token {tokenType = NUMBER "1" 1.0, line = 1},Token {tokenType = PLUS, line = 1},Token {tokenType = NUMBER "2" 2.0, line = 1},Token {tokenType = RIGHT_PAREN, line = 1},Token {tokenType = PLUS, line = 1},Token {tokenType = LEFT_PAREN, line = 1},Token {tokenType = NUMBER "3" 3.0, line = 1},Token {tokenType = PLUS, line = 1},Token {tokenType = NUMBER "4" 4.0, line = 1},Token {tokenType = RIGHT_PAREN, line = 1},Token {tokenType = EOF, line = 1}]
-- >>> runParser expression tokenList
-- Right (BinaryOperation 1 Plus (Grouping (BinaryOperation 1 Plus (Literal (Number 1.0)) (Literal (Number 2.0)))) (Grouping (BinaryOperation 1 Plus (Literal (Number 3.0)) (Literal (Number 4.0)))),[Token {tokenType = EOF, line = 1}])
--
-- Additional doctests using the scanner directly for readability:
--
-- >>> let Right toks = scanTokens "1 + 2 == 3 == 4"
-- >>> runParser expression toks
-- (Right (BinaryOperation 1 EqualEqual (BinaryOperation 1 EqualEqual (BinaryOperation 1 Plus (Literal (Number 1.0)) (Literal (Number 2.0))) (Literal (Number 3.0))) (Literal (Number 4.0))),[])
--
-- >>> let Right toks = scanTokens "(1 + 2) + (3 + 4)"
-- >>> runParser expression toks
-- (Right (BinaryOperation 1 Plus (Grouping (BinaryOperation 1 Plus (Literal (Number 1.0)) (Literal (Number 2.0)))) (Grouping (BinaryOperation 1 Plus (Literal (Number 3.0)) (Literal (Number 4.0))))),[])
--
-- >>> let Right toks = scanTokens "foo(1, 2, 3)"
-- >>> runParser expression toks
-- (Right (Call 1 (VariableExpr 1 "foo") [Literal (Number 1.0),Literal (Number 2.0),Literal (Number 3.0)]),[])
--
-- >>> let Right toks = scanTokens "a = b = c"
-- >>> runParser expression toks
-- (Right (VariableAssignment 1 "a" (VariableAssignment 1 "b" (VariableExpr 1 "c"))),[])

-- | Top-level expression parser.
-- >>> runParser expression (tokensOf "1 + 2 * 3")
-- (Right (BinaryOperation 1 Plus (Literal (Number 1.0)) (BinaryOperation 1 Star (Literal (Number 2.0)) (Literal (Number 3.0)))),[Token {tokenType = EOF, line = 1}])
-- >>> runParser expression (tokensOf "a = b = 1")
-- (Right (VariableAssignment 1 "a" (VariableAssignment 1 "b" (Literal (Number 1.0)) NotResolved) NotResolved),[Token {tokenType = EOF, line = 1}])
-- >>> runParser expression (tokensOf "foo(1)(2)(3)")
-- (Right (Call 1 (Call 1 (Call 1 (VariableExpr 1 "foo" NotResolved) [Literal (Number 1.0)]) [Literal (Number 2.0)]) [Literal (Number 3.0)]),[Token {tokenType = EOF, line = 1}])
expression :: TokenParser (Expression 'Unresolved)
expression = assignment

-- | Assignment parser (handles right-associative '=' after an l-value).
-- >>> runParser assignment (tokensOf "a = b = c")
-- (Right (VariableAssignment 1 "a" (VariableAssignment 1 "b" (VariableExpr 1 "c" NotResolved) NotResolved) NotResolved),[Token {tokenType = EOF, line = 1}])
assignment :: TokenParser (Expression 'Unresolved)
assignment = do
  expr <- orOp
  t <- peek
  if tokenType t == EQUAL
    then case expr of
      VariableExpr lineNum name _ -> do
        void $ satisfy ((EQUAL ==) . tokenType) ("Expect " <> displayTokenType EQUAL <> ".")
        VariableAssignment lineNum name <$> assignment <*> pure NotResolved
      Get lineNum object propName -> do
        void $ satisfy ((EQUAL ==) . tokenType) ("Expect " <> displayTokenType EQUAL <> ".")
        Set lineNum object propName <$> assignment
      _ -> fail "Invalid assignment target."
    else pure expr

-- Logic

-- | Logical OR parser.
-- >>> runParser orOp (tokensOf "true or false or true")
-- (Right (Logical 1 Or (Logical 1 Or (Literal (Bool True)) (Literal (Bool False))) (Literal (Bool True))),[Token {tokenType = EOF, line = 1}])
orOp :: TokenParser (Expression 'Unresolved)
orOp = leftAssociative andOp parseOr

-- | Consumes a single 'or' operator.
-- >>> let (Right f, _) = runParser parseOr (tokensOf "or")
-- >>> displayExpr (f (Literal (Bool True)) (Literal (Bool False)))
-- "(or true false)"
parseOr :: TokenParser (Expression 'Unresolved -> Expression 'Unresolved -> Expression 'Unresolved)
parseOr = satisfy ((OR ==) . tokenType) ("Expect " <> displayTokenType OR <> ".") >>= \t -> pure (Logical (line t) Or)

-- | Consumes a single 'and' operator.
-- >>> let (Right f, _) = runParser parseAnd (tokensOf "and")
-- >>> displayExpr (f (Literal (Bool True)) (Literal (Bool False)))
-- "(and true false)"
parseAnd :: TokenParser (Expression 'Unresolved -> Expression 'Unresolved -> Expression 'Unresolved)
parseAnd = satisfy ((AND ==) . tokenType) ("Expect " <> displayTokenType AND <> ".") >>= \t -> pure (Logical (line t) And)

-- | Logical AND parser.
-- >>> runParser andOp (tokensOf "true and false and true")
-- (Right (Logical 1 And (Logical 1 And (Literal (Bool True)) (Literal (Bool False))) (Literal (Bool True))),[Token {tokenType = EOF, line = 1}])
andOp :: TokenParser (Expression 'Unresolved)
andOp = leftAssociative equality parseAnd

-- Equality

-- | Equality / inequality chain parser.
-- >>> runParser equality (tokensOf "1 == 2 != 3")
-- (Right (BinaryOperation 1 BangEqual (BinaryOperation 1 EqualEqual (Literal (Number 1.0)) (Literal (Number 2.0))) (Literal (Number 3.0))),[Token {tokenType = EOF, line = 1}])
equality :: TokenParser (Expression 'Unresolved)
equality = leftAssociative comparison (parseEq <|> parseNeq)

-- | Parses '==' operator.
-- >>> let (Right f, _) = runParser parseEq (tokensOf "==")
-- >>> displayExpr (f (Literal (Number 1)) (Literal (Number 2)))
-- "(== 1.0 2.0)"
parseEq :: TokenParser (Expression 'Unresolved -> Expression 'Unresolved -> Expression 'Unresolved)
parseEq = satisfy ((EQUAL_EQUAL ==) . tokenType) ("Expect " <> displayTokenType EQUAL_EQUAL <> ".") >>= \token -> pure (BinaryOperation (line token) EqualEqual)

-- | Parses '!=' operator.
-- >>> let (Right f, _) = runParser parseNeq (tokensOf "!=")
-- >>> displayExpr (f (Literal (Number 1)) (Literal (Number 2)))
-- "(!= 1.0 2.0)"
parseNeq :: TokenParser (Expression 'Unresolved -> Expression 'Unresolved -> Expression 'Unresolved)
parseNeq = satisfy ((BANG_EQUAL ==) . tokenType) ("Expect " <> displayTokenType BANG_EQUAL <> ".") >>= \token -> pure (BinaryOperation (line token) BangEqual)

-- Comparison

-- | Comparison chain parser (<, <=, >, >=).
-- >>> runParser comparison (tokensOf "1 < 2 <= 3")
-- (Right (BinaryOperation 1 LessEqual (BinaryOperation 1 Less (Literal (Number 1.0)) (Literal (Number 2.0))) (Literal (Number 3.0))),[Token {tokenType = EOF, line = 1}])
comparison :: TokenParser (Expression 'Unresolved)
comparison = leftAssociative term (parseGT <|> parseGTE <|> parseLT <|> parseLTE)

-- | Parses '>' operator.
-- >>> let (Right f, _) = runParser parseGT (tokensOf ">")
-- >>> displayExpr (f (Literal (Number 2)) (Literal (Number 1)))
-- "(> 2.0 1.0)"
parseGT :: TokenParser (Expression 'Unresolved -> Expression 'Unresolved -> Expression 'Unresolved)
parseGT = satisfy ((GREATER ==) . tokenType) ("Expect " <> displayTokenType GREATER <> ".") >>= \token -> pure (BinaryOperation (line token) Greater)

-- | Parses '>=' operator.
-- >>> let (Right f, _) = runParser parseGTE (tokensOf ">=")
-- >>> displayExpr (f (Literal (Number 2)) (Literal (Number 1)))
-- "(>= 2.0 1.0)"
parseGTE :: TokenParser (Expression 'Unresolved -> Expression 'Unresolved -> Expression 'Unresolved)
parseGTE = satisfy ((GREATER_EQUAL ==) . tokenType) ("Expect " <> displayTokenType GREATER_EQUAL <> ".") >>= \token -> pure (BinaryOperation (line token) GreaterEqual)

-- | Parses '<' operator.
-- >>> let (Right f, _) = runParser parseLT (tokensOf "<")
-- >>> displayExpr (f (Literal (Number 1)) (Literal (Number 2)))
-- "(< 1.0 2.0)"
parseLT :: TokenParser (Expression 'Unresolved -> Expression 'Unresolved -> Expression 'Unresolved)
parseLT = satisfy ((LESS ==) . tokenType) ("Expect " <> displayTokenType LESS <> ".") >>= \token -> pure (BinaryOperation (line token) Less)

-- | Parses '<=' operator.
-- >>> let (Right f, _) = runParser parseLTE (tokensOf "<=")
-- >>> displayExpr (f (Literal (Number 1)) (Literal (Number 2)))
-- "(<= 1.0 2.0)"
parseLTE :: TokenParser (Expression 'Unresolved -> Expression 'Unresolved -> Expression 'Unresolved)
parseLTE = satisfy ((LESS_EQUAL ==) . tokenType) ("Expect " <> displayTokenType LESS_EQUAL <> ".") >>= \token -> pure (BinaryOperation (line token) LessEqual)

-- Terms

-- | Addition / subtraction left-associative chain.
-- >>> runParser term (tokensOf "1 + 2 - 3 + 4")
-- (Right (BinaryOperation 1 Plus (BinaryOperation 1 BMinus (BinaryOperation 1 Plus (Literal (Number 1.0)) (Literal (Number 2.0))) (Literal (Number 3.0))) (Literal (Number 4.0))),[Token {tokenType = EOF, line = 1}])
term :: TokenParser (Expression 'Unresolved)
term = leftAssociative factor (parsePlus <|> parseMinus)

-- | Parses '+' operator.
-- >>> let (Right f, _) = runParser parsePlus (tokensOf "+")
-- >>> displayExpr (f (Literal (Number 1)) (Literal (Number 2)))
-- "(+ 1.0 2.0)"
parsePlus :: TokenParser (Expression 'Unresolved -> Expression 'Unresolved -> Expression 'Unresolved)
parsePlus = satisfy ((PLUS ==) . tokenType) ("Expect " <> displayTokenType PLUS <> ".") >>= \token -> pure (BinaryOperation (line token) Plus)

-- | Parses '-' operator (binary minus).
-- >>> let (Right f, _) = runParser parseMinus (tokensOf "-")
-- >>> displayExpr (f (Literal (Number 3)) (Literal (Number 1)))
-- "(- 3.0 1.0)"
parseMinus :: TokenParser (Expression 'Unresolved -> Expression 'Unresolved -> Expression 'Unresolved)
parseMinus = satisfy ((MINUS ==) . tokenType) ("Expect " <> displayTokenType MINUS <> ".") >>= \token -> pure (BinaryOperation (line token) BMinus)

-- Factors

-- | Multiplication / division left-associative chain.
-- >>> runParser factor (tokensOf "2 * 3 / 4")
-- (Right (BinaryOperation 1 Slash (BinaryOperation 1 Star (Literal (Number 2.0)) (Literal (Number 3.0))) (Literal (Number 4.0))),[Token {tokenType = EOF, line = 1}])
factor :: TokenParser (Expression 'Unresolved)
factor = leftAssociative unary (parseMul <|> parseDiv)

-- | Parses '*' operator.
-- >>> let (Right f, _) = runParser parseMul (tokensOf "*")
-- >>> displayExpr (f (Literal (Number 2)) (Literal (Number 3)))
-- "(* 2.0 3.0)"
parseMul :: TokenParser (Expression 'Unresolved -> Expression 'Unresolved -> Expression 'Unresolved)
parseMul = satisfy ((STAR ==) . tokenType) ("Expect " <> displayTokenType STAR <> ".") >>= \token -> pure (BinaryOperation (line token) Star)

-- | Parses '/' operator.
-- >>> let (Right f, _) = runParser parseDiv (tokensOf "/")
-- >>> displayExpr (f (Literal (Number 6)) (Literal (Number 2)))
-- "(/ 6.0 2.0)"
parseDiv :: TokenParser (Expression 'Unresolved -> Expression 'Unresolved -> Expression 'Unresolved)
parseDiv = satisfy ((SLASH ==) . tokenType) ("Expect " <> displayTokenType SLASH <> ".") >>= \token -> pure (BinaryOperation (line token) Slash)

-- Unary expressions

-- | Unary prefix parser for '!' and '-'.
-- >>> runParser unary (tokensOf "!-5")
-- (Right (UnaryOperation 1 Bang (UnaryOperation 1 UMinus (Literal (Number 5.0)))),[Token {tokenType = EOF, line = 1}])
unary :: TokenParser (Expression 'Unresolved)
unary = (parseBang <|> parseMinusUnary) <*> unary <|> call

-- | Function call (handles nested calls).
-- >>> runParser call (tokensOf "foo(1)(2)(3)")
-- (Right (Call 1 (Call 1 (Call 1 (VariableExpr 1 "foo" NotResolved) [Literal (Number 1.0)]) [Literal (Number 2.0)]) [Literal (Number 3.0)]),[Token {tokenType = EOF, line = 1}])
call :: TokenParser (Expression 'Unresolved)
call = do
  expr <- primary
  tt <- tokenType <$> peek
  case tt of
    LEFT_PAREN -> finishCall expr
    DOT -> finishCall expr
    _ -> pure expr

-- | Tail-recursive call finisher (continues parsing chained calls).
-- (Internal helper; prefer using 'call')
finishCall :: Expression 'Unresolved -> TokenParser (Expression 'Unresolved)
finishCall expr = do
  t <- peek
  case tokenType t of
    LEFT_PAREN -> functionArgs >>= finishCall . Call (line t) expr
    DOT -> getProperty expr >>= finishCall
    _ -> pure expr

getProperty :: Expression 'Unresolved -> TokenParser (Expression 'Unresolved)
getProperty expr = do
  void $ satisfy ((DOT ==) . tokenType) ("Expect " <> displayTokenType DOT <> ".")
  Token (IDENTIFIER name) lineNum <- satisfy (isIdentifier . tokenType) "Expect property name after '.'."
  pure (Get lineNum expr name)

-- | Parses argument list including parentheses.
functionArgs :: TokenParser [Expression 'Unresolved]
functionArgs =
  satisfy ((LEFT_PAREN ==) . tokenType) ("Expect " <> displayTokenType LEFT_PAREN <> ".")
    *> argumentList
    <* satisfy ((RIGHT_PAREN ==) . tokenType) ("Expect '" <> displayTokenType RIGHT_PAREN <> "' after arguments.")

-- | Parses arguments (comma separated) without consuming closing ')'.
-- (Internal helper; prefer 'functionArgs')
argumentList :: TokenParser [Expression 'Unresolved]
argumentList = do
  t <- peek
  if tokenType t == RIGHT_PAREN
    then pure []
    else go 0 []
  where
    go :: Int -> [Expression 'Unresolved] -> TokenParser [Expression 'Unresolved]
    go n acc = do
      -- Enforce limit before consuming the next argument so the error
      -- is emitted for the offending identifier token.
      if n >= 255
        then fail "Can't have more than 255 arguments."
        else do
          arg <- expression
          t <- peek
          case tokenType t of
            -- More arguments to parse
            COMMA -> do
              void $ satisfy ((COMMA ==) . tokenType) ("Expect " <> displayTokenType COMMA <> ".")
              go (n + 1) (arg : acc)
            -- Finished parsing arguments (correctly or will fail next on closing paren check)
            _ -> pure (reverse (arg : acc))

-- | Parses unary '!'.
-- >>> let (Right f, _) = runParser parseBang (tokensOf "!")
-- >>> displayExpr (f (Literal (Bool True)))
-- "(! true)"
parseBang :: TokenParser (Expression 'Unresolved -> Expression 'Unresolved)
parseBang = satisfy ((BANG ==) . tokenType) ("Expect " <> displayTokenType BANG <> ".") >>= \token -> pure (UnaryOperation (line token) Bang)

-- | Parses unary '-'.
-- >>> let (Right f, _) = runParser parseMinusUnary (tokensOf "-")
-- >>> displayExpr (f (Literal (Number 5)))
-- "(- 5.0)"
parseMinusUnary :: TokenParser (Expression 'Unresolved -> Expression 'Unresolved)
parseMinusUnary = satisfy ((MINUS ==) . tokenType) ("Expect " <> displayTokenType MINUS <> ".") >>= \token -> pure (UnaryOperation (line token) UMinus)

-- Primary expressions

-- | Primary expression parser: literals, grouping, identifiers.
-- >>> runParser primary (tokensOf "(1 + 2)")
-- (Right (Grouping (BinaryOperation 1 Plus (Literal (Number 1.0)) (Literal (Number 2.0)))),[Token {tokenType = EOF, line = 1}])
-- >>> runParser primary (tokensOf "identifier")
-- (Right (VariableExpr 1 "identifier" NotResolved),[Token {tokenType = EOF, line = 1}])
primary :: TokenParser (Expression 'Unresolved)
primary = do
  t <- peek
  case tokenType t of
    FALSE -> parseFalse
    TRUE -> parseTrue
    NIL -> parseNil
    NUMBER _ _ -> parseNumber
    STRING _ _ -> parseString
    LEFT_PAREN -> parseGrouping
    THIS -> parseThis
    SUPER -> parseSuper
    IDENTIFIER _ -> parseVarName
    _ -> fail "Expect expression."

-- | Parses 'false'.
-- >>> runParser parseFalse (tokensOf "false")
-- (Right (Literal (Bool False)),[Token {tokenType = EOF, line = 1}])
parseFalse :: TokenParser (Expression 'Unresolved)
parseFalse = satisfy ((FALSE ==) . tokenType) ("Expect " <> displayTokenType FALSE <> ".") >> pure (Literal (Bool False))

-- | Parses 'true'.
-- >>> runParser parseTrue (tokensOf "true")
-- (Right (Literal (Bool True)),[Token {tokenType = EOF, line = 1}])
parseTrue :: TokenParser (Expression 'Unresolved)
parseTrue = satisfy ((TRUE ==) . tokenType) ("Expect " <> displayTokenType TRUE <> ".") >> pure (Literal (Bool True))

-- | Parses 'nil'.
-- >>> runParser parseNil (tokensOf "nil")
-- (Right (Literal Nil),[Token {tokenType = EOF, line = 1}])
parseNil :: TokenParser (Expression 'Unresolved)
parseNil = satisfy ((NIL ==) . tokenType) ("Expect " <> displayTokenType NIL <> ".") >> pure (Literal Nil)

-- | Parses numeric literal.
-- >>> runParser parseNumber (tokensOf "42")
-- (Right (Literal (Number 42.0)),[Token {tokenType = EOF, line = 1}])
parseNumber :: TokenParser (Expression 'Unresolved)
parseNumber = do
  Token (NUMBER _ n) _ <- satisfy (isNumber . tokenType) "a number"
  pure (Literal (Number n))

-- | Parses string literal.
-- >>> runParser parseString (tokensOf "\"hello\"")
-- (Right (Literal (String "hello")),[Token {tokenType = EOF, line = 1}])
parseString :: TokenParser (Expression 'Unresolved)
parseString = do
  Token (STRING _ s) _ <- satisfy (isString . tokenType) "a string"
  pure (Literal (String s))

-- | Parses parenthesized expression.
-- >>> runParser parseGrouping (tokensOf "(1 + 2)")
-- (Right (Grouping (BinaryOperation 1 Plus (Literal (Number 1.0)) (Literal (Number 2.0)))),[Token {tokenType = EOF, line = 1}])
parseGrouping :: TokenParser (Expression 'Unresolved)
parseGrouping = Grouping <$> parens expression

-- | Parses 'this' keyword as variable expression.
-- >>> runParser parseThis (tokensOf "this")
-- (Right (This 1 NotResolved),[Token {tokenType = EOF, line = 1}])
parseThis :: TokenParser (Expression 'Unresolved)
parseThis = do
  Token {tokenType = THIS, line = lineNum} <- satisfy ((THIS ==) . tokenType) ("Expect " <> displayTokenType THIS <> ".")
  pure (This lineNum NotResolved)

-- | Parses variable name as expression.
-- >>> runParser parseVarName (tokensOf "foo")
-- (Right (VariableExpr 1 "foo" NotResolved),[Token {tokenType = EOF, line = 1}])
parseVarName :: TokenParser (Expression 'Unresolved)
parseVarName = do
  Token {tokenType = IDENTIFIER name, line = lineNum} <- satisfy (isIdentifier . tokenType) "Expect expression."
  pure (VariableExpr lineNum name NotResolved)

-- | Parses 'super' keyword as expression.
-- >>> runParser parseSuper (tokensOf "super.mymethod")
-- (Right (Super 1 "mymethod" NotResolved NotResolved),[Token {tokenType = EOF, line = 1}])
parseSuper :: TokenParser (Expression 'Unresolved)
parseSuper = do
  Token {tokenType = SUPER, line = lineNum} <- satisfy ((SUPER ==) . tokenType) ("Expect " <> displayTokenType SUPER <> ".")
  void $ satisfy ((DOT ==) . tokenType) "Expect '.' after 'super'."
  Token {tokenType = IDENTIFIER methodName} <- satisfy (isIdentifier . tokenType) "Expect superclass method name."
  pure (Super lineNum methodName NotResolved NotResolved)

-- Helpers

-- | Utility: parses '(' p ')'.
-- >>> runParser (parens parseNumber) (tokensOf "(123)")
-- (Right (Literal (Number 123.0)),[Token {tokenType = EOF, line = 1}])
parens :: TokenParser a -> TokenParser a
parens p = do
  void $ satisfy ((LEFT_PAREN ==) . tokenType) ("Expect " <> displayTokenType LEFT_PAREN <> ".")
  result <- p
  void $ satisfy ((RIGHT_PAREN ==) . tokenType) ("Expect " <> displayTokenType RIGHT_PAREN <> ".")
  pure result

-- | Folds a left-associative chain of (operand (op operand)*).
-- >>> runParser (leftAssociative parseNumber parsePlus) (tokensOf "1 + 2 + 3")
-- (Right (BinaryOperation 1 Plus (BinaryOperation 1 Plus (Literal (Number 1.0)) (Literal (Number 2.0))) (Literal (Number 3.0))),[Token {tokenType = EOF, line = 1}])
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
displayExpr :: Expression a -> String
displayExpr (Literal lit) = displayLit lit
displayExpr (UnaryOperation _ op expr) = "(" <> displayUnOp op <> " " <> displayExpr expr <> ")"
displayExpr (BinaryOperation _ op e1 e2) = "(" <> displayBinOp op <> " " <> displayExpr e1 <> " " <> displayExpr e2 <> ")"
displayExpr (Call _ callee args) = "(call " <> displayExpr callee <> " " <> unwords (map displayExpr args) <> ")"
displayExpr (Get _ object propName) = "(get " <> displayExpr object <> " " <> propName <> ")"
displayExpr (Set _ object propName value) = "(set " <> displayExpr object <> " " <> propName <> " " <> displayExpr value <> ")"
displayExpr (This _ _) = "this"
displayExpr (Super _ methodName _ _) = "(super " <> methodName <> ")"
displayExpr (Grouping expr) = "(group " <> displayExpr expr <> ")"
displayExpr (VariableExpr _ name _) = name
displayExpr (VariableAssignment _ name expr _) = "(= " <> name <> " " <> displayExpr expr <> ")"
displayExpr (Logical _ op e1 e2) = "(" <> displayLogicOp op <> " " <> displayExpr e1 <> " " <> displayExpr e2 <> ")"

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
