module Expression.Parser where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Expression.AST (Expression)
import Expression.AST qualified as AST
import Token (Token (..), TokenType)
import Token qualified as T

-- | Basic parser type. For an error type `e`, an input type `s`, and an output type `a`.
newtype Parser e s a = Parser
  { runParser :: s -> Either e (a, s)
  }

-- | Concrete type for our expression parser.
type TokenParser = Parser String [Token]

instance Functor TokenParser where
  fmap f p = Parser $ \input -> do
    (parsedValue, rest) <- runParser p input
    pure (f parsedValue, rest)

instance Applicative TokenParser where
  pure a = Parser $ \s -> pure (a, s)

  fP <*> pP = Parser $ \input -> do
    (parsedFun, fRest) <- runParser fP input
    (parsedValue, pRest) <- runParser pP fRest
    pure (parsedFun parsedValue, pRest)

instance Monad TokenParser where
  p >>= f = Parser $ \input -> do
    (parsedValue, rest) <- runParser p input
    runParser (f parsedValue) rest

instance Alternative TokenParser where
  empty = fail "Parser: empty"

  parserA <|> parserB = Parser $ \input ->
    case runParser parserA input of
      Right result -> Right result
      Left _ -> runParser parserB input

instance MonadFail TokenParser where
  fail = Parser . const . Left

{-
  In the same way of the defined grammar, which we reproduce here:

expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;

  We map each of these into functions using our `Parser` defined in `Expression.Parser.Type`.
-}

-- >>> import Token (Token (..), TokenType (..))
-- >>> tokenList = [Token {tokenType = NUMBER "1" 1.0, line = 1},Token {tokenType = EQUAL_EQUAL, line = 1},Token {tokenType = NUMBER "2" 2.0, line = 1},Token {tokenType = EQUAL_EQUAL, line = 1},Token {tokenType = NUMBER "3" 3.0, line = 1},Token {tokenType = EQUAL_EQUAL, line = 1},Token {tokenType = NUMBER "4" 4.0, line = 1},Token {tokenType = EOF, line = 1}]
-- >>> runParser expression tokenList
-- Right (Binary EqualEqual (Binary EqualEqual (Binary EqualEqual (Literal (Number 1.0)) (Literal (Number 2.0))) (Literal (Number 3.0))) (Literal (Number 4.0)),[Token {tokenType = EOF, line = 1}])
-- >>> tokenList = [Token {tokenType = LEFT_PAREN, line = 1},Token {tokenType = NUMBER "1" 1.0, line = 1},Token {tokenType = PLUS, line = 1},Token {tokenType = NUMBER "2" 2.0, line = 1},Token {tokenType = RIGHT_PAREN, line = 1},Token {tokenType = PLUS, line = 1},Token {tokenType = LEFT_PAREN, line = 1},Token {tokenType = NUMBER "3" 3.0, line = 1},Token {tokenType = PLUS, line = 1},Token {tokenType = NUMBER "4" 4.0, line = 1},Token {tokenType = RIGHT_PAREN, line = 1},Token {tokenType = EOF, line = 1}]
-- >>> runParser expression tokenList
-- Right (Binary Plus (Grouping (Binary Plus (Literal (Number 1.0)) (Literal (Number 2.0)))) (Grouping (Binary Plus (Literal (Number 3.0)) (Literal (Number 4.0)))),[Token {tokenType = EOF, line = 1}])
expression :: TokenParser Expression
expression = equality

-- Equality
equality :: TokenParser Expression
equality = do
  left <- comparison
  equalityRest left

-- Left-associativity chaining of additional equalities
equalityRest :: Expression -> TokenParser Expression
equalityRest left =
  ( do
      op <- matchEqualityOp
      right <- comparison
      equalityRest (AST.Binary op left right)
  )
    <|> pure left
  where
    matchEqualityOp = do
      t <- matchTokenType T.EQUAL_EQUAL <|> matchTokenType T.BANG_EQUAL
      pure (if tokenType t == T.EQUAL_EQUAL then AST.EqualEqual else AST.BangEqual)

-- Comparison
comparison :: TokenParser Expression
comparison = do
  left <- term
  comparisonRest left

comparisonRest :: Expression -> TokenParser Expression
comparisonRest left =
  ( do
      op <- matchComparisonOp
      right <- term
      comparisonRest (AST.Binary op left right)
  )
    <|> pure left
  where
    matchComparisonOp = do
      t <-
        matchTokenType T.GREATER
          <|> matchTokenType T.GREATER_EQUAL
          <|> matchTokenType T.LESS
          <|> matchTokenType T.LESS_EQUAL
      case tokenType t of
        T.GREATER -> pure AST.Greater
        T.GREATER_EQUAL -> pure AST.GreaterEqual
        T.LESS -> pure AST.Less
        T.LESS_EQUAL -> pure AST.LessEqual
        _ -> empty

-- Terms
term :: TokenParser Expression
term = do
  left <- factor
  termRest left

termRest :: Expression -> TokenParser Expression
termRest left =
  ( do
      op <- matchTermOp
      right <- factor
      termRest (AST.Binary op left right)
  )
    <|> pure left
  where
    matchTermOp = do
      t <- matchTokenType T.PLUS <|> matchTokenType T.MINUS
      pure (if tokenType t == T.PLUS then AST.Plus else AST.BMinus)

-- Factors
factor :: TokenParser Expression
factor = do
  left <- unary
  factorRest left

factorRest :: Expression -> TokenParser Expression
factorRest left =
  ( do
      op <- matchFactorOp
      right <- unary
      factorRest (AST.Binary op left right)
  )
    <|> pure left
  where
    matchFactorOp = do
      t <- matchTokenType T.STAR <|> matchTokenType T.SLASH
      pure (if tokenType t == T.STAR then AST.Star else AST.Slash)

-- Unary expressions
unary :: TokenParser Expression
unary =
  ( do
      op <- matchUnaryOp
      AST.Unary op <$> unary
  )
    <|> primary
  where
    matchUnaryOp = do
      t <- matchTokenType T.BANG <|> matchTokenType T.MINUS
      pure (if tokenType t == T.BANG then AST.Bang else AST.UMinus)

-- Primary expressions
primary :: TokenParser Expression
primary =
  parseFalse
    <|> parseTrue
    <|> parseNil
    <|> parseNumber
    <|> parseString
    <|> ( AST.Grouping
            <$> parens expression
        )
  where
    parseFalse = do
      void $ matchTokenType T.FALSE
      pure (AST.Literal (AST.Bool False))
    parseTrue = do
      void $ matchTokenType T.TRUE
      pure (AST.Literal (AST.Bool True))
    parseNil = do
      void $ matchTokenType T.NIL
      pure (AST.Literal AST.Nil)
    parseNumber = do
      t <-
        satisfy
          ( \t -> case tokenType t of
              T.NUMBER _ _ -> True
              _ -> False
          )
      case t of
        Token (T.NUMBER _ n) _ -> pure (AST.Literal (AST.Number n))
        _ -> empty
    parseString = do
      t <-
        satisfy
          ( \t -> case tokenType t of
              T.STRING _ _ -> True
              _ -> False
          )
      case t of
        Token (T.STRING _ s) _ -> pure (AST.Literal (AST.String s))
        _ -> empty

-- Helpers

satisfy :: (Token -> Bool) -> TokenParser Token
satisfy predicate = Parser $ \case
  (t : tt) | predicate t -> Right (t, tt)
  _ -> Left "Parser: unexpected token"

matchTokenType :: TokenType -> TokenParser Token
matchTokenType tType = satisfy (\t -> tokenType t == tType)

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
  pure $ foldl (\acc (op, next) -> op acc next) first rest
