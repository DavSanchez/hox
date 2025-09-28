module Expression.Parser (expression) where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Expression.AST (Expression)
import Expression.AST qualified as AST
import Parser (TokenParser, matchTokenType, satisfy)
import Token (Token (..), isNumber, isString)
import Token qualified as T

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

  We map each of these into functions using our `Parser` defined in the `Parser` module.
-}

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
parseEq = matchTokenType T.EQUAL_EQUAL >>= \token -> pure (AST.Binary (AST.BinaryOperator {bOpLine = T.line token, bOp = AST.EqualEqual}))

parseNeq :: TokenParser (Expression -> Expression -> Expression)
parseNeq = matchTokenType T.BANG_EQUAL >>= \token -> pure (AST.Binary (AST.BinaryOperator {bOpLine = T.line token, bOp = AST.BangEqual}))

-- Comparison
comparison :: TokenParser Expression
comparison = leftAssociative term (parseGT <|> parseGTE <|> parseLT <|> parseLTE)

parseGT :: TokenParser (Expression -> Expression -> Expression)
parseGT = matchTokenType T.GREATER >>= \token -> pure (AST.Binary (AST.BinaryOperator {bOpLine = T.line token, bOp = AST.Greater}))

parseGTE :: TokenParser (Expression -> Expression -> Expression)
parseGTE = matchTokenType T.GREATER_EQUAL >>= \token -> pure (AST.Binary (AST.BinaryOperator {bOpLine = T.line token, bOp = AST.GreaterEqual}))

parseLT :: TokenParser (Expression -> Expression -> Expression)
parseLT = matchTokenType T.LESS >>= \token -> pure (AST.Binary (AST.BinaryOperator {bOpLine = T.line token, bOp = AST.Less}))

parseLTE :: TokenParser (Expression -> Expression -> Expression)
parseLTE = matchTokenType T.LESS_EQUAL >>= \token -> pure (AST.Binary (AST.BinaryOperator {bOpLine = T.line token, bOp = AST.LessEqual}))

-- Terms
term :: TokenParser Expression
term = leftAssociative factor (parsePlus <|> parseMinus)

parsePlus :: TokenParser (Expression -> Expression -> Expression)
parsePlus = matchTokenType T.PLUS >>= \token -> pure (AST.Binary (AST.BinaryOperator {bOpLine = T.line token, bOp = AST.Plus}))

parseMinus :: TokenParser (Expression -> Expression -> Expression)
parseMinus = matchTokenType T.MINUS >>= \token -> pure (AST.Binary (AST.BinaryOperator {bOpLine = T.line token, bOp = AST.BMinus}))

-- Factors
factor :: TokenParser Expression
factor = leftAssociative unary (parseMul <|> parseDiv)

parseMul :: TokenParser (Expression -> Expression -> Expression)
parseMul = matchTokenType T.STAR >>= \token -> pure (AST.Binary (AST.BinaryOperator {bOpLine = T.line token, bOp = AST.Star}))

parseDiv :: TokenParser (Expression -> Expression -> Expression)
parseDiv = matchTokenType T.SLASH >>= \token -> pure (AST.Binary (AST.BinaryOperator {bOpLine = T.line token, bOp = AST.Slash}))

-- Unary expressions
unary :: TokenParser Expression
unary = ((parseBang <|> parseMinusUnary) <*> unary) <|> primary

parseBang :: TokenParser (Expression -> Expression)
parseBang = matchTokenType T.BANG >>= \token -> pure (AST.Unary (AST.UnaryOperator {uOpLine = T.line token, uOp = AST.Bang}))

parseMinusUnary :: TokenParser (Expression -> Expression)
parseMinusUnary = matchTokenType T.MINUS >>= \token -> pure (AST.Unary (AST.UnaryOperator {uOpLine = T.line token, uOp = AST.UMinus}))

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
parseFalse = matchTokenType T.FALSE >> pure (AST.Literal (AST.Bool False))

parseTrue :: TokenParser Expression
parseTrue = matchTokenType T.TRUE >> pure (AST.Literal (AST.Bool True))

parseNil :: TokenParser Expression
parseNil = matchTokenType T.NIL >> pure (AST.Literal AST.Nil)

parseNumber :: TokenParser Expression
parseNumber = do
  Token (T.NUMBER _ n) _ <- satisfy (isNumber . tokenType) "expected a number"
  pure (AST.Literal (AST.Number n))

parseString :: TokenParser Expression
parseString = do
  Token (T.STRING _ s) _ <- satisfy (isString . tokenType) "expected a string"
  pure (AST.Literal (AST.String s))

parseGrouping :: TokenParser Expression
parseGrouping = AST.Grouping <$> parens expression

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
