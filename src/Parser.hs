module Parser (TokenParser, Parser (Parser, runParser), matchTokenType, satisfy) where

import Control.Applicative (Alternative (..))
import Token (Token (tokenType), TokenType, toString)

-- | Basic parser type. For an error type `e`, an input type `s`, and an output type `a`.
-- A rough equivalent to this in Rust would be:
--
-- @
-- pub struct Parser\<E, S, A\> {
--     run_parser: fn(S) -> Result\<(A, S), E\>,
-- }
-- @
newtype Parser e s a = Parser
  { runParser :: s -> Either e (a, s)
  }

-- | Concrete type for our program parser.
--
-- It's the same as @Parser String [Token] a@ for a generic type `a`, which itself just
-- contains a function @[Token] -> Either String (a, [Token])@ under the `runParser` field.
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

-- Helpers

satisfy :: (Token -> Bool) -> String -> TokenParser Token
satisfy predicate failMsg = Parser $ \case
  (t : tt) | predicate t -> Right (t, tt)
  _ -> Left failMsg

matchTokenType :: TokenType -> TokenParser Token
matchTokenType tType = satisfy (\t -> tokenType t == tType) ("expected " <> toString tType)
