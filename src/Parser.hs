module Parser (TokenParser, Parser (Parser, runParser), ParseError (..), prettyPrintParseErr, matchTokenType, satisfy, peekToken) where

import Control.Applicative (Alternative (..))
import Scanner.Error (Error (..), prettyPrintErr)
import Token (Token (..), TokenType, toString)
import Token qualified as T

-- | Basic parser type. For an error type `e`, an input type `s`, and an output type `a`.
-- A rough equivalent to this in Rust would be:
--
-- @
-- struct Parser\<E, S, A\> {
--     run_parser: fn(S) -> Result\<(A, S), E\>,
-- }
-- @
newtype Parser e s a = Parser
  { runParser :: s -> Either e (a, s)
  }

-- newtype ParseErrors = ParseErrors [ParseError]
--   deriving stock (Show, Eq)
--   deriving newtype (Semigroup, Monoid)

-- toErrList :: ParseErrors -> [ParseError]
-- toErrList (ParseErrors errs) = errs

prettyPrintParseErr :: ParseError -> String
prettyPrintParseErr (ParseError (Just (Token T.EOF line)) msg) = prettyPrintErr (Error msg line " at end")
prettyPrintParseErr (ParseError (Just (Token tType line)) msg) = prettyPrintErr (Error msg line (" at '" <> toString tType <> "'"))
prettyPrintParseErr (ParseError Nothing msg) = prettyPrintErr (Error msg 0 " at unknown") -- catchall... should not happen

data ParseError = ParseError
  { errToken :: Maybe Token,
    errMessage :: String
  }
  deriving stock (Show, Eq)

-- | Concrete type for our program parser.
--
-- It's the same as @Parser ParseError [Token] a@ for a generic type `a`, which itself just
-- contains a function @[Token] -> Either ParseError (a, [Token])@ under the `runParser` field.
type TokenParser = Parser ParseError [Token]

instance Functor TokenParser where
  fmap :: (a -> b) -> TokenParser a -> TokenParser b
  fmap f p = Parser $ \input -> do
    (parsedValue, rest) <- runParser p input
    pure (f parsedValue, rest)

instance Applicative TokenParser where
  pure :: a -> TokenParser a
  pure a = Parser $ \s -> pure (a, s)

  (<*>) :: TokenParser (a -> b) -> TokenParser a -> TokenParser b
  fP <*> pP = Parser $ \input -> do
    (parsedFun, fRest) <- runParser fP input
    (parsedValue, pRest) <- runParser pP fRest
    pure (parsedFun parsedValue, pRest)

instance Monad TokenParser where
  (>>=) :: TokenParser a -> (a -> TokenParser b) -> TokenParser b
  p >>= f = Parser $ \input -> do
    (parsedValue, rest) <- runParser p input
    runParser (f parsedValue) rest

instance Alternative TokenParser where
  empty :: TokenParser a
  empty = fail "empty parser"

  (<|>) :: TokenParser a -> TokenParser a -> TokenParser a
  parserA <|> parserB = Parser $ \input ->
    case runParser parserA input of
      Right result -> Right result
      Left _ -> runParser parserB input

instance MonadFail TokenParser where
  fail :: String -> TokenParser a
  fail msg = Parser $ \case
    (t : _) -> Left (ParseError (Just t) msg)
    [] -> Left (ParseError Nothing msg)

-- Helpers

peekToken :: TokenParser Token
peekToken = Parser $ \case
  [] -> Left (ParseError Nothing "Unexpected end of input.")
  (t : tt) -> Right (t, t : tt)

satisfy :: (Token -> Bool) -> String -> TokenParser Token
satisfy predicate failMsg = Parser $ \case
  (t : tt) | predicate t -> Right (t, tt)
  (t : _) -> Left (ParseError (Just t) failMsg)
  [] -> Left (ParseError Nothing failMsg)

matchTokenType :: TokenType -> TokenParser Token
matchTokenType tType = satisfy (\t -> tokenType t == tType) ("Expect " <> toString tType <> ".")
