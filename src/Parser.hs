module Parser (TokenParser, Parser (Parser, runParser), toErrList, prettyPrintParseErr, ParseErrors, matchTokenType, satisfy) where

import Control.Applicative (Alternative (..))
import Token (Token (..), TokenType, toString)

-- | Basic parser type. For an error type `e`, an input type `s`, and an output type `a`.
-- A rough equivalent to this in Rust would be:
--
-- @
-- struct Parser\<E, S, A\> {
--     run_parser: fn(S) -> Result\<(A, S, E), E\>,
-- }
-- @
--
-- Our "`Ok`" variant (here as `Right`) still contains the error type `e` because we want to
-- accumulate errors as we parse. This is useful for reporting multiple errors in one go instead of
-- stopping at the first one.
newtype Parser e s a = Parser
  { runParser :: s -> Either e (a, s, e)
  }

newtype ParseErrors = ParseErrors [ParseError]
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

toErrList :: ParseErrors -> [ParseError]
toErrList (ParseErrors errs) = errs

prettyPrintParseErr :: ParseError -> String
prettyPrintParseErr (ParseError line msg) = msg <> "\n[line " <> show line <> "]"

data ParseError = ParseError
  { errLine :: Int,
    errMessage :: String
  }
  deriving stock (Show, Eq)

-- | Concrete type for our program parser.
--
-- It's the same as @Parser ParseErrors [Token] a@ for a generic type `a`, which itself just
-- contains a function @[Token] -> Either ParseErrors (a, [Token])@ under the `runParser` field.
type TokenParser = Parser ParseErrors [Token]

instance Functor TokenParser where
  fmap :: (a -> b) -> TokenParser a -> TokenParser b
  fmap f p = Parser $ \input -> do
    (parsedValue, rest, errs) <- runParser p input
    pure (f parsedValue, rest, errs)

instance Applicative TokenParser where
  pure :: a -> TokenParser a
  pure a = Parser $ \s -> pure (a, s, mempty)

  (<*>) :: TokenParser (a -> b) -> TokenParser a -> TokenParser b
  fP <*> pP = Parser $ \input -> case runParser fP input of
    Left err -> Left err
    Right (parsedFun, fRest, fErrs) -> case runParser pP fRest of
      Left err -> Left (fErrs <> err)
      Right (parsedValue, pRest, pErrs) -> Right (parsedFun parsedValue, pRest, fErrs <> pErrs)

instance Monad TokenParser where
  (>>=) :: TokenParser a -> (a -> TokenParser b) -> TokenParser b
  p >>= f = Parser $ \input -> case runParser p input of
    Left err -> Left err
    Right (parsedValue, rest, errs) -> case runParser (f parsedValue) rest of
      Left err -> Left (errs <> err)
      Right (newValue, newRest, newErrs) -> Right (newValue, newRest, errs <> newErrs)

instance Alternative TokenParser where
  empty :: TokenParser a
  empty = Parser $ \_ -> Left (ParseErrors [])

  (<|>) :: TokenParser a -> TokenParser a -> TokenParser a
  parserA <|> parserB = Parser $ \input ->
    case runParser parserA input of
      Right result -> Right result
      Left e -> case runParser parserB input of
        -- TODO handle panic mode for error recovery
        Right (a, t, e') -> Right (a, t, e <> e')
        Left e' -> Left (e <> e')

instance MonadFail TokenParser where
  fail :: String -> TokenParser a
  fail msg = Parser $ \input ->
    let position = case input of
          (t : _) -> line t
          [] -> 0
     in Left (ParseErrors [ParseError position msg])

-- Helpers

satisfy :: (Token -> Bool) -> String -> TokenParser Token
satisfy predicate failMsg = Parser $ \case
  (t : tt)
    | predicate t -> Right (t, tt, mempty)
    | otherwise -> Left (ParseErrors [ParseError (line t) failMsg])
  [] -> Left (ParseErrors [ParseError 0 failMsg])

matchTokenType :: TokenType -> TokenParser Token
matchTokenType tType = satisfy (\t -> tokenType t == tType) ("expected " <> toString tType)
