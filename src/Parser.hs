module Parser
  ( TokenParser,
    Parser (Parser, runParser),
    ParseError (..),
    displayParseErr,
    satisfy,
    peek,
    consume,
  )
where

import Control.Applicative (Alternative (..))
import Data.Functor (void)
import Scanner.Error (Error (..), displayErr)
import Token (Token (..), TokenType (..), displayTokenType)

-- | Basic generic parser type. For an error type `e`, an input type `s`, and an output type `a`.
-- A rough equivalent to this in Rust would be:
--
-- @
-- struct Parser\<E, S, A\> {
--     run_parser: fn(S) -> Result\<(A, S), E\>,
-- }
-- @
newtype Parser e s a = Parser
  { runParser :: s -> (Either e a, s)
  }

-- | A parse error, containing the token where the error happened (if any) and a message.
data ParseError = ParseError
  { errToken :: Maybe Token,
    errMessage :: String
  }
  deriving stock (Show, Eq)

displayParseErr :: ParseError -> String
displayParseErr (ParseError (Just (Token EOF line)) msg) = displayErr (Error msg line " at end")
displayParseErr (ParseError (Just (Token tType line)) msg) = displayErr (Error msg line (" at '" <> displayTokenType tType <> "'"))
displayParseErr (ParseError Nothing msg) = displayErr (Error msg 0 " at unknown") -- catchall... should not happen

-- | Concrete type for our program parser.
--
-- It's the same as @Parser ParseError [Token] a@ for a generic type `a`, which itself just
-- contains a function @[Token] -> Either ParseError (a, [Token])@ under the `runParser` field.
type TokenParser = Parser ParseError [Token]

instance Functor TokenParser where
  fmap :: (a -> b) -> TokenParser a -> TokenParser b
  fmap f p = Parser $ \input ->
    let (parsedValue, rest) = runParser p input
     in (f <$> parsedValue, rest)

instance Applicative TokenParser where
  pure :: a -> TokenParser a
  pure a = Parser (Right a,)

  (<*>) :: TokenParser (a -> b) -> TokenParser a -> TokenParser b
  fP <*> pP = Parser $ \input ->
    let (parsedFun, fRest) = runParser fP input
        (parsedValue, pRest) = runParser pP fRest
     in (parsedFun <*> parsedValue, pRest)

instance Monad TokenParser where
  (>>=) :: TokenParser a -> (a -> TokenParser b) -> TokenParser b
  p >>= f = Parser $ \input -> do
    let (parsedValue, rest) = runParser p input
    case parsedValue of
      Left err -> (Left err, rest)
      Right v -> runParser (f v) rest

instance Alternative TokenParser where
  empty :: TokenParser a
  empty = fail "empty parser"

  -- Try the first parser, if it fails try the second one without consuming input.
  (<|>) :: TokenParser a -> TokenParser a -> TokenParser a
  parserA <|> parserB = Parser $ \input ->
    case runParser parserA input of
      (Right result, rest) -> (Right result, rest)
      (Left _, _) -> runParser parserB input

instance MonadFail TokenParser where
  fail :: String -> TokenParser a
  fail msg = Parser $ \case
    (t : tt) -> (Left $ ParseError (Just t) msg, tt)
    [] -> (Left $ ParseError Nothing msg, [])

-- Helpers

peek :: TokenParser Token
peek = Parser $ \case
  (t : tt) -> (Right t, t : tt)
  [] -> (Left $ ParseError Nothing "Unexpected end of input.", [])

satisfy :: (Token -> Bool) -> String -> TokenParser Token
satisfy predicate failMsg = Parser $ \case
  (t : tt) | predicate t -> (Right t, tt)
  (t : tt) -> (Left $ ParseError (Just t) failMsg, tt)
  [] -> (Left $ ParseError Nothing failMsg, [])

consume :: TokenParser ()
consume = void $ satisfy (const True) "Unexpected end of input."
