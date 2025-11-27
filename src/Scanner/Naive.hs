module Scanner.Naive (naiveScanTokens) where

import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Scanner.Internal (TokenResult, isAlpha, isAlphaNum, keywords, syntaxError, validToken)
import Text.Read (readMaybe)
import Token (TokenType (..))

-- | Auxiliary function that scans the input strings and tracks the state: the current line number and the accumulated tokens
naiveScanTokens ::
  -- | Input string
  String ->
  -- | Current line number
  Int ->
  -- | Accumulated tokens
  [TokenResult] ->
  -- | Resulting list of tokens
  NonEmpty TokenResult
-- We start with the base case: no more characters to process
naiveScanTokens "" l tt = validToken EOF l :| tt
-- Single character tokens
-- >>> naiveScanTokens "((\n(((" 1 []
-- [Right (Token {tokenType = LEFT_PAREN, line = 1}),Right (Token {tokenType = LEFT_PAREN, line = 1}),Right (Token {tokenType = LEFT_PAREN, line = 2}),Right (Token {tokenType = LEFT_PAREN, line = 2}),Right (Token {tokenType = LEFT_PAREN, line = 2}),Right (Token {tokenType = EOF, line = 2})]
naiveScanTokens ('(' : ss) l tt = naiveScanTokens ss l (validToken LEFT_PAREN l : tt)
naiveScanTokens (')' : ss) l tt = naiveScanTokens ss l (validToken RIGHT_PAREN l : tt)
naiveScanTokens ('{' : ss) l tt = naiveScanTokens ss l (validToken LEFT_BRACE l : tt)
naiveScanTokens ('}' : ss) l tt = naiveScanTokens ss l (validToken RIGHT_BRACE l : tt)
naiveScanTokens (',' : ss) l tt = naiveScanTokens ss l (validToken COMMA l : tt)
naiveScanTokens ('.' : ss) l tt = naiveScanTokens ss l (validToken DOT l : tt)
naiveScanTokens ('-' : ss) l tt = naiveScanTokens ss l (validToken MINUS l : tt)
naiveScanTokens ('+' : ss) l tt = naiveScanTokens ss l (validToken PLUS l : tt)
naiveScanTokens (';' : ss) l tt = naiveScanTokens ss l (validToken SEMICOLON l : tt)
naiveScanTokens ('*' : ss) l tt = naiveScanTokens ss l (validToken STAR l : tt)
-- Operators
naiveScanTokens ('!' : '=' : ss) l tt = naiveScanTokens ss l (validToken BANG_EQUAL l : tt)
naiveScanTokens ('!' : ss) l tt = naiveScanTokens ss l (validToken BANG l : tt)
naiveScanTokens ('=' : '=' : ss) l tt = naiveScanTokens ss l (validToken EQUAL_EQUAL l : tt)
naiveScanTokens ('=' : ss) l tt = naiveScanTokens ss l (validToken EQUAL l : tt)
naiveScanTokens ('>' : '=' : ss) l tt = naiveScanTokens ss l (validToken GREATER_EQUAL l : tt)
naiveScanTokens ('>' : ss) l tt = naiveScanTokens ss l (validToken GREATER l : tt)
naiveScanTokens ('<' : '=' : ss) l tt = naiveScanTokens ss l (validToken LESS_EQUAL l : tt)
naiveScanTokens ('<' : ss) l tt = naiveScanTokens ss l (validToken LESS l : tt)
-- Longer lexemes:
-- Comments: Consume the rest of the line as a comment
naiveScanTokens ('/' : '/' : ss) l tt = naiveScanTokens (dropWhile (/= '\n') ss) l tt
-- Slash case (needs comment handling first, above)
naiveScanTokens ('/' : ss) l tt = naiveScanTokens ss l (validToken SLASH l : tt)
-- Whitespaces
naiveScanTokens (s : ss) l tt
  | s == ' ' || s == '\r' || s == '\t' =
      naiveScanTokens ss l tt
naiveScanTokens ('\n' : ss) l tt = naiveScanTokens ss (l + 1) tt -- Newline handling increments the line number
-- String literals
naiveScanTokens ('"' : ss) l tt =
  -- Looking for a starting quote
  let stringContent = takeWhile (/= '"') ss -- Take characters until the next quote
      newLinesInString = length $ filter (== '\n') stringContent
   in if stringContent == ss -- If there are no closing quotes, the calculated remainder is the same as the remaining input string
        then syntaxError "Unterminated string." l "" :| tt
        else
          naiveScanTokens
            (drop (length stringContent + 1 {- For the closing quote -}) ss)
            (l + newLinesInString)
            (validToken (STRING (show stringContent) stringContent) l : tt)
-- Number literals
naiveScanTokens input@(c : ss) l tt
  | isDigit c =
      let integerPart = c : takeWhile isDigit ss
          decimalPart = case drop (length integerPart) input of
            ('.' : rest) ->
              let decimals = takeWhile isDigit rest
               in if null decimals -- no digits behind the dot?
                    then ""
                    else '.' : decimals
            _ -> ""
          numberStr = integerPart ++ decimalPart
          numParseResult = case readMaybe @Double numberStr of
            Just n -> validToken (NUMBER numberStr n) l
            Nothing -> syntaxError ("Attempted to parse an invalid number: " ++ numberStr) l ""
       in naiveScanTokens (drop (length numberStr) input) l (numParseResult : tt)
  -- reserved words and identifiers
  | isAlpha c =
      let identifier = c : takeWhile isAlphaNum ss
          tokenType = fromMaybe (IDENTIFIER identifier) (lookup identifier keywords)
       in naiveScanTokens (drop (length identifier) input) l (validToken tokenType l : tt)
-- If we reach here, it means we encountered an unexpected character
naiveScanTokens (_ : ss) line tt = naiveScanTokens ss line (syntaxError "Unexpected character." line "" : tt)
