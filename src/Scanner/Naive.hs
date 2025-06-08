module Scanner.Naive (naiveScanTokens) where

import Data.Char (isDigit)
import Scanner.Internal (SyntaxError (..), TokenResult, identifierOrKeyword, isAlpha, isAlphaNum)
import Text.Read (readMaybe)
import Token (Token (Token), TokenType (..))

-- | Auxiliary function that scans the input strings and tracks the state: the current line number and the accumulated tokens
naiveScanTokens ::
  -- | Input string
  String ->
  -- | Current line number
  Int ->
  -- | Accumulated tokens
  [TokenResult] ->
  -- | Resulting list of tokens
  [TokenResult]
-- Base case: no more characters to process
naiveScanTokens "" l tt = reverse $ Right (Token EOF l) : tt
-- Single character tokens
naiveScanTokens ('(' : ss) l tt = naiveScanTokens ss l (Right (Token LEFT_PAREN l) : tt)
naiveScanTokens (')' : ss) l tt = naiveScanTokens ss l (Right (Token RIGHT_PAREN l) : tt)
naiveScanTokens ('{' : ss) l tt = naiveScanTokens ss l (Right (Token LEFT_BRACE l) : tt)
naiveScanTokens ('}' : ss) l tt = naiveScanTokens ss l (Right (Token RIGHT_BRACE l) : tt)
naiveScanTokens (',' : ss) l tt = naiveScanTokens ss l (Right (Token COMMA l) : tt)
naiveScanTokens ('.' : ss) l tt = naiveScanTokens ss l (Right (Token DOT l) : tt)
naiveScanTokens ('-' : ss) l tt = naiveScanTokens ss l (Right (Token MINUS l) : tt)
naiveScanTokens ('+' : ss) l tt = naiveScanTokens ss l (Right (Token PLUS l) : tt)
naiveScanTokens (';' : ss) l tt = naiveScanTokens ss l (Right (Token SEMICOLON l) : tt)
naiveScanTokens ('*' : ss) l tt = naiveScanTokens ss l (Right (Token STAR l) : tt)
-- Operators
naiveScanTokens ('!' : '=' : ss) l tt = naiveScanTokens ss l (Right (Token BANG_EQUAL l) : tt)
naiveScanTokens ('!' : ss) l tt = naiveScanTokens ss l (Right (Token BANG l) : tt)
naiveScanTokens ('=' : '=' : ss) l tt = naiveScanTokens ss l (Right (Token EQUAL_EQUAL l) : tt)
naiveScanTokens ('=' : ss) l tt = naiveScanTokens ss l (Right (Token EQUAL l) : tt)
naiveScanTokens ('>' : '=' : ss) l tt = naiveScanTokens ss l (Right (Token GREATER_EQUAL l) : tt)
naiveScanTokens ('>' : ss) l tt = naiveScanTokens ss l (Right (Token GREATER l) : tt)
naiveScanTokens ('<' : '=' : ss) l tt = naiveScanTokens ss l (Right (Token LESS_EQUAL l) : tt)
naiveScanTokens ('<' : ss) l tt = naiveScanTokens ss l (Right (Token LESS l) : tt)
-- Longer lexemes:
-- Comments
naiveScanTokens ('/' : '/' : ss) l tt =
  let -- Consume the rest of the line as a comment
      remainder = dropWhile (/= '\n') ss
   in -- Continue scanning from the next line
      naiveScanTokens remainder (l + 1) tt
-- Slash case (needs comment handling first, above)
naiveScanTokens ('/' : ss) l tt = naiveScanTokens ss l (Right (Token SLASH l) : tt)
-- Whitespaces
naiveScanTokens (' ' : ss) l tt = naiveScanTokens ss l tt
naiveScanTokens ('\r' : ss) l tt = naiveScanTokens ss l tt
naiveScanTokens ('\t' : ss) l tt = naiveScanTokens ss l tt
naiveScanTokens ('\n' : ss) l tt = naiveScanTokens ss (l + 1) tt -- Newline handling increments the line number
-- String literals
naiveScanTokens ('"' : ss) l tt =
  -- Looking for a starting quote
  let stringContent = takeWhile (/= '"') ss -- Take characters until the next quote
      newLinesInString = length $ filter (== '\n') stringContent
   in if stringContent == ss -- If there are no closing quotes, the calculated remainder is the same as the remaining input string
        then Left (SyntaxError "Unterminated string" l "") : tt
        else naiveScanTokens (drop (length stringContent + 1 {- For the closing quote -}) ss) (l + newLinesInString) (Right (Token (STRING ('"' : stringContent ++ ['"']) stringContent) l) : tt)
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
            Just n -> Right (Token (NUMBER numberStr n) l)
            Nothing -> Left (SyntaxError ("Attempted to parse an invalid number: " ++ numberStr) l "")
       in naiveScanTokens (drop (length numberStr) input) l (numParseResult : tt)
  -- reserved words and identifiers
  | isAlpha c =
      let identifier = c : takeWhile isAlphaNum ss
          tokenType = identifierOrKeyword identifier
       in naiveScanTokens (drop (length identifier) input) l (Right (Token tokenType l) : tt)
-- If we reach here, it means we encountered an unexpected character
naiveScanTokens (c : ss) line tt = naiveScanTokens ss line (Left (SyntaxError ("Unexpected character: " ++ [c]) line "") : tt)
