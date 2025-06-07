module Scanner.Internal where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Text.Read (readMaybe)
import Token (Token (Token), TokenType (..))

data SyntaxError = SyntaxError
  { errorMessage :: String,
    errorLine :: Int,
    errorWhere :: String
  }
  deriving (Show)

prettyPrint :: SyntaxError -> String
prettyPrint (SyntaxError msg line w) = "[line " <> show line <> "] Error" <> w <> ": " <> msg

type TokenResult = Either SyntaxError Token

-- >>> scanTokens "hello world"
-- [Right (Token {tokenType = IDENTIFIER "hello", line = 1}),Right (Token {tokenType = IDENTIFIER "world", line = 1}),Right (Token {tokenType = EOF, line = 1})]
-- >>> scanTokens "\"Hello World\""
-- [Right (Token {tokenType = STRING "\"Hello World\"" "Hello World", line = 1}),Right (Token {tokenType = EOF, line = 1})]
-- >>> scanTokens "hello\nworld"
-- [Right (Token {tokenType = IDENTIFIER "hello", line = 1}),Right (Token {tokenType = IDENTIFIER "world", line = 2}),Right (Token {tokenType = EOF, line = 2})]
scanTokens :: String -> [TokenResult]
scanTokens s = scanTokens' s 1 []

-- | Auxiliary function that scans the input strings and tracks the state: the current line number and the accumulated tokens
scanTokens' ::
  -- | Input string
  String ->
  -- | Current line number
  Int ->
  -- | Accumulated tokens
  [TokenResult] ->
  -- | Resulting list of tokens
  [TokenResult]
-- Base case: no more characters to process
scanTokens' "" l tt = reverse $ Right (Token EOF l) : tt
-- Single character tokens
scanTokens' ('(' : ss) l tt = scanTokens' ss l (Right (Token LEFT_PAREN l) : tt)
scanTokens' (')' : ss) l tt = scanTokens' ss l (Right (Token RIGHT_PAREN l) : tt)
scanTokens' ('{' : ss) l tt = scanTokens' ss l (Right (Token LEFT_BRACE l) : tt)
scanTokens' ('}' : ss) l tt = scanTokens' ss l (Right (Token RIGHT_BRACE l) : tt)
scanTokens' (',' : ss) l tt = scanTokens' ss l (Right (Token COMMA l) : tt)
scanTokens' ('.' : ss) l tt = scanTokens' ss l (Right (Token DOT l) : tt)
scanTokens' ('-' : ss) l tt = scanTokens' ss l (Right (Token MINUS l) : tt)
scanTokens' ('+' : ss) l tt = scanTokens' ss l (Right (Token PLUS l) : tt)
scanTokens' (';' : ss) l tt = scanTokens' ss l (Right (Token SEMICOLON l) : tt)
scanTokens' ('*' : ss) l tt = scanTokens' ss l (Right (Token STAR l) : tt)
-- Operators
scanTokens' ('!' : '=' : ss) l tt = scanTokens' ss l (Right (Token BANG_EQUAL l) : tt)
scanTokens' ('!' : ss) l tt = scanTokens' ss l (Right (Token BANG l) : tt)
scanTokens' ('=' : '=' : ss) l tt = scanTokens' ss l (Right (Token EQUAL_EQUAL l) : tt)
scanTokens' ('=' : ss) l tt = scanTokens' ss l (Right (Token EQUAL l) : tt)
scanTokens' ('>' : '=' : ss) l tt = scanTokens' ss l (Right (Token GREATER_EQUAL l) : tt)
scanTokens' ('>' : ss) l tt = scanTokens' ss l (Right (Token GREATER l) : tt)
scanTokens' ('<' : '=' : ss) l tt = scanTokens' ss l (Right (Token LESS_EQUAL l) : tt)
scanTokens' ('<' : ss) l tt = scanTokens' ss l (Right (Token LESS l) : tt)
-- Longer lexemes:
-- Comments
scanTokens' ('/' : '/' : ss) l tt =
  let -- Consume the rest of the line as a comment
      remainder = dropWhile (/= '\n') ss
   in -- Continue scanning from the next line
      scanTokens' remainder (l + 1) tt
-- Slash case (needs comment handling first, above)
scanTokens' ('/' : ss) l tt = scanTokens' ss l (Right (Token SLASH l) : tt)
-- Whitespaces
scanTokens' (' ' : ss) l tt = scanTokens' ss l tt
scanTokens' ('\r' : ss) l tt = scanTokens' ss l tt
scanTokens' ('\t' : ss) l tt = scanTokens' ss l tt
scanTokens' ('\n' : ss) l tt = scanTokens' ss (l + 1) tt -- Newline handling increments the line number
-- String literals
scanTokens' ('"' : ss) l tt =
  -- Looking for a starting quote
  let stringContent = takeWhile (/= '"') ss -- Take characters until the next quote
      newLinesInString = length $ filter (== '\n') stringContent
   in if stringContent == ss -- If there are no closing quotes, the calculated remainder is the same as the remaining input string
        then Left (SyntaxError "Unterminated string" l "") : tt
        else scanTokens' (drop (length stringContent + 1 {- For the closing quote -}) ss) (l + newLinesInString) (Right (Token (STRING ('"' : stringContent ++ ['"']) stringContent) l) : tt)
-- Number literals
scanTokens' (c : ss) l tt
  | isDigit c =
      let maybeNumberStr = ss
          integerPart = c : takeWhile isDigit maybeNumberStr
          decimalPart = case drop (length integerPart - 1) maybeNumberStr of
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
       in scanTokens' (drop (length numberStr - 1) ss) l (numParseResult : tt)
  -- reserved words and identifiers
  | isAlpha c =
      let identifier = c : takeWhile isAlphaNum ss
          tokenType = identifierOrKeyword identifier
       in scanTokens' (drop (length identifier - 1) ss) l (Right (Token tokenType l) : tt)
scanTokens' (c : ss) line tt =
  -- If we reach here, it means we encountered an unexpected character
  scanTokens' ss line (Left (SyntaxError ("Unexpected character: " ++ [c]) line "") : tt)

isAlpha :: Char -> Bool
isAlpha '_' = True
isAlpha c
  | isAsciiLower c || isAsciiUpper c =
      True
isAlpha _ = False

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

identifierOrKeyword :: String -> TokenType
identifierOrKeyword "and" = AND
identifierOrKeyword "class" = CLASS
identifierOrKeyword "else" = ELSE
identifierOrKeyword "false" = FALSE
identifierOrKeyword "fun" = FUN
identifierOrKeyword "for" = FOR
identifierOrKeyword "if" = IF
identifierOrKeyword "nil" = NIL
identifierOrKeyword "or" = OR
identifierOrKeyword "print" = PRINT
identifierOrKeyword "return" = RETURN
identifierOrKeyword "super" = SUPER
identifierOrKeyword "this" = THIS
identifierOrKeyword "true" = TRUE
identifierOrKeyword "var" = VAR
identifierOrKeyword "while" = WHILE
identifierOrKeyword ident = IDENTIFIER ident
