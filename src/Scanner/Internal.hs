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
scanTokens s = reverse $ Right (Token EOF (length . lines $ s)) : scanTokens' [] 1 s

scanTokens' :: [TokenResult] -> Int -> String -> [TokenResult]
scanTokens' tokenList _ [] = tokenList -- Base case: no more characters to process
-- single character tokens
scanTokens' tokenList line ('(' : ss) = scanTokens' (Right (Token LEFT_PAREN line) : tokenList) line ss
scanTokens' tokenList line (')' : ss) = scanTokens' (Right (Token RIGHT_PAREN line) : tokenList) line ss
scanTokens' tokenList line ('{' : ss) = scanTokens' (Right (Token LEFT_BRACE line) : tokenList) line ss
scanTokens' tokenList line ('}' : ss) = scanTokens' (Right (Token RIGHT_BRACE line) : tokenList) line ss
scanTokens' tokenList line (',' : ss) = scanTokens' (Right (Token COMMA line) : tokenList) line ss
scanTokens' tokenList line ('.' : ss) = scanTokens' (Right (Token DOT line) : tokenList) line ss
scanTokens' tokenList line ('-' : ss) = scanTokens' (Right (Token MINUS line) : tokenList) line ss
scanTokens' tokenList line ('+' : ss) = scanTokens' (Right (Token PLUS line) : tokenList) line ss
scanTokens' tokenList line (';' : ss) = scanTokens' (Right (Token SEMICOLON line) : tokenList) line ss
scanTokens' tokenList line ('*' : ss) = scanTokens' (Right (Token STAR line) : tokenList) line ss
-- operators
scanTokens' tokenList line ('!' : '=' : ss) = scanTokens' (Right (Token BANG_EQUAL line) : tokenList) line ss
scanTokens' tokenList line ('!' : ss) = scanTokens' (Right (Token BANG line) : tokenList) line ss
scanTokens' tokenList line ('=' : '=' : ss) = scanTokens' (Right (Token EQUAL_EQUAL line) : tokenList) line ss
scanTokens' tokenList line ('=' : ss) = scanTokens' (Right (Token EQUAL line) : tokenList) line ss
scanTokens' tokenList line ('>' : '=' : ss) = scanTokens' (Right (Token GREATER_EQUAL line) : tokenList) line ss
scanTokens' tokenList line ('>' : ss) = scanTokens' (Right (Token GREATER line) : tokenList) line ss
scanTokens' tokenList line ('<' : '=' : ss) = scanTokens' (Right (Token LESS_EQUAL line) : tokenList) line ss
scanTokens' tokenList line ('<' : ss) = scanTokens' (Right (Token LESS line) : tokenList) line ss
-- longer lexemes
-- comments
scanTokens' tokenList line ('/' : '/' : ss) =
  let -- Consume the rest of the line as a comment
      remainder = dropWhile (/= '\n') ss
   in -- Continue scanning from the next line
      scanTokens' tokenList (line + 1) remainder
-- slash case
scanTokens' tokenList line ('/' : ss) = scanTokens' (Right (Token SLASH line) : tokenList) line ss
-- whitespaces
scanTokens' tokenList line (' ' : ss) = scanTokens' tokenList line ss
scanTokens' tokenList line ('\r' : ss) = scanTokens' tokenList line ss
scanTokens' tokenList line ('\t' : ss) = scanTokens' tokenList line ss
scanTokens' tokenList line ('\n' : ss) = scanTokens' tokenList (line + 1) ss
-- string literals
scanTokens' tokenList line ('"' : ss) =
  let stringContent = takeWhile (/= '"') ss
      newLinesInString = length $ filter (== '\n') stringContent
   in if stringContent == ss
        then Left (SyntaxError "Unterminated string" line "") : tokenList
        else scanTokens' (Right (Token (STRING ('"' : stringContent ++ ['"']) stringContent) line) : tokenList) (line + newLinesInString) (drop (length stringContent + 1 {- For the closing quote -}) ss)
-- number literals
scanTokens' tokenList line (c : ss)
  | isDigit c =
      let maybeNumberStr = ss
          integerPart = c : takeWhile isDigit maybeNumberStr
          decimalPart = case drop (length integerPart - 1) maybeNumberStr of
            ('.' : rest) ->
              let decimals = takeWhile isDigit rest
               in if null decimals
                    then []
                    else '.' : decimals
            _ -> []
          numberStr = integerPart ++ decimalPart
          numParseResult = case readMaybe @Double numberStr of
            Just n -> Right (Token (NUMBER numberStr n) line)
            Nothing -> Left (SyntaxError ("Attempted to parse an invalid number: " ++ numberStr) line "")
       in scanTokens' (numParseResult : tokenList) line (drop (length numberStr - 1) ss)
  -- reserved words and identifiers
  | isAlpha c =
      let identifier = c : takeWhile isAlphaNum ss
          tokenType = identifierOrKeyword identifier
       in scanTokens' (Right (Token tokenType line) : tokenList) line (drop (length identifier - 1) ss)
scanTokens' tokenList line (c : ss) =
  -- If we reach here, it means we encountered an unexpected character
  scanTokens' (Left (SyntaxError ("Unexpected character: " ++ [c]) line "") : tokenList) line ss

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
