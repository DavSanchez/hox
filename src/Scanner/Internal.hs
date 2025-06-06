module Scanner.Internal where

import Data.Bifunctor (Bifunctor (second))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Text.Read (readMaybe)
import Token (Token (Token), TokenType (..))

data SyntaxError = SyntaxError
  { errorMessage :: String,
    errorLine :: Int
    -- errorColumn :: Int
  }
  deriving (Show)

type TokenResult = Either SyntaxError Token

type IndexedSource = [(Int, Char)]

-- >>> scanTokens "hello world"
-- [Right (Token {tokenType = Identifier "hello", line = 1}),Right (Token {tokenType = Identifier "world", line = 1})]
-- >>> scanTokens "\"Hello World\""
-- [Right (Token {tokenType = StringToken "Hello World", line = 1})]
scanTokens :: String -> [TokenResult]
scanTokens s = scanTokens' [] (charsWithLines s) ++ [Right (Token EOF (length . lines $ s))]

-- >>> charsWithLines "Hello\nWorld"
-- [(1,'H'),(1,'e'),(1,'l'),(1,'l'),(1,'o'),(1,'\n'),(2,'W'),(2,'o'),(2,'r'),(2,'l'),(2,'d'),(2,'\n')]
charsWithLines :: String -> IndexedSource
charsWithLines s =
  let -- Index the lines of the input string, to track what line each token is on.
      -- This creates a [(Int, String)] where the Int is the line number and the String is the line.
      indexedLines = second (++ ['\n']) <$> zip [1 :: Int ..] (lines s)
   in concatMap (\(lineNum, line) -> map (lineNum,) line) indexedLines

scanTokens' :: [TokenResult] -> IndexedSource -> [TokenResult]
scanTokens' tokenList [] = tokenList -- Base case: no more characters to process
-- single character tokens
scanTokens' tokenList ((line, '(') : ss) = scanTokens' (tokenList ++ [Right (Token LeftParen line)]) ss
scanTokens' tokenList ((line, ')') : ss) = scanTokens' (tokenList ++ [Right (Token RightParen line)]) ss
scanTokens' tokenList ((line, '{') : ss) = scanTokens' (tokenList ++ [Right (Token LeftBrace line)]) ss
scanTokens' tokenList ((line, '}') : ss) = scanTokens' (tokenList ++ [Right (Token RightBrace line)]) ss
scanTokens' tokenList ((line, ',') : ss) = scanTokens' (tokenList ++ [Right (Token Comma line)]) ss
scanTokens' tokenList ((line, '.') : ss) = scanTokens' (tokenList ++ [Right (Token Dot line)]) ss
scanTokens' tokenList ((line, '-') : ss) = scanTokens' (tokenList ++ [Right (Token Minus line)]) ss
scanTokens' tokenList ((line, '+') : ss) = scanTokens' (tokenList ++ [Right (Token Plus line)]) ss
scanTokens' tokenList ((line, ';') : ss) = scanTokens' (tokenList ++ [Right (Token Semicolon line)]) ss
scanTokens' tokenList ((line, '*') : ss) = scanTokens' (tokenList ++ [Right (Token Star line)]) ss
-- operators
scanTokens' tokenList ((line, '!') : (_, '=') : ss) =
  scanTokens' (tokenList ++ [Right (Token BangEqual line)]) ss
scanTokens' tokenList ((line, '!') : ss) = scanTokens' (tokenList ++ [Right (Token Bang line)]) ss
scanTokens' tokenList ((line, '=') : (_, '=') : ss) =
  scanTokens' (tokenList ++ [Right (Token EqualEqual line)]) ss
scanTokens' tokenList ((line, '=') : ss) = scanTokens' (tokenList ++ [Right (Token Equal line)]) ss
scanTokens' tokenList ((line, '>') : (_, '=') : ss) =
  scanTokens' (tokenList ++ [Right (Token GreaterEqual line)]) ss
scanTokens' tokenList ((line, '>') : ss) = scanTokens' (tokenList ++ [Right (Token Greater line)]) ss
scanTokens' tokenList ((line, '<') : (_, '=') : ss) =
  scanTokens' (tokenList ++ [Right (Token LessEqual line)]) ss
scanTokens' tokenList ((line, '<') : ss) = scanTokens' (tokenList ++ [Right (Token Less line)]) ss
-- longer lexemes
-- comments
scanTokens' tokenList ((_, '/') : (_, '/') : ss) =
  let -- Consume the rest of the line as a comment
      remainder = dropWhile ((/= '\n') . snd) ss
   in scanTokens' tokenList remainder
-- slash case
scanTokens' tokenList ((line, '/') : ss) = scanTokens' (tokenList ++ [Right (Token Slash line)]) ss
-- whitespaces
scanTokens' tokenList ((_, ' ') : ss) = scanTokens' tokenList ss
scanTokens' tokenList ((_, '\r') : ss) = scanTokens' tokenList ss
scanTokens' tokenList ((_, '\t') : ss) = scanTokens' tokenList ss
scanTokens' tokenList ((_, '\n') : ss) = scanTokens' tokenList ss
-- string literals
scanTokens' tokenList ((line, '"') : ss) =
  let stringContent = takeWhile ((/= '"') . snd) ss
   in if stringContent == ss
        then tokenList ++ [Left (SyntaxError "Unterminated string" line)]
        else scanTokens' (tokenList ++ [Right (Token (StringToken (snd <$> stringContent)) line)]) (drop (length stringContent + 1 {- For the closing quote -}) ss)
-- number literals
scanTokens' tokenList ((line, c) : ss)
  | isDigit c =
      let maybeNumberStr = snd <$> ss
          integerPart = c : takeWhile isDigit maybeNumberStr
          decimalPart = case drop (length integerPart) maybeNumberStr of
            ('.' : rest) -> '.' : takeWhile isDigit rest
            _ -> []
          numberStr = integerPart ++ decimalPart
          numParseResult = case readMaybe @Double numberStr of
            Just n -> Right (Token (NumberToken n) line)
            Nothing -> Left (SyntaxError ("Attempted to parse an invalid number: " ++ numberStr) line)
       in scanTokens' (tokenList ++ [numParseResult]) (drop (length numberStr) ss)
  -- reserved words and identifiers
  | isAlpha c =
      let identifier = c : takeWhile isAlphaNum (snd <$> ss)
          tokenType = identifierOrKeyword identifier
       in scanTokens' (tokenList ++ [Right (Token tokenType line)]) (drop (length identifier) ss)
scanTokens' tokenList ((line, c) : ss) =
  -- If we reach here, it means we encountered an unexpected character
  scanTokens' (tokenList ++ [Left (SyntaxError ("Unexpected character: " ++ [c]) line)]) ss

isAlpha :: Char -> Bool
isAlpha '_' = True
isAlpha c
  | isAsciiLower c || isAsciiUpper c =
      True
isAlpha _ = False

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

identifierOrKeyword :: String -> TokenType
identifierOrKeyword "and" = And
identifierOrKeyword "class" = Class
identifierOrKeyword "else" = Else
identifierOrKeyword "false" = FalseK
identifierOrKeyword "fun" = Fun
identifierOrKeyword "for" = For
identifierOrKeyword "if" = If
identifierOrKeyword "nil" = Nil
identifierOrKeyword "or" = Or
identifierOrKeyword "print" = Print
identifierOrKeyword "return" = Return
identifierOrKeyword "super" = Super
identifierOrKeyword "this" = This
identifierOrKeyword "true" = TrueK
identifierOrKeyword "var" = Var
identifierOrKeyword "while" = While
identifierOrKeyword ident = Identifier ident
