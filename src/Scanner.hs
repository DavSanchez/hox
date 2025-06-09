module Scanner (scanTokens, prettyPrint) where

import Scanner.Internal (TokenResult, prettyPrint)
import Scanner.Naive (naiveScanTokens)

-- The version with parser combinators via the megaparsec library.
-- I could have used std's `ReadP`, but I wanted to try the powerful one.
-- import Scanner.ParserCombinators (megaparsecScanTokens)

-- >>> scanTokens "hello world"
-- [Right (Token {tokenType = IDENTIFIER "hello", line = 1}),Right (Token {tokenType = IDENTIFIER "world", line = 1}),Right (Token {tokenType = EOF, line = 1})]
-- >>> scanTokens "\"Hello World\""
-- [Right (Token {tokenType = STRING "\"Hello World\"" "Hello World", line = 1}),Right (Token {tokenType = EOF, line = 1})]
-- >>> scanTokens "hello\nworld"
-- [Right (Token {tokenType = IDENTIFIER "hello", line = 1}),Right (Token {tokenType = IDENTIFIER "world", line = 2}),Right (Token {tokenType = EOF, line = 2})]
scanTokens :: String -> [TokenResult]
scanTokens s = naiveScanTokens s 1 []

-- scanTokens = megaparsecScanTokens
