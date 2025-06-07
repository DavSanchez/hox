module Scanner (scanTokens, prettyPrint) where

import Scanner.Internal (TokenResult, prettyPrint)
import Scanner.Naive (naiveScanTokens)

-- >>> scanTokens "hello world"
-- [Right (Token {tokenType = IDENTIFIER "hello", line = 1}),Right (Token {tokenType = IDENTIFIER "world", line = 1}),Right (Token {tokenType = EOF, line = 1})]
-- >>> scanTokens "\"Hello World\""
-- [Right (Token {tokenType = STRING "\"Hello World\"" "Hello World", line = 1}),Right (Token {tokenType = EOF, line = 1})]
-- >>> scanTokens "hello\nworld"
-- [Right (Token {tokenType = IDENTIFIER "hello", line = 1}),Right (Token {tokenType = IDENTIFIER "world", line = 2}),Right (Token {tokenType = EOF, line = 2})]
scanTokens :: String -> [TokenResult]
scanTokens s = naiveScanTokens s 1 []
