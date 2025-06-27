module Scanner (scanTokens, prettyPrintErr, SyntaxError (..)) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Scanner.Error (SyntaxError (..), prettyPrintErr)
import Scanner.Internal (TokenResult)
import Scanner.Naive (naiveScanTokens)

-- The version with parser combinators via the megaparsec library.
-- I could have used std's `ReadP`, but I wanted to try the powerful one.
-- import Scanner.ParserCombinators (megaparsecScanTokens)

-- | Scans the input string and returns a list of tokens.
-- >>> scanTokens "hello world"
-- Right (Token {tokenType = IDENTIFIER "hello", line = 1}) :| [Right (Token {tokenType = IDENTIFIER "world", line = 1}),Right (Token {tokenType = EOF, line = 1})]
-- >>> scanTokens "\"Hello World\""
-- Right (Token {tokenType = STRING "\"Hello World\"" "Hello World", line = 1}) :| [Right (Token {tokenType = EOF, line = 1})]
-- >>> scanTokens "hello\nworld"
-- Right (Token {tokenType = IDENTIFIER "hello", line = 1}) :| [Right (Token {tokenType = IDENTIFIER "world", line = 2}),Right (Token {tokenType = EOF, line = 2})]
scanTokens :: String -> NonEmpty TokenResult
scanTokens s = NE.reverse $ naiveScanTokens s 1 []

-- scanTokens = megaparsecScanTokens
