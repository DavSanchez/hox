module Language.Scanner
  ( scanTokens,
    displaySyntaxError,
    SyntaxError (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Language.Scanner.Error (SyntaxError (..), displaySyntaxError)
import Language.Scanner.Internal (TokenResult)
import Language.Scanner.Naive (naiveScanTokens)

-- | Scans the input string and returns a list of tokens.
-- >>> scanTokens "hello world"
-- Right (Token {tokenType = IDENTIFIER "hello", line = 1}) :| [Right (Token {tokenType = IDENTIFIER "world", line = 1}),Right (Token {tokenType = EOF, line = 1})]
-- >>> scanTokens "\"Hello World\""
-- Right (Token {tokenType = STRING "\"Hello World\"" "Hello World", line = 1}) :| [Right (Token {tokenType = EOF, line = 1})]
-- >>> scanTokens "hello\nworld"
-- Right (Token {tokenType = IDENTIFIER "hello", line = 1}) :| [Right (Token {tokenType = IDENTIFIER "world", line = 2}),Right (Token {tokenType = EOF, line = 2})]
scanTokens :: String -> NonEmpty TokenResult
scanTokens s = NE.reverse $ naiveScanTokens s 1 []
