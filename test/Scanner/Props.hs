module Scanner.Props (scannerProperties) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List.NonEmpty qualified as NE
import Language.Scanner (SyntaxError (..), scanTokens)
import Language.Syntax.Token (Token (tokenType), TokenType (EOF))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

scannerProperties :: TestTree
scannerProperties =
  testGroup
    "Scanner Property Tests"
    [ testProperty "Always ends in EOF or an unterminated string error" eofOrUnterminatedString,
      testProperty "Less or equal tokens than input length (plus EOF if present)" lessOrEqualTokensThanInputLength
    ]

eofOrUnterminatedString :: String -> Bool
eofOrUnterminatedString s =
  let lastToken = NE.last $ scanTokens s
      result = bimap errorMessage tokenType lastToken
   in result
        == if endsInsideString s
          then Left "Unterminated string."
          else Right EOF

-- | Whether the input ends inside a string literal, i.e. a string is never
-- closed.
--
-- A @//@ only starts a comment outside of a string: inside one it is just
-- text, so @"//"@ is a complete string. Stripping comments before counting
-- quotes would get that wrong. Strings may span lines, comments may not.
endsInsideString :: String -> Bool
endsInsideString = go False
  where
    go inString ('"' : ss) = go (not inString) ss
    go False ('/' : '/' : ss) = go False (dropWhile (/= '\n') ss)
    go inString (_ : ss) = go inString ss
    go inString [] = inString

lessOrEqualTokensThanInputLength :: String -> Bool
lessOrEqualTokensThanInputLength s =
  let tokens = scanTokens s
      hasEOF = fmap tokenType (NE.last tokens) == Right EOF
      tokenListLength = NE.length tokens
   in if hasEOF
        then tokenListLength <= length s + 1
        else tokenListLength <= length s -- The case with an unterminated string somewhere does not include EOF.
