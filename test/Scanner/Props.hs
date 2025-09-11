module Scanner.Props (scannerProperties) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List.NonEmpty qualified as NE
import Scanner (errorMessage, scanTokens)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Token (Token (tokenType), TokenType (EOF))

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
      numDoubleQuotes = length $ filter (== '"') $ removeComments s
      result = bimap errorMessage tokenType lastToken
   in result
        == if even numDoubleQuotes -- Strings are closed?
          then Right EOF
          else Left "Unterminated string"

removeComments :: String -> String
removeComments ('/' : '/' : ss) = removeComments $ drop 1 $ dropWhile (/= '\n') ss
removeComments (s : ss) = s : removeComments ss
removeComments [] = []

lessOrEqualTokensThanInputLength :: String -> Bool
lessOrEqualTokensThanInputLength s =
  let tokens = scanTokens s
      hasEOF = fmap tokenType (NE.last tokens) == Right EOF
      tokenListLength = NE.length tokens
   in if hasEOF
        then tokenListLength <= length s + 1
        else tokenListLength <= length s -- The case with an unterminated string somewhere does not include EOF.
