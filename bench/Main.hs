module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Language.Scanner (scanTokens)
import Language.Scanner.Internal (TokenResult)
import Language.Syntax.Program (parseProgram)
import Language.Syntax.Token (Token)
import Runtime.Interpreter (buildTreeWalkInterpreter, runInterpreter)
import Test.Tasty.Bench

fibSrc :: String
fibSrc =
  unlines
    [ "fun fib(n) {",
      "  if (n < 2) return n;",
      "  return fib(n - 1) + fib(n - 2);",
      "}",
      "var f = fib(20);"
    ]

-- Helper to extract tokens (assuming valid input for benchmark)
extractTokens :: NonEmpty TokenResult -> [Token]
extractTokens (r :| rs) = rights (r : rs)
  where
    rights [] = []
    rights (Right x : xs) = x : rights xs
    rights (Left _ : xs) = rights xs

main :: IO ()
main = do
  -- Pre-compute values to avoid benchmarking setup costs in the loops
  let tokens = extractTokens $ scanTokens fibSrc
  -- Note: We are benchmarking the parsing of the *token list*, not the string.

  defaultMain
    [ bgroup
        "Scanner"
        -- Force evaluation by calculating length of the list
        [ bench "fib(20)" $ whnf (length . scanTokens) fibSrc
        ],
      bgroup
        "Parser"
        -- Force evaluation by checking if it's a Right
        [ bench "fib(20)" $ whnf (isRight . parseProgram) tokens
        ],
      bgroup
        "Interpreter"
        -- whnfIO is sufficient because the return value is simple key to success
        [ bench "fib(20)" $ whnfIO (runInterpreter (buildTreeWalkInterpreter (Right tokens)))
        ]
    ]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
