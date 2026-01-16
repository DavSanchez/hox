module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Language.Scanner (TokenResult, scanTokens)
import Language.Syntax.Program (parseProgram)
import Language.Syntax.Token (Token)
import Runtime.Interpreter (buildTreeWalkInterpreter, runInterpreter)
import Test.Tasty.Bench

fibSrc :: Int -> String
fibSrc n =
  unlines
    [ "fun fib(n) {",
      "  if (n < 2) return n;",
      "  return fib(n - 1) + fib(n - 2);",
      "}",
      "var f = fib(" ++ show n ++ ");"
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
  let tokens n = extractTokens $ scanTokens (fibSrc n)
  -- Note: We are benchmarking the parsing of the *token list*, not the string.

  defaultMain
    [ bgroup
        "Scanner"
        -- Force evaluation by calculating length of the list
        [ bench "fib(20)" $ whnf (length . scanTokens) (fibSrc 20)
        -- additional tests should be done with bigger files
        ],
      bgroup
        "Parser"
        -- Force evaluation by checking if it's a Right
        [ bench "fib(20)" $ whnf (isRight . parseProgram) (tokens 20)
        -- additional tests should be done with bigger files
        ],
      bgroup
        "Interpreter"
        -- whnfIO is sufficient because the return value is simple key to success
        [ bench "fib(20)" $ whnfIO (runInterpreter (buildTreeWalkInterpreter (Right (tokens 20)))),
          bench "fib(25)" $ whnfIO (runInterpreter (buildTreeWalkInterpreter (Right (tokens 25)))),
          bench "fib(30)" $ whnfIO (runInterpreter (buildTreeWalkInterpreter (Right (tokens 30))))
        ]
    ]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
