module Main (main) where

import Data.Either (rights)
import Data.List.NonEmpty (toList)
import Language.Scanner (scanTokens)
import Runtime.Interpreter (buildTreeWalkInterpreter, runInterpreter)
import Test.Tasty.Bench (bench, bgroup, defaultMain, whnfIO)

fibSrc :: Int -> String
fibSrc n =
  unlines
    [ "fun fib(n) {",
      "  if (n < 2) return n;",
      "  return fib(n - 1) + fib(n - 2);",
      "}",
      "var f = fib(" ++ show n ++ ");"
    ]

main :: IO ()
main = do
  -- Pre-compute values to avoid benchmarking setup costs in the loops
  let tokens n = (rights . toList . scanTokens) (fibSrc n)
  -- Note: We are benchmarking the parsing of the *token list*, not the string.

  defaultMain
    [ bgroup
        "Scanner"
        -- Force evaluation by calculating length of the list
        -- Example: `bench "fib(20)" $ whnf (length . scanTokens) (fibSrc 20)` -- too small
        -- additional tests should be done with bigger files
        [],
      bgroup
        "Parser"
        -- Force evaluation by checking if it's a Right
        -- Example: `bench "fib(20)" $ whnf (isRight . parseProgram) (tokens 20)` -- too small
        -- additional tests should be done with bigger files
        [],
      bgroup
        "Interpreter"
        -- whnfIO is sufficient because the return value is simple key to success
        [ bench "fib(20)" $ whnfIO $ runInterpreter $ buildTreeWalkInterpreter $ Right $ tokens 20,
          bench "fib(25)" $ whnfIO $ runInterpreter $ buildTreeWalkInterpreter $ Right $ tokens 25,
          bench "fib(30)" $ whnfIO $ runInterpreter $ buildTreeWalkInterpreter $ Right $ tokens 30
        ]
    ]
