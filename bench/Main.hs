module Main (main) where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import Data.Either (rights)
import Data.List.NonEmpty (toList)
import Language.Analysis.Resolver (programResolver, runResolver)
import Language.Scanner (scanTokens)
import Language.Syntax.Expression (Phase (Unresolved))
import Language.Syntax.Program (Program (..), parseProgram)
import Language.Syntax.Token (Token)
import Runtime.Interpreter (buildTreeWalkInterpreter, runInterpreter)
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, env, whnf, whnfIO)
import Workloads qualified as W

-- Run with `+RTS -T` (set by default in the cabal stanza) to also get the
-- allocated / copied / peak memory columns. See bench/README.md.

tokensOf :: String -> [Token]
tokensOf = rights . toList . scanTokens

-- | Runs a program to completion, failing the benchmark if the interpreter
-- reports any error. Otherwise a broken workload would silently be measured
-- as a (very fast) failure.
runLox :: [Token] -> IO ()
runLox tokens =
  runInterpreter (buildTreeWalkInterpreter (Right tokens)) >>= either (fail . show) pure

-- | Benchmark a workload end-to-end (parse + resolve + interpret). The token
-- list is built once, outside of the measured section.
interpreted :: String -> String -> Benchmark
interpreted name src =
  env (evaluate (forceList (tokensOf src))) $ \(Shared tokens) ->
    bench name $ whnfIO (runLox tokens)

-- | The AST and tokens have no 'NFData' instances, and 'env' requires one.
-- The setup actions below already force what matters (the list spines), so
-- WHNF is all that is left to do.
newtype Shared a = Shared a

instance NFData (Shared a) where
  rnf (Shared a) = a `seq` ()

forceList :: [a] -> Shared [a]
forceList xs = length xs `seq` Shared xs

main :: IO ()
main =
  defaultMain
    [ -- Front-end stages, each on the same large program, fed with the output
      -- of the previous stage (built once, outside the measured section).
      bgroup
        "Scanner"
        [ bench "large program" $ whnf (length . scanTokens) W.frontEndSource
        ],
      bgroup
        "Parser"
        [ env (evaluate (forceList (tokensOf W.frontEndSource))) $ \(Shared tokens) ->
            bench "large program" $ whnf (either (const (-1)) (\(Program ds) -> length ds) . parseProgram) tokens
        ],
      bgroup
        "Resolver"
        [ env (parsed W.frontEndSource) $ \(Shared prog) ->
            bench "large program" $ whnf (\p -> let (Program ds, errs) = runResolver (programResolver p) in length errs + length ds) prog
        ],
      -- Back-end. These names (the fib ones in particular) are stable so the
      -- history tracked in CI stays comparable.
      bgroup
        "Interpreter"
        [ interpreted "fib(20)" (W.fib 20),
          interpreted "fib(25)" (W.fib 25),
          interpreted "fib(30)" (W.fib 30),
          interpreted "loop global (100k)" (W.loopGlobal 100_000),
          interpreted "loop local (100k)" (W.loopLocal 100_000),
          interpreted "closures (50k calls)" (W.closures 50_000),
          interpreted "classes (10k iterations)" (W.classes 10_000),
          interpreted "strings (2k concats)" (W.strings 2_000),
          interpreted "blocks (20k iterations)" (W.blocks 20_000),
          interpreted "deep recursion (5k)" (W.deepRecursion 5_000)
        ]
    ]
  where
    parsed :: String -> IO (Shared (Program 'Unresolved))
    parsed src = case parseProgram (tokensOf src) of
      Right p@(Program ds) -> evaluate (length ds `seq` Shared p)
      Left errs -> fail ("workload does not parse: " ++ show errs)
