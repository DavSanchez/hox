module Main where

import Data.Either (lefts, rights)
import Scanner (scanTokens)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, isEOF, readFile', stdout)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runPrompt
    [script] -> readFile' script >>= runScript
    _ -> do
      putStrLn "Usage: hox [script]"
      exitWith (ExitFailure 64)

runScript :: String -> IO ()
runScript script =
  let tokenResult = scanTokens script
      errors = lefts tokenResult
   in if null errors
        then mapM_ print (rights tokenResult)
        else do
          putStrLn "Syntax errors found:"
          mapM_ print errors
          exitWith (ExitFailure 65)

runPrompt :: IO ()
runPrompt = do
  putStr "> "
  hFlush stdout -- Make sure the prompt is printed before reading input
  eof <- isEOF -- Was EOF entered?
  if eof
    then putStrLn "Goodbye!"
    else do
      line <- getLine
      print $ scanTokens line
      runPrompt
