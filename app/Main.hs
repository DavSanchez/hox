module Main where

import Scanner (scanTokens)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, isEOF, stdout)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runPrompt
    [script] -> readFile script >>= print -- Just show the file for now
    _ -> do
      putStrLn "Usage: hox [script]"
      exitWith (ExitFailure 64)

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
