module Main (main) where

import Data.Either (lefts, rights)
import Data.List.NonEmpty (toList)
import Expression (Parser (runParser), expression, prettyPrintExpr)
import Scanner (prettyPrintErr, scanTokens)
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
      errors = lefts $ toList tokenResult
   in if null errors
        then
          let expr = fmap fst $ runParser expression $ rights $ toList tokenResult
           in case expr of
                Right e -> putStrLn $ prettyPrintExpr e
                Left err -> do
                  putStrLn "Parsing error:"
                  putStrLn err
                  exitWith (ExitFailure 65)
        else do
          putStrLn "Syntax errors found:"
          mapM_ (putStrLn . prettyPrintErr) errors
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
