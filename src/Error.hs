module Error (InterpreterError (..), handleErr) where

import Scanner.Error (SyntaxError, prettyPrintErr)
import System.Exit (ExitCode (ExitFailure), exitWith)

-- Error handling
data InterpreterError = Syntax [SyntaxError] | Parse String | Eval String deriving stock (Show)

handleErr :: InterpreterError -> IO ()
handleErr = \case
  Syntax errs -> do
    mapM_ (putStrLn . prettyPrintErr) errs
    exitWith (ExitFailure 65)
  Parse err -> do
    putStrLn err
    exitWith (ExitFailure 65)
  Eval err -> do
    putStrLn err
    exitWith (ExitFailure 70)
