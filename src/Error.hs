module Error (InterpreterError (..), handleErr) where

import GHC.IO.Handle.Text (hPutStrLn)
import Scanner.Error (SyntaxError, prettyPrintErr)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)

-- Error handling
data InterpreterError = Syntax [SyntaxError] | Parse String | Eval String deriving stock (Show)

handleErr :: InterpreterError -> IO ()
handleErr = \case
  Syntax errs -> do
    mapM_ (hPutStrLn stderr . prettyPrintErr) errs
    exitWith (ExitFailure 65)
  Parse err -> do
    hPutStrLn stderr err
    exitWith (ExitFailure 65)
  Eval err -> do
    hPutStrLn stderr err
    exitWith (ExitFailure 70)
