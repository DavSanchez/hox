module Error (InterpreterError (..), handleErr) where

import Evaluation (EvalError, prettyPrintEvalErr)
import GHC.IO.Handle.Text (hPutStrLn)
import Parser (ParseError, prettyPrintParseErr)
import Scanner.Error (Error, prettyPrintErr)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)

-- Error handling
data InterpreterError = Syntax [Error] | Parse [ParseError] | Eval EvalError deriving stock (Show)

handleErr :: InterpreterError -> IO ()
handleErr = \case
  Syntax errs -> do
    mapM_ (hPutStrLn stderr . prettyPrintErr) errs
    exitWith (ExitFailure 65)
  Parse err -> do
    mapM_ (hPutStrLn stderr . prettyPrintParseErr) err
    exitWith (ExitFailure 65)
  Eval err -> do
    hPutStrLn stderr (prettyPrintEvalErr err)
    exitWith (ExitFailure 70)
