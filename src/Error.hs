module Error (InterpreterError (..), handleErr) where

import Evaluation (EvalError, prettyPrintEvalErr)
import GHC.IO.Handle.Text (hPutStrLn)
import Parser (ParseErrors, prettyPrintParseErr, toErrList)
import Scanner.Error (SyntaxError, prettyPrintErr)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)

-- Error handling
data InterpreterError = Syntax [SyntaxError] | Parse ParseErrors | Eval EvalError deriving stock (Show)

handleErr :: InterpreterError -> IO ()
handleErr = \case
  Syntax errs -> do
    mapM_ (hPutStrLn stderr . prettyPrintErr) errs
    exitWith (ExitFailure 65)
  Parse errs -> do
    mapM_ (hPutStrLn stderr . prettyPrintParseErr) (toErrList errs)
    exitWith (ExitFailure 65)
  Eval err -> do
    hPutStrLn stderr (prettyPrintEvalErr err)
    exitWith (ExitFailure 70)
