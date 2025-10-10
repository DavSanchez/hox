module Error
  ( InterpreterError (..),
    handleErr,
  )
where

import Evaluation (EvalError, displayEvalErr)
import GHC.IO.Handle.Text (hPutStrLn)
import Parser (ParseError, displayParseErr)
import Scanner.Error (Error, displayErr)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)

-- Error handling
data InterpreterError = Syntax [Error] | Parse [ParseError] | Eval EvalError deriving stock (Show)

handleErr :: InterpreterError -> IO ()
handleErr = \case
  Syntax errs -> do
    mapM_ (hPutStrLn stderr . displayErr) errs
    exitWith (ExitFailure 65)
  Parse err -> do
    mapM_ (hPutStrLn stderr . displayParseErr) err
    exitWith (ExitFailure 65)
  Eval err -> do
    hPutStrLn stderr (displayEvalErr err)
    exitWith (ExitFailure 70)
