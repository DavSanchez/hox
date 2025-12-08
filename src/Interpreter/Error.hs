module Interpreter.Error
  ( InterpreterError (..),
    handleErr,
  )
where

import Evaluation.Error (EvalError, displayEvalErr)
import GHC.IO.Handle.Text (hPutStrLn)
import Parser (ParseError, displayParseErr)
import Resolver (ResolveError)
import Scanner.Error (SyntaxError, displayErr)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)

-- Error handling
data InterpreterError
  = Syntax [SyntaxError]
  | Parse [ParseError]
  | Resolve ResolveError
  | Eval EvalError
  deriving stock (Show, Eq)

handleErr :: InterpreterError -> IO ()
handleErr = \case
  Syntax errs -> do
    mapM_ (hPutStrLn stderr . displayErr) errs
    exitWith (ExitFailure 65)
  Parse err -> do
    mapM_ (hPutStrLn stderr . displayParseErr) err
    exitWith (ExitFailure 65)
  Resolve err -> do
    hPutStrLn stderr (show err)
    exitWith (ExitFailure 65)
  Eval err -> do
    hPutStrLn stderr (displayEvalErr err)
    exitWith (ExitFailure 70)
