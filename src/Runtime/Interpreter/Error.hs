module Runtime.Interpreter.Error
  ( InterpreterError (..),
    handleErr,
  )
where

import Language.Analysis.Resolver (ResolveError, displayResolveError)
import Language.Parser (ParseError, displayParseErr)
import Language.Scanner.Error (SyntaxError, displayErr)
import Runtime.Error (EvalError, displayEvalErr)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

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
    hPutStrLn stderr (displayResolveError err)
    exitWith (ExitFailure 65)
  Eval err -> do
    hPutStrLn stderr (displayEvalErr err)
    exitWith (ExitFailure 70)
