module Runtime.Interpreter.Error (InterpreterError (..)) where

import Language.Analysis.Error (ResolveError)
import Language.Parser (ParseError)
import Language.Scanner.Error (SyntaxError)
import Runtime.Error (EvalError)

-- Error handling
data InterpreterError
  = Syntax [SyntaxError]
  | Parse [ParseError]
  | Resolve [ResolveError]
  | Eval EvalError
  deriving stock (Show, Eq)
