module Main (main) where

import Control.Monad ((>=>))
import Data.List (singleton)
import Error (InterpreterError (..), handleErr)
import Expression (Expression, displayExpr, expression)
import Interpreter (Interpreter, buildTreeWalkInterpreter, evaluateExpr, runInterpreter)
import Parser (runParser)
import Scanner (scanTokens)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, hPutStrLn, isEOF, readFile', stderr, stdout)
import Token (Token, displayToken)
import Value (Value, displayValue)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runPrompt
    ["--chap04_scanning", script] -> readFile' script >>= handleChap04Out . runChapter04
    ["--chap06_parsing", script] -> readFile' script >>= handleChap06Out . (runChapter04 >=> runChapter06)
    ["--chap07_evaluating", script] ->
      readFile' script >>= \src ->
        case runChapter04 src >>= runChapter06 of
          Left err -> handleErr err
          Right expr -> runChapter07 expr >>= handleChap07Out
    ["--chap08_statements", script] -> readFile' script >>= handleChap08Out . runChapter08 . runChapter04
    ["--chap09_control", script] -> readFile' script >>= treeWalkInterpreter
    [script] -> readFile' script >>= treeWalkInterpreter
    _ -> do
      hPutStrLn stderr "Usage: hox [[--<CHAP>] script]"
      exitWith (ExitFailure 64)

runPrompt :: IO ()
runPrompt = do
  putStr "> "
  hFlush stdout -- Make sure the prompt is printed before reading input
  eof <- isEOF -- Was EOF entered?
  if eof
    then putStrLn "Goodbye!"
    else getLine >>= treeWalkInterpreter >> runPrompt

-- Chapter 04 operations
runChapter04 :: String -> Either InterpreterError [Token]
runChapter04 = scanTokens

handleChap04Out :: Either InterpreterError [Token] -> IO ()
handleChap04Out = either handleErr (mapM_ (putStrLn . displayToken))

-- Chapter 06 operations
runChapter06 :: [Token] -> Either InterpreterError Expression
runChapter06 s =
  let (result, _) = runParser expression s
   in case result of
        Left parseErr -> Left (Parse $ singleton parseErr)
        Right expr -> Right expr

handleChap06Out :: Either InterpreterError Expression -> IO ()
handleChap06Out = either handleErr (putStrLn . displayExpr)

-- Chapter 07 operations
runChapter07 :: Expression -> IO (Either InterpreterError Value)
runChapter07 = runInterpreter . evaluateExpr

handleChap07Out :: Either InterpreterError Value -> IO ()
handleChap07Out = either handleErr (putStrLn . displayValue)

-- Chapter 08+ operations
-- The previous chapters where "but a hack". Now we have the real deal!
runChapter08 :: Either InterpreterError [Token] -> Interpreter ()
runChapter08 = buildTreeWalkInterpreter

handleChap08Out :: Interpreter () -> IO ()
handleChap08Out = run

-- Actual functions that will run from now on
treeWalkInterpreter :: String -> IO ()
treeWalkInterpreter = run . buildTreeWalkInterpreter . scanTokens

run :: Interpreter () -> IO ()
run =
  runInterpreter >=> \case
    Left err -> handleErr err
    Right v -> pure v
