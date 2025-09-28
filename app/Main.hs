module Main (main) where

import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Either (lefts, rights)
import Data.List (singleton)
import Data.List.NonEmpty (toList)
import Error (InterpreterError (..), handleErr)
import Evaluation (Value, evalExpr, printValue)
import Expression (Expression, expression, prettyPrintExpr)
import Grammar (Program, evaluate, parseProgram)
import Parser (runParser)
import Scanner (scanTokens)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, hPutStrLn, isEOF, readFile', stderr, stdout)
import Token (Token, prettyPrintToken)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runPrompt
    ["--chap04_scanning", script] -> readFile' script >>= handleChap04Out . runChapter04
    ["--chap06_parsing", script] -> readFile' script >>= handleChap06Out . (runChapter04 >=> runChapter06)
    ["--chap07_evaluating", script] -> readFile' script >>= handleChap07Out . (runChapter04 >=> runChapter06 >=> runChapter07)
    ["--chap08_statements", script] -> readFile' script >>= handleChap08Out . (runChapter04 >=> runChapter08)
    [script] -> readFile' script >>= currentImpl
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
    else getLine >>= currentImpl >> runPrompt

currentImpl :: String -> IO ()
currentImpl = handleChap08Out . (runChapter04 >=> runChapter08)

-- Chapter 04 operations
runChapter04 :: String -> Either InterpreterError [Token]
runChapter04 script =
  let tokenResult = (toList . scanTokens) script
      errors = lefts tokenResult
   in if null errors
        then Right $ rights tokenResult
        else Left $ Syntax errors

handleChap04Out :: Either InterpreterError [Token] -> IO ()
handleChap04Out = either handleErr (mapM_ (putStrLn . prettyPrintToken))

-- Chapter 06 operations
runChapter06 :: [Token] -> Either InterpreterError Expression
runChapter06 = bimap (Parse . singleton) fst . runParser expression

handleChap06Out :: Either InterpreterError Expression -> IO ()
handleChap06Out = either handleErr (putStrLn . prettyPrintExpr)

-- Chapter 07 operations
runChapter07 :: Expression -> Either InterpreterError Value
runChapter07 = first Eval . evalExpr

handleChap07Out :: Either InterpreterError Value -> IO ()
handleChap07Out = either handleErr (putStrLn . printValue)

-- Chapter 08 operations
-- The previous chapters where "but a hack". Now we have the real deal!
runChapter08 :: [Token] -> Either InterpreterError Program
runChapter08 = first Parse . parseProgram

handleChap08Out :: Either InterpreterError Program -> IO ()
handleChap08Out = either handleErr evaluate
