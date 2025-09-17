module Main (main) where

import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Either (lefts, rights)
import Data.List.NonEmpty (toList)
import Evaluation (Value, evalExpr, printValue)
import Expression (Expression, expression, prettyPrintExpr)
import Grammar (Program, program)
import Parser (runParser)
import Scanner (SyntaxError, prettyPrintErr, scanTokens)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, isEOF, readFile', stdout)
import Token (Token, prettyPrintToken)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runPrompt
    ["--chap04_scanning", script] -> readFile' script >>= handleChap04Out . runChapter04
    ["--chap06_parsing", script] -> readFile' script >>= handleChap06Out . (runChapter04 >=> runChapter06)
    ["--chap07_evaluating", script] -> readFile' script >>= handleChap07Out . (runChapter04 >=> runChapter06 >=> runChapter07)
    ["--chap08_statements", script] -> readFile' script >>= handleChap08Out . currentImpl -- TODO same as chapter 08 for now
    [script] -> readFile' script >>= handleChap08Out . currentImpl
    _ -> do
      putStrLn "Usage: hox [[--<CHAP>] script]"
      exitWith (ExitFailure 64)
  where
    handleChap04Out = either handleErr (mapM_ (putStrLn . prettyPrintToken))
    handleChap06Out = either handleErr (putStrLn . prettyPrintExpr)
    handleChap07Out = either handleErr (putStrLn . printValue)
    handleChap08Out = either handleErr print -- TODO improve

currentImpl :: String -> Either InterpreterError Program
currentImpl = runChapter04 >=> runChapter08

-- Chapter 04 operations
runChapter04 :: String -> Either InterpreterError [Token]
runChapter04 script =
  let tokenResult = (toList . scanTokens) script
      errors = lefts tokenResult
   in if null errors
        then Right $ rights tokenResult
        else Left $ Syntax errors

-- Chapter 06 operations
runChapter06 :: [Token] -> Either InterpreterError Expression
runChapter06 = bimap Parse fst . runParser expression

-- Chapter 07 operations
runChapter07 :: Expression -> Either InterpreterError Value
runChapter07 = first Eval . evalExpr

-- Chapter08 operations
-- The previous chapters where "but a hack". Now we have the real deal
runChapter08 :: [Token] -> Either InterpreterError Program
runChapter08 = bimap Parse fst . runParser program

-- Error handling
data InterpreterError = Syntax [SyntaxError] | Parse String | Eval String

handleErr :: InterpreterError -> IO a
handleErr = \case
  Syntax errs -> do
    putStrLn "Syntax errors found:"
    mapM_ (putStrLn . prettyPrintErr) errs
    exitWith (ExitFailure 65)
  Parse err -> do
    putStrLn "Parsing error:"
    putStrLn err
    exitWith (ExitFailure 65)
  Eval err -> do
    putStrLn "Evaluation error:"
    putStrLn err
    exitWith (ExitFailure 70)

-- TODO update
runPrompt :: IO ()
runPrompt = do
  putStr "> "
  hFlush stdout -- Make sure the prompt is printed before reading input
  eof <- isEOF -- Was EOF entered?
  if eof
    then putStrLn "Goodbye!"
    else do
      line <- getLine
      print $ scanTokens line
      runPrompt
