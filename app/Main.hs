module Main (main) where

import Control.Monad ((>=>))
import Data.Either (partitionEithers)
import Data.List (singleton)
import Data.List.NonEmpty (toList)
import Language.Analysis.Resolver (displayResolveError)
import Language.Parser (displayParseErr, runParser)
import Language.Scanner (displayErr, scanTokens)
import Language.Syntax.Expression (Expression (..), Phase (..), Resolution (Global), displayExpr, expression)
import Language.Syntax.Token (Token, displayToken)
import Runtime.Error (displayEvalErr)
import Runtime.Interpreter
  ( Interpreter,
    InterpreterError (..),
    buildTreeWalkInterpreter,
    evaluateExpr,
    runInterpreter,
  )
import Runtime.Value (Value, displayValue)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, hPutStrLn, isEOF, readFile', stderr, stdout)

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
    -- complete implementation for the remaining chapters
    [flag, script]
      | flag
          `elem` [ "--chap09_control",
                   "--chap10_functions",
                   "--chap11_resolving",
                   "--chap12_classes"
                 ] ->
          readFile' script >>= treeWalkInterpreter
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
runChapter04 s =
  let (errs, toks) = partitionEithers $ toList $ scanTokens s
   in if null errs
        then Right toks
        else Left (Syntax errs)

handleChap04Out :: Either InterpreterError [Token] -> IO ()
handleChap04Out = either handleErr (mapM_ (putStrLn . displayToken))

-- Chapter 06 operations
runChapter06 :: [Token] -> Either InterpreterError (Expression 'Unresolved)
runChapter06 s =
  let (result, _) = runParser expression s
   in case result of
        Left parseErr -> Left (Parse $ singleton parseErr)
        Right expr -> Right expr

handleChap06Out :: Either InterpreterError (Expression 'Unresolved) -> IO ()
handleChap06Out = either handleErr (putStrLn . displayExpr)

-- Chapter 07 operations
runChapter07 :: Expression 'Unresolved -> IO (Either InterpreterError Value)
runChapter07 expr = runInterpreter $ evaluateExpr (resolveGlobal expr)
  where
    resolveGlobal :: Expression 'Unresolved -> Expression 'Resolved
    resolveGlobal (Literal l) = Literal l
    resolveGlobal (Logical line op left right) = Logical line op (resolveGlobal left) (resolveGlobal right)
    resolveGlobal (UnaryOperation line op expr') = UnaryOperation line op (resolveGlobal expr')
    resolveGlobal (BinaryOperation line op left right) = BinaryOperation line op (resolveGlobal left) (resolveGlobal right)
    resolveGlobal (Call line callee args) = Call line (resolveGlobal callee) (map resolveGlobal args)
    resolveGlobal (Get line obj name) = Get line (resolveGlobal obj) name
    resolveGlobal (Set line obj name val) = Set line (resolveGlobal obj) name (resolveGlobal val)
    resolveGlobal (This line _) = This line Global
    resolveGlobal (Grouping expr') = Grouping (resolveGlobal expr')
    resolveGlobal (VariableExpr line name _) = VariableExpr line name Global
    resolveGlobal (VariableAssignment line name val _) = VariableAssignment line name (resolveGlobal val) Global

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
treeWalkInterpreter src = do
  let (errs, toks) = partitionEithers $ toList $ scanTokens src
  mapM_ (hPutStrLn stderr . displayErr) errs
  run (buildTreeWalkInterpreter (Right toks))
  -- If there were scanner errors, exit with 65 after running,
  -- so both scanner and parser errors can be emitted.
  if null errs
    then pure ()
    else exitWith (ExitFailure 65)

run :: Interpreter () -> IO ()
run =
  runInterpreter >=> \case
    Left err -> handleErr err
    Right v -> pure v

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
