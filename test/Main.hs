module Main (main) where

import Scanner.Props (scannerProperties)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests = testGroup "Lox Tests" [scannerProperties]

main :: IO ()
main = defaultMain tests
