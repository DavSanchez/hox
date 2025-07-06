module Main (main) where

import RepresentingCode (prettyPrintExprUnitTests)
import Scanner.Props (scannerProperties)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup
    "Lox Tests"
    [ scannerProperties,
      prettyPrintExprUnitTests
    ]

main :: IO ()
main = defaultMain tests
