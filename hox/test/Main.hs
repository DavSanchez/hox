module Main (main) where

import RepresentingCode (displayExprUnitTests)
import Scanner.Props (scannerProperties)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup
    "Lox Tests"
    [ scannerProperties,
      displayExprUnitTests
    ]

main :: IO ()
main = defaultMain tests
