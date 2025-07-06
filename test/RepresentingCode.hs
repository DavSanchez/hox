module RepresentingCode (prettyPrintExprUnitTests) where

import Expression
  ( BinaryOperator (..),
    Expression (..),
    Literal (..),
    UnaryOperator (..),
    prettyPrintExpr,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

prettyPrintExprUnitTests :: TestTree
prettyPrintExprUnitTests =
  testGroup
    "Chapter 05: Pretty Prints according to what the CraftInt book expects"
    [ testCase "print 1 + 2" $
        prettyPrintExpr (Binary Plus (Literal (Number 1)) (Literal (Number 2))) `compare` "(+ 1.0 2.0)" @?= EQ,
      testCase "print (-123) * group 45.67" $
        prettyPrintExpr (Binary Star (Unary UMinus (Literal (Number 123))) (Grouping (Literal (Number 45.67)))) `compare` "(* (- 123.0) (group 45.67))" @?= EQ
    ]
