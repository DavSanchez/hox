module RepresentingCode (prettyPrintExprUnitTests) where

import Expression
  ( BinaryOperator (Plus, Star),
    Expression (BinaryOperation, Grouping, Literal, UnaryOperation),
    Literal (Number),
    UnaryOperator (UMinus),
    prettyPrint,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

prettyPrintExprUnitTests :: TestTree
prettyPrintExprUnitTests =
  testGroup
    "Chapter 05: Pretty Prints according to what the CraftInt book expects"
    [ testCase "print 1 + 2" $
        prettyPrint (BinaryOperation 0 Plus (Literal (Number 1)) (Literal (Number 2))) `compare` "(+ 1.0 2.0)" @?= EQ,
      testCase "print (-123) * group 45.67" $
        prettyPrint (BinaryOperation 0 Star (UnaryOperation 0 UMinus (Literal (Number 123))) (Grouping (Literal (Number 45.67)))) `compare` "(* (- 123.0) (group 45.67))" @?= EQ
    ]
