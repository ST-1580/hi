module T10 (
    testT10 
    ) where

import Helper (TestRes (..), check)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, Assertion, (@?=))
import HW3.Base (HiError (..))

lazy :: Assertion
lazy = do check "true || 1 / 0" @?= Ok "true"
          check "true && 1 / 0" @?= EvalError HiErrorDivideByZero
          check "false && 1 / 0" @?= Ok "false"
          check "true && 1 / 0" @?= EvalError HiErrorDivideByZero
          check "false && 1 / 0 || true" @?= Ok "true"

evalError :: Assertion
evalError = do check "echo(5)" @?= EvalError HiErrorInvalidArgument
               check "echo([20, 10])" @?= EvalError HiErrorInvalidArgument           
               check "echo()" @?= EvalError HiErrorArityMismatch

testT10 :: TestTree
testT10 = testGroup "Tests for T9"
            [testCase "Evaluator errors" evalError,
             testCase "Lazy && and ||" lazy]
