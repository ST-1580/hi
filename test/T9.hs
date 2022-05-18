module T9 (
    testT9 
    ) where

import Helper (TestRes (..), check)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, Assertion, (@?=))
import HW3.Base (HiError (..))

evalError :: Assertion
evalError = do check "rand(5.3, 10)" @?= EvalError HiErrorInvalidArgument
               check "rand(20, 10)" @?= EvalError HiErrorInvalidArgument           
               check "rand(5)" @?= EvalError HiErrorArityMismatch

testT9 :: TestTree
testT9 = testGroup "Tests for T9"
            [testCase "Evaluator errors" evalError]
