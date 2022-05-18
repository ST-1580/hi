module T7 (
    testT7 
    ) where

import Helper (TestRes (..), check)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, Assertion, (@?=))
import HW3.Base (HiError(..))

mixed :: Assertion
mixed = do check "write(\"path\", \"Hello\")" @?= Ok "write(\"path\", [# 48 65 6c 6c 6f #])"
           check "write(\"path\", [# 48 65 6c 6c 6f #])" @?= Ok "write(\"path\", [# 48 65 6c 6c 6f #])"

evalError :: Assertion
evalError = do check "read(5)" @?= EvalError HiErrorInvalidArgument
               check "write(5, 10)" @?= EvalError HiErrorInvalidArgument               
               check "write(5)" @?= EvalError HiErrorArityMismatch

prettyTest :: Assertion
prettyTest = do check "cwd" @?= Ok "cwd"
                check "cwd!" @?= Ok "null"

testT7 :: TestTree
testT7 = testGroup "Tests for T7"
            [testCase "Evaluator errors" evalError,
             testCase "Mixed tests" mixed,
             testCase "Pretty" prettyTest]
