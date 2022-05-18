module T1 (
    testT1 
    ) where

import HW3.Base (HiError(..))
import Helper (TestRes (..), check)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, Assertion, (@?=))

constants :: Assertion
constants = do check "1" @?= Ok "1"
               check "3.14" @?= Ok "3.14"
               check "-1.618" @?= Ok "-1.618"

functions :: Assertion
functions = do check "add(500, 12)" @?= Ok "512"
               check "sub(10, 100)" @?= Ok "-90"
               check "mul(23, 768)" @?= Ok "17664"
               check "div(57, 190)" @?= Ok "0.3"

spaces :: Assertion
spaces = do check "    1     " @?= Ok "1"
            check "  add   (  5  ,   -    1   )    " @?= Ok "4"

prettyTest :: Assertion
prettyTest = do check "1.2e5" @?= Ok "120000"
                check "div(4, 3)" @?= Ok "1 + 1/3"
                check "div(-4, 3)" @?= Ok "-1 - 1/3"
                check "div(1, 3)" @?= Ok "1/3"
                check "div(-1, 3)" @?= Ok "-1/3"

parseError :: Assertion
parseError = do check "gg" @?= ParseError "error"
                check "-div(4, 4)" @?= ParseError "error"

evalError :: Assertion
evalError = do check "sub()" @?= EvalError HiErrorArityMismatch
               check "sub(1, 2, 3)" @?= EvalError HiErrorArityMismatch
               check "sub(mul, 10, add)" @?= EvalError HiErrorArityMismatch
               check "add(div(1, 0))" @?= EvalError HiErrorArityMismatch
               check "5(1)" @?= EvalError HiErrorInvalidFunction
               check "sub(10, add)" @?= EvalError HiErrorInvalidArgument
               check "div(1, sub(5, 5))" @?= EvalError HiErrorDivideByZero

mixed :: Assertion
mixed = do check "-15" @?= Ok "-15"            
           check "add(100, -15)" @?= Ok "85"
           check "add(3, div(14, 100))" @?= Ok "3.14"          
           check "div(10, 3)" @?= Ok "3 + 1/3"
           check "sub(mul(201, 11), 0.33)" @?= Ok "2210.67"

testT1 :: TestTree
testT1 = testGroup "Tests for T1"
            [testCase "Constants" constants,
             testCase "Functions" functions,
             testCase "Spaces" spaces,
             testCase "Pretty" prettyTest,
             testCase "Parser errors" parseError,
             testCase "Evaluator errors" evalError,
             testCase "Mixed tests" mixed]
