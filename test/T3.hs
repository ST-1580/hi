module T3 (
    testT3 
    ) where

import Helper (TestRes (..), check)

import HW3.Base (HiError(..))
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, Assertion, (@?=))

sampleTests :: Assertion
sampleTests = do check "2 + 2" @?= Ok "4"
                 check "2 + 2 * 3" @?= Ok "8"
                 check "(2 + 2) * 3" @?= Ok "12"
                 check "2 + 2 * 3 == (2 + 2) * 3" @?= Ok "false"
                 check "10 == 2*5 && 143 == 11*13" @?= Ok "true"
                 check "2 + + 2" @?= Ok "4"

parseError :: Assertion
parseError = do check "2 + / 2" @?= ParseError "error"
                check "2 + (2 * 2" @?= ParseError "error"
                check "1 < 2 < 3" @?= ParseError "error"

evalError :: Assertion
evalError = do check "1 + or(true, false)" @?= EvalError HiErrorInvalidArgument
               check "1 / 0" @?= EvalError HiErrorDivideByZero

spaces :: Assertion
spaces = do check "  3   +    3   " @?= Ok "6"
            check "  (    3   +    3   )    /     3   " @?= Ok "2"

notEq :: Assertion
notEq = do check "3 /= 2131" @?= Ok "true"
           check "false /= 3" @?= Ok "true"        
           check "true /= true" @?= Ok "false"

testT3 :: TestTree
testT3 = testGroup "Tests for T3"
            [testCase "Evaluator errors" evalError,
             testCase "Parse errors" parseError,
             testCase "Spaces" spaces,
             testCase "/=" notEq,
             testCase "Mixed tests" sampleTests]
