module T2 (
    testT2 
    ) where

import Helper (TestRes (..), check)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, Assertion, (@?=))

someTests :: Assertion
someTests = do check "not(true)" @?= Ok "false"
               check "and(true, false)" @?= Ok "false"
               check "or(true, false)" @?= Ok "true"
               check "equals(10, 10)" @?= Ok "true"
               check "equals(3, 10)" @?= Ok "false"

functionsEq :: Assertion
functionsEq = do check "equals(greater-than(5, 2), less-than(2, 5))" @?= Ok "true"
                 check "equals(not-equals(5, 2), not(equals(5, 2)))" @?= Ok "true"
                 check "equals(not-less-than(5, 2), not(less-than(5, 2)))" @?= Ok "true"
                 check "equals(not-greater-than(5, 2), not(greater-than(5, 2)))" @?= Ok "true"

lazyIf :: Assertion
lazyIf = do check "if(true, add, mul)" @?= Ok "add"
            check "if(true, add, mul)(10, 10)" @?= Ok "20"
            check "if(false, add, mul)(10, 10)" @?= Ok "100"

prettyTest :: Assertion
prettyTest = do check "true" @?= Ok "true"
                check "false" @?= Ok "false"

mixed :: Assertion
mixed = do check "equals(add(2, 2), 4)" @?= Ok "true"            
           check "less-than(mul(999, 99), 10000)" @?= Ok "false"
           check "if(greater-than(div(2, 5), div(3, 7)), 1, -1)" @?= Ok "-1"          
           check "and(less-than(0, 1), less-than(1, 0))" @?= Ok "false"

testT2 :: TestTree
testT2 = testGroup "Tests for T2"
             [testCase "Logic functions" someTests,
              testCase "Functions equality" functionsEq,
              testCase "Pretty" prettyTest,
              testCase "Lazy if" lazyIf,
              testCase "Mixed tests" mixed]
