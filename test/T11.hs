module T11 (
    testT11 
    ) where

import Helper (TestRes (..), check)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, Assertion, (@?=))
import HW3.Base (HiError(..))

mixed :: Assertion
mixed = do check "count(\"Hello World\").o" @?= Ok "2"
           check "invert(count(\"big blue bag\"))" @?=
               Ok "{ 1: [ \"u\", \"l\", \"i\", \"e\", \"a\" ], 2: [ \"g\", \" \" ], 3: [ \"b\" ] }"
           check "fold(add, values(count(\"Hello, World!\")))" @?= Ok "13"    

func :: Assertion
func = do check "count(\"XXXOX\")" @?= Ok "{ \"O\": 1, \"X\": 4 }"
          check "count([# 58 58 58 4f 58 #])" @?= Ok "{ 79: 1, 88: 4 }"
          check "count([true, true, false, true])" @?= Ok "{ false: 1, true: 3 }"
          check "values({ \"width\": 120, \"height\": 80 })" @?= Ok "[ 80, 120 ]"
          check "keys({ \"width\": 120, \"height\": 80 })" @?= Ok "[ \"height\", \"width\" ]"

functions :: Assertion
functions = do check "{ \"width\": 120, \"height\": 80 }(\"width\")" @?= Ok "120"
               check "{ \"width\": 120, \"height\": 80 }.width" @?= Ok "120"               
               check "[ 1, 2, 3].a" @?= EvalError HiErrorInvalidArgument

spaces :: Assertion
spaces = do check " {    1     :     false    }   " @?= Ok "{ 1: false }"
            check "{\"a\": true}   .a   " @?= Ok "true"
            check "{\"a\": true}. a" @?= ParseError "error"

prettyTest :: Assertion
prettyTest = do check "{}" @?= Ok "{ }"
                check "{1:[#aa bb#]}" @?= Ok "{ 1: [# aa bb #] }"

testT11 :: TestTree
testT11 = testGroup "Tests for T11"
            [testCase "Mixed tests" mixed,
             testCase "Functions" func,
             testCase "Dict as functions" functions,
             testCase "Spaces" spaces,
             testCase "Pretty" prettyTest]
