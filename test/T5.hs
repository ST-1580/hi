module T5 (
    testT5 
    ) where

import Helper (TestRes (..), check)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, Assertion, (@?=))
import HW3.Base (HiError (..))

sliceAndIndex :: Assertion
sliceAndIndex = do check "[1, 2, 3, 4, 5](0) " @?= Ok "1"
                   check "[1, 2, 3, 4, 5](2) " @?= Ok "3"                   
                   check "[1, 2, 3, 4, 5](-1) " @?= Ok "null"             
                   check "[1, 2, 3, 4, 5](100) " @?= Ok "null"
                   check "[1, 2, 3, 4, 5](0, 2) " @?= Ok "[ 1, 2 ]"
                   check "[1, 2, 3, 4, 5](0, -4) " @?= Ok "[ 1 ]"                   
                   check "[1, 2, 3, 4, 5](2, null) " @?= Ok "[ 3, 4, 5 ]"             
                   check "[1, 2, 3, 4, 5](3, 2) " @?= Ok "[ ]"

math :: Assertion
math = do check "[1, 2] + [3, 4, 5]" @?= Ok "[ 1, 2, 3, 4, 5 ]"
          check "[1, 2] * 2 " @?= Ok "[ 1, 2, 1, 2 ]"

functions :: Assertion
functions = do check "list(1, 2, 3)" @?= Ok "[ 1, 2, 3 ]"
               check "range(5, 10.3)" @?= Ok "[ 5, 6, 7, 8, 9, 10 ]"
               check "range(5, 10.6)" @?= Ok "[ 5, 6, 7, 8, 9, 10, 11 ]"
               check "fold(add, [11, 22, 33])" @?= Ok "66"
               check "fold(div, [11, 22, 33])" @?= Ok "1/66"               
               check "fold(if, [1])" @?= Ok "1"            
               check "length([1, true, \"Hello\"]) " @?= Ok "3"             
               check "reverse([1, true, \"Hello\"]) " @?= Ok "[ \"Hello\", true, 1 ]"

evalError :: Assertion
evalError = do check "[1, 2] * -1 " @?= EvalError HiErrorInvalidArgument               
               check "[1, 2] * 5.2 " @?= EvalError HiErrorInvalidArgument                             
               check "[1, 2](null) " @?= EvalError HiErrorInvalidArgument
               check "fold([1, 2, 3], [11, 22, 33])" @?= EvalError HiErrorInvalidArgument
               check "[1, 2](1, 2, 3)" @?= EvalError HiErrorArityMismatch               

prettyTest :: Assertion
prettyTest = do check "[]" @?= Ok "[ ]"
                check "[1]" @?= Ok "[ 1 ]"

space :: Assertion
space = do check "    [   ]    " @?= Ok "[ ]"
           check "   [    1   ,     2     ]    " @?= Ok "[ 1, 2 ]"

mixed :: Assertion
mixed = do check "list(1, 2, 3, 4, 5)" @?= Ok "[ 1, 2, 3, 4, 5 ]"
           check "fold(add, [2, 5] * 3)" @?= Ok "21"
           check "fold(mul, range(1, 10))" @?= Ok "3628800"
           check "[0, true, false, \"hello\", \"world\"](2, 4)" @?= Ok "[ false, \"hello\" ]"
           check "reverse(range(0.5, 70/8))" @?= Ok "[ 8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5 ]"

testT5 :: TestTree
testT5 = testGroup "Tests for T5"
            [testCase "Functions with lists" functions,
             testCase "Math operators with lists" math,
             testCase "Evaluator errors" evalError,             
             testCase "Lists slice and index" sliceAndIndex,
             testCase "Pretty" prettyTest,
             testCase "Spaces" space,       
             testCase "Mixed tests" mixed]
