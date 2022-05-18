module T4 (
    testT4 
    ) where

import Helper (TestRes (..), check)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, Assertion, (@?=))
import HW3.Base (HiError (..))

sliceAndIndex :: Assertion
sliceAndIndex = do check "\"Hello World\"(0) " @?= Ok "\"H\""
                   check "\"Hello World\"(7) " @?= Ok "\"o\""                   
                   check "\"Hello World\"(-1) " @?= Ok "null"             
                   check "\"Hello World\"(100) " @?= Ok "null"
                   check "\"Hello World\"(0, 5) " @?= Ok "\"Hello\""
                   check "\"Hello World\"(0, -4) " @?= Ok "\"Hello W\""                   
                   check "\"Hello World\"(2, null) " @?= Ok "\"llo World\""             
                   check "\"Hello World\"(3, 2) " @?= Ok "\"\""

math :: Assertion
math = do check "\"Hello\" + \"World\"" @?= Ok "\"HelloWorld\""
          check "\"Cat\" * 5 " @?= Ok "\"CatCatCatCatCat\""          
          check "\"/home/user\" / \"hi\" " @?= Ok "\"/home/user/hi\""

functions :: Assertion
functions = do check "length(\"Hello World\")" @?= Ok "11"
               check "to-upper(\"Hello World\")" @?= Ok "\"HELLO WORLD\""
               check "to-lower(\"Hello World\")" @?= Ok "\"hello world\""
               check "reverse(\"stressed\")" @?= Ok "\"desserts\""               
               check "trim(\" Hello World \")" @?= Ok "\"Hello World\""

evalError :: Assertion
evalError = do check "sub(\"a\", \"b\")" @?= EvalError HiErrorInvalidArgument
               check "\"Cat\" * -1 " @?= EvalError HiErrorInvalidArgument               
               check "\"Cat\" * 5.2 " @?= EvalError HiErrorInvalidArgument                             
               check "\"Hello World\"(null) " @?= EvalError HiErrorInvalidArgument
               check "\"a\"(1, 2, 3)" @?= EvalError HiErrorArityMismatch

prettyTest :: Assertion
prettyTest = do check "\"Hello!\"" @?= Ok "\"Hello!\""
                check "\"Hel\'lo!\"" @?= Ok "\"Hel\'lo!\""                
                check "null" @?= Ok "null"

mixed :: Assertion
mixed = do check "to-upper(\"what a nice language\")(7, 11)" @?= Ok "\"NICE\""
           check "\"Hello\" == \"World\"" @?= Ok "false"
           check "length(\"Hello\" + \"World\")" @?= Ok "10"
           check "length(\"hehe\" * 5) / 3" @?= Ok "6 + 2/3"

testT4 :: TestTree
testT4 = testGroup "Tests for T4"
            [testCase "Functions with strings" functions,
             testCase "Math operators with string" math,
             testCase "Evaluator errors" evalError,             
             testCase "String slice and index" sliceAndIndex,
             testCase "Pretty" prettyTest,             
             testCase "Mixed tests" mixed]
