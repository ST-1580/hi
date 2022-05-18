module T6 (
    testT6 
    ) where

import Helper (TestRes (..), check)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, Assertion, (@?=))
import HW3.Base (HiError (..))

sliceAndIndex :: Assertion
sliceAndIndex = do check "[# 01 02 03 04 05 #](0) " @?= Ok "1"
                   check "[# 01 02 03 04 05 #](2) " @?= Ok "3"                   
                   check "[# 01 02 03 04 05 #](-1) " @?= Ok "null"             
                   check "[# 01 02 03 04 05 #](100) " @?= Ok "null"
                   check "[# 01 02 03 04 05 #](0, 2) " @?= Ok "[# 01 02 #]"
                   check "[# 01 02 03 04 05 #](0, -4) " @?= Ok "[# 01 #]"                   
                   check "[# 01 02 03 04 05 #](2, null) " @?= Ok "[# 03 04 05 #]"             
                   check "[# 01 02 03 04 05 #](3, 2) " @?= Ok "[# #]"

math :: Assertion
math = do check "[# 01 02 #] + [# 03 04 05 #]" @?= Ok "[# 01 02 03 04 05 #]"
          check "[# 01 02 #] * 2 " @?= Ok "[# 01 02 01 02 #]"

functions :: Assertion
functions = do check "pack-bytes([ 3, 255, 158, 32 ])" @?= Ok "[# 03 ff 9e 20 #]"
               check "unpack-bytes([# 10 20 30 #])" @?= Ok "[ 16, 32, 48 ]"
               check "encode-utf8(\"Hello!\")" @?= Ok "[# 48 65 6c 6c 6f 21 #]"
               check "decode-utf8([# 48 65 6c 6c 6f #])" @?= Ok "\"Hello\""
               check "decode-utf8([# c3 28 #])" @?= Ok "null"
               check "length([# c3 28 #]) " @?= Ok "2"             
               check "reverse([# c3 28 #]) " @?= Ok "[# 28 c3 #]"

evalError :: Assertion
evalError = do check "[# 01 02 #] * -1 " @?= EvalError HiErrorInvalidArgument               
               check "[# 01 02 #] * 5.2 " @?= EvalError HiErrorInvalidArgument                             
               check "[# 01 02 #](null) " @?= EvalError HiErrorInvalidArgument
               check "fold([# 01 02 03 #], [# 11 22 33 #])" @?= EvalError HiErrorInvalidArgument
               check "[# 01 02 #](1, 2, 3)" @?= EvalError HiErrorArityMismatch               

prettyTest :: Assertion
prettyTest = do check "[##]" @?= Ok "[# #]"
                check "[# 01 #]" @?= Ok "[# 01 #]"

toFrom :: Assertion
toFrom = do check "unzip(zip([# 01 #]))" @?= Ok "[# 01 #]"
            check "deserialise(serialise(null))" @?= Ok "null"
            check "deserialise(serialise([# 01 #]))" @?= Ok "[# 01 #]"         
            check "deserialise(serialise(2 + 2))" @?= Ok "4"

parseError :: Assertion
parseError = do check "[# 0 #]" @?= ParseError "error"
                check "[# 000 #]" @?= ParseError "error"                
                check "[ # 00 #]" @?= ParseError "error"                              
                check "[# 00 # ]" @?= ParseError "error"

space :: Assertion
space = do check "    [#   #]    " @?= Ok "[# #]"
           check "   [#    01        02     #]    " @?= Ok "[# 01 02 #]"

mixed :: Assertion
mixed = do check "pack-bytes(range(30, 40))" @?= Ok "[# 1e 1f 20 21 22 23 24 25 26 27 28 #]"
           check "zip(encode-utf8(\"Hello, World!\" * 100))" @?= Ok "[# 78 da f3 48 cd c9 c9 d7 51 08 cf 2f ca 49 51 f4 18 e5 8c 72 46 39 a3 9c 91 cd 01 00 2c f5 b9 14 #]"
           check "decode-utf8([# 68 69 #] * 5)" @?= Ok "\"hihihihihi\""
           check "unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])" @?= Ok "[# 01 02 03 #]"

testT6 :: TestTree
testT6 = testGroup "Tests for T6"
            [testCase "Functions with bytes" functions,
             testCase "Math operators with bytes" math,
             testCase "Parse errors" parseError,
             testCase "Evaluator errors" evalError,             
             testCase "Bytes slice and index" sliceAndIndex,
             testCase "Unzip/Zip and deserialise/serialise" toFrom,
             testCase "Pretty" prettyTest,
             testCase "Spaces" space,       
             testCase "Mixed tests" mixed]
