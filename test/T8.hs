module T8 (
    testT8 
    ) where

import Helper (TestRes (..), check)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, Assertion, (@?=))

math :: Assertion
math = do check "parse-time(\"2021-12-15 00:00:00 UTC\") + 1000" @?= Ok "parse-time(\"2021-12-15 00:16:40 UTC\")"
          check "1000 + parse-time(\"2021-12-15 00:00:00 UTC\")" @?= Ok "parse-time(\"2021-12-15 00:16:40 UTC\")"
          check "parse-time(\"2021-12-15 00:37:51.000890793 UTC\") - parse-time(\"2021-12-15 00:37:47.649047038 UTC\")"
                @?= Ok "3.351843755"

prettyTest :: Assertion
prettyTest = do check "parse-time(\"2021-12-15 00:00:00 UTC\")" @?= Ok "parse-time(\"2021-12-15 00:00:00 UTC\")"

testT8 :: TestTree
testT8 = testGroup "Tests for T8"
            [testCase "Math operators with time" math,
             testCase "Pretty" prettyTest]
