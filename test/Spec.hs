import T1 (testT1)
import T2 (testT2) 
import T3 (testT3)
import T4 (testT4)
import T5 (testT5)
import T6 (testT6)
import T7 (testT7)
import T8 (testT8)
import T9 (testT9)
import T10 (testT10)
import T11 (testT11)

import Test.Tasty ( defaultMain, testGroup )

main :: IO ()
main = defaultMain (testGroup "Tests"
                [testT1, testT2, testT3, testT4, testT5, testT6, testT7, testT8, testT9, testT10, testT11])