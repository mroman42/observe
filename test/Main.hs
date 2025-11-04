module Main (main) where

import Test.HUnit
import Subdistribution

test1 = TestCase $ assertEqual "reordering breaks equality" 
    (fromList [("a", 0.3), ("b", 0.7)]) 
    (fromList [("b", 0.7), ("a", 0.3)])
    

tests = TestList 
    [ TestLabel "test1" test1
    ]    

main :: IO ()
main = runTestTTAndExit tests
