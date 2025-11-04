module Main (main) where

import Test.HUnit


tests = TestList 
    [ TestLabel "test2" (TestCase $ assertBool "Why is this not running," False)
    ]    

main :: IO ()
main = runTestTTAndExit tests
