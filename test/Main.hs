module Main (main) where

import Test.HUnit
import qualified Subdistribution as S

test1 = TestCase $ assertEqual "reordering breaks equality" 
    (S.fromList [("a", 0.3), ("b", 0.7)]) 
    (S.fromList [("b", 0.7), ("a", 0.3)])

    

tests = TestList 
    [ TestLabel "test1" test1
    ]    

main :: IO ()
main = runTestTTAndExit tests
