module Main (main) where

import Test.HUnit
import Subdistribution (subdistribution)
import Bag (bag, Bag)

check lbl = TestLabel lbl . TestCase . assertBool lbl


tests = TestList 
  [ check "subEq1" $
     subdistribution [("a", 0.3), ("b", 0.7)] == 
     subdistribution [("b", 0.7), ("a", 0.3)]
  , check "bagEq1" $ 
     bag [('x', 3), ('y', 2)] == 
     bag [('y', 2), ('x', 1), ('x', 2)]
  , check "bagNeq1" $
     bag [('x', 3), ('y', 2)] /=
     bag [('x', 1), ('y', 2), ('z', 2)]
  , check "bagNeq2" $
     bag [(bag [] :: Bag (Bag Int), 1)] /=
     bag [(bag [], 1), (bag [], 1)]
  , check "bagNeq3" $
     bag [] /=
     bag [('x',1)]
  ]    

main :: IO ()
main = runTestTTAndExit tests
