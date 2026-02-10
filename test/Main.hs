module Main (main) where

import Test.HUnit
import Subdistribution (subdistribution)
import Bag (Bag, bagWeight)
import Test.QuickCheck
import Distribution (Distribution(..), squeezeDist, weightOf)
import Data.Ratio
import AuxiliarySemiring (condense)
import FinitaryMonad

check lbl = TestLabel lbl . TestCase . assertBool lbl


tests = TestList 
  [ check "subEq1" $
     subdistribution [("a", 0.3), ("b", 0.7)] == 
     subdistribution [("b", 0.7), ("a", 0.3)]
  , check "bagEq1" $ 
     bagWeight [('x', 3), ('y', 2)] == 
     bagWeight [('y', 2), ('x', 1), ('x', 2)]
  , check "bagNeq1" $
     bagWeight [('x', 3), ('y', 2)] /=
     bagWeight [('x', 1), ('y', 2), ('z', 2)]
  , check "bagNeq2" $
     bagWeight [(bagWeight [] :: Bag (Bag Int), 1)] /=
     bagWeight [(bagWeight [], 1), (bagWeight [], 1)]
  , check "bagNeq3" $
     bagWeight [] /=
     bagWeight [('x',1)]
  ]    

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs  

arbitraryInterval :: Gen Rational
arbitraryInterval = do
   x <- choose (1, 1000) :: Gen Integer
   y <- choose (1, 1000) :: Gen Integer
   case x < y of
      True -> return $ x % y
      False -> return $ y % (x+1)

-- too slow
instance (Arbitrary a, Eq a) => Arbitrary (Distribution a) where
   arbitrary = sized aDist
      where
         aDist 0 = do
            x <- arbitrary
            return (Distribution [(x,1)])
         aDist n | n>0 = do
            r <- arbitraryInterval
            x <- arbitrary
            d <- aDist (n-1)
            return $ squeezeDist x r d

-- instance (Arbitrary a) => Arbitrary (Bag a) where

prop_distribution_associative 
  :: Distribution (Distribution (Distribution Char)) -> Bool
prop_distribution_associative d 
  = (fJoin $ fMap fJoin d) == (fJoin $ fJoin d)


main :: IO ()
main = do
   quickCheck prop_reverse
   runTestTTAndExit tests
