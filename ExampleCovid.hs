{-# LANGUAGE RebindableSyntax #-}
module ExampleCovid where

import Prelude hiding ((>>=), (>>), return)
import Subdistribution
import Data.Ord
import Data.List
import Data.MultiSet qualified as MSet

data Result = Pos | Neg deriving (Eq, Show, Ord)
data Individual = Ill | Healthy deriving (Eq, Show)

testing :: Individual -> Distribution Result
testing Ill = fromList [(Pos, 9/10), (Neg, 1 / 10)]
testing Healthy = fromList [(Pos, 2/5), (Neg, 3/5)]

multiset :: (Ord a) => [a] -> MSet.MultiSet a
multiset = MSet.fromList


-- This code solves Jacobs' question on Covid tests using Pearl's update.
-- validity positiveTest is 17/40
positiveTest :: Distribution ()
positiveTest = do
    individual <- distribution [(Ill, 1/20), (Healthy, 19/20)]
    result <- testing individual
    observe (result == Pos)
    return ()

-- validity threeTest 381 % 4000
-- normalized update [(Ill,27 % 635),(Healthy,608 % 635)]
threeTest :: Distribution Individual
threeTest = do
    individual <- distribution [(Ill, 1/20), (Healthy, 19/20)]
    result1 <- testing individual
    result2 <- testing individual
    result3 <- testing individual
    observe (result1 == Pos)
    observe (result2 == Pos)
    observe (result3 == Neg)
    return individual

-- validity threeTest 1143 % 4000
-- normalized update [(Ill,27 % 635),(Healthy,608 % 635)]
threeUnorderedTest :: Distribution Individual
threeUnorderedTest = do
  individual <- distribution [(Ill, 1/20), (Healthy, 19/20)]
  result1 <- testing individual
  result2 <- testing individual
  result3 <- testing individual
  observe (multiset [result1,result2,result3] == multiset [Pos,Pos,Neg])
  return individual