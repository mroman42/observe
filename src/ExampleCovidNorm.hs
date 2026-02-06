{-# LANGUAGE RebindableSyntax #-}

module ExampleCovidNorm where

import Prelude hiding ((>>=), (>>), return)
import NormalizedDistribution
import Data.Ord
import Data.List
import Data.MultiSet qualified as MSet

data Result = Pos | Neg deriving (Eq, Show, Ord)
data Individual = Ill | Healthy deriving (Eq, Show)

testing :: Individual -> Normalized Result
testing Ill     = distribution [(Pos, 90/100), (Neg, 10/100)]
testing Healthy = distribution [(Pos,  5/100), (Neg, 95/100)]

multiset :: (Ord a) => [a] -> MSet.MultiSet a
multiset = MSet.fromList


threeTest :: Normalized Individual
threeTest = do
    individual <- distribution [(Ill, 5/100), (Healthy, 95/100)]
    result1 <- testing individual
    observe (result1 == Pos)
    result2 <- testing individual
    observe (result2 == Pos)
    result3 <- testing individual
    observe (result3 == Pos)
    return individual
