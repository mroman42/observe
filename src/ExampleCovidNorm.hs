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

-- Jeffrey: Ill with probability ~9%.
experimentJeffrey :: Normalized Individual
experimentJeffrey = do
    (r,p,t) <- do
        r <- uniform [Pos, Pos, Neg]
        p <- distribution [(Ill, 5/100), (Healthy, 95/100)]
        t <- testing p
        return (r,p,t)
    observe (t == r)
    return p

-- Variational: Ill with probability ~33%
experiment :: Normalized Individual
experiment = do
    r <- uniform [Pos, Pos, Neg]
    (p,t) <- do
        p <- distribution [(Ill, 5/100), (Healthy, 95/100)]
        t <- testing p
        return (p,t)
    observe (t == r)
    return p    

-- Pearl, however, changes with size, and thus needs the bag instead of its distribution. It cannot use the homomorphism.