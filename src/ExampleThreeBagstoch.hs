{-# LANGUAGE RebindableSyntax #-}

module ExampleThreeBagstoch where

import Prelude hiding (id, (.), (>>=), (>>), return)
import DistBag
import FinitaryMonad

data Health = Healthy | Ill deriving (Show, Eq)
data TestResult = Positive | Negative deriving (Show, Eq)

neg :: TestResult -> TestResult
neg Positive = Negative
neg Negative = Positive

prior :: BagStoch Health
prior = distribution [(Healthy, 99/100), (Ill, 1/100)]

test :: Health -> BagStoch TestResult
test Healthy = distribution [(Positive, 5/100), (Negative, 95/100)]
test Ill = distribution [(Positive, 90/100), (Negative, 10/100)]

--experiment :: BagStoch Health
experiment = do
    patient <- prior
    result <- test patient
    --observe (result == Positive)
    return result