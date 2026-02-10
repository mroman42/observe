{-# LANGUAGE RebindableSyntax #-}

module ExampleThreeMBD where

import Prelude hiding (id, (.), (>>=), (>>), return)
--import DistBag hiding ((>>=), (>>), return, distribution)
import FinitaryMonad
import MBD

data Health = Healthy | Ill deriving (Show, Eq)
data TestResult = Positive | Negative deriving (Show, Eq)

neg :: TestResult -> TestResult
neg Positive = Negative
neg Negative = Positive

prior :: MBD Health
prior = distribution [(Healthy, 99/100), (Ill, 1/100)]

test :: Health -> MBD TestResult
test Healthy = distribution [(Positive, 5/100), (Negative, 95/100)]
test Ill = distribution [(Positive, 90/100), (Negative, 10/100)]


experiment = do
    (patient, result) <- do 
        patient <- prior
        result <- test patient
        return (patient, result)
    observe (result == Positive)
    return patient
