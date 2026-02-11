{-# LANGUAGE RebindableSyntax #-}

module ExampleThreeMBD where


import Prelude hiding (id, (.), (>>=), (>>), return)
import FinitaryMonad
import MBD

data Health = Healthy | Ill deriving (Show, Eq)
data TestResult = Positive | Negative deriving (Show, Eq)

neg :: TestResult -> TestResult
neg Positive = Negative
neg Negative = Positive

prior :: MBD Health
prior = distribution [(Healthy, 95/100), (Ill, 5/100)]

test :: Health -> MBD TestResult
test Healthy = distribution [(Positive, 5/100), (Negative, 95/100)]
test Ill = distribution [(Positive, 90/100), (Negative, 10/100)]

--experiment :: MBD Health
-- close to what we want
experiment = do
  (r,p,t) <- do 
    (r,p) <- do
        r <- bag [Positive, Positive, Negative]
        p <- prior
        return (r,p)
    t <- test p
    return (r,p,t)
  observe (r == t)
  return (p, t, r)

experiment2 :: MBD Health
experiment2 = do
    (patient, result, seeing) <- do
        (patient, result) <- do 
            patient <- prior
            result <- test patient
            return (patient, result)
        seeing <- bag [Positive, Positive, Negative]
        return (patient, result, seeing)
    observe (result == seeing)
    return patient
