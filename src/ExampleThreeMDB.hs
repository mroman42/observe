{-# LANGUAGE RebindableSyntax #-}

module ExampleThreeMDB where

import Prelude hiding (id, (.), (>>=), (>>), return)
import FinitaryMonad
import MDB

data Health = Healthy | Ill deriving (Show, Eq)
data TestResult = Positive | Negative deriving (Show, Eq)

prior :: MDB Health
prior = distribution [(Healthy, 95/100), (Ill, 5/100)]

test :: Health -> MDB TestResult
test Healthy = distribution [(Positive, 5/100), (Negative, 95/100)]
test Ill = distribution [(Positive, 90/100), (Negative, 10/100)]

experiment = do
  p <- prior
  t <- test p
  r <- bag [Positive, Positive, Negative]
  observe (t == r)
  return p

experiment2 = do
  r <- bag [Positive, Positive, Negative]
  p <- do
    p <- prior
    t <- test p
    observe (t == r)
    return p
  return p
