{-# LANGUAGE RebindableSyntax #-}

module ExampleThreeNormBags where


import Prelude hiding (id, (.), (>>=), (>>), return)
import FinMonad.FinMonad
import FinMonad.Distribution (Distribution(..))
import qualified FinMonad.Distribution as D
import NormBag
import FrequentistTransformation


data Health = Healthy | Ill deriving (Show, Eq)
data TestResult = Positive | Negative deriving (Show, Eq)

neg :: TestResult -> TestResult
neg Positive = Negative
neg Negative = Positive

prior :: NormBag Health
prior = distribution [(Healthy, 95/100), (Ill, 5/100)]

test :: Health -> NormBag TestResult
test Healthy = distribution [(Positive, 5/100), (Negative, 95/100)]
test Ill = distribution [(Positive, 90/100), (Negative, 10/100)]



e1 = do
  (p,r,t) <- do
    (p,r) <- do
        p <- prior
        r <- bag [Positive, Positive, Negative]
        return (p,r)    
    t <- test p
    return (p,r,t)
  return p
