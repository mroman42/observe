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


-- Pearl
e1 = frql $ do
  (p,r,t) <- do
    (p,r) <- do
        p <- prior
        r <- bag [Positive, Positive, Negative]
        return (p,r)    
    t <- test p
    return (p,r,t)
  observe (t == r)
  return p

-- Jeffrey
e2 = frql $ do
  (p,r,t) <- do
    (p,r) <- do
        r <- bag [Positive, Positive, Negative]
        p <- prior
        return (p,r)    
    t <- test p
    return (p,r,t)
  observe (t == r)
  return p
