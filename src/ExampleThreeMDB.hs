{-# LANGUAGE RebindableSyntax #-}

module ExampleThreeMDB where

import Prelude hiding (id, (.), (>>=), (>>), return)
import Distribution hiding ((>>=), (>>), return, distribution)
import Bag hiding ((>>=), (>>), return, distribution, bag)
import qualified Bag as B
import AffineBag hiding ((>>=), (>>), return, distribution)
import AuxiliarySemiring 
import FinitaryMonad
import MDB


data Health = Healthy | Ill deriving (Show, Eq)
data TestResult = Positive | Negative deriving (Show, Eq)

prior :: MDB Health
prior = distribution [(Healthy, 95/100), (Ill, 5/100)]

test :: Health -> MDB TestResult
test Healthy = distribution [(Positive, 5/100), (Negative, 95/100)]
test Ill = distribution [(Positive, 90/100), (Negative, 10/100)]

expm = do
  p <- prior
  t <- test p
  r <- bag [Positive, Positive, Negative]
  observe (r == t)
  return (p, r)

experiment = do
  p <- prior
  t <- test p
  r <- bag [Positive, Positive, Negative]
  observe (t == r)
  return p

-- Variational Jeffrey
experiment2 = do
  r <- bag [Positive, Positive, Negative]
  (p,t) <- do
    p <- prior
    t <- test p
    return (p,t)
  observe (t == r)
  return p

experiment5 = do
  r <- bag [Positive, Positive, Negative]
  (p,t) <- do
    p <- prior
    t <- test p
    return (p,t)
  observe (t == r)
  return p
  
experiment3 = do
  r <- bag [Positive, Positive, Negative]
  p <- do
    p <- prior
    t <- test p
    observe (t == r)
    return p
  return p

experiment4 = do
  (p,t,r) <- do 
    (p,t) <- do
        p <- prior
        t <- test p
        return (p,t)
    r <- bag [Positive, Positive, Negative]
    return (p,t,r)
  observe (t == r)
  return p

flrn :: (Eq a) => AffineBag a -> Distribution a  
flrn (AffineBag (Bag d)) = Distribution $ valueMap 
  (\v -> (fromIntegral v) / (fromIntegral (totalWeight d))) d

collapsing :: (Eq a) => MDB a -> Maybe (Distribution a)
collapsing (MDB Nothing) = Nothing
collapsing (MDB (Just d)) = Just $ fJoin $ fMap flrn d