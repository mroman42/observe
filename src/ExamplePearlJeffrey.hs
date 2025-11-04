{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use let" #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Redundant bracket" #-}

module ExamplePearlJeffrey where

import Prelude hiding (id, (.), (>>=), (>>), return)
import Subdistribution
import Prelude hiding ((>>=), (>>), return, Left, Right)
import Data.MultiSet qualified as MSet
import Control.Category
import Control.Arrow
import qualified Control.Category as Control


data Health = Healthy | Ill deriving (Show, Eq)
data TestResult = Positive | Negative deriving (Show, Eq)

neg :: TestResult -> TestResult
neg Positive = Negative
neg Negative = Positive

prior :: Distribution Health
prior = distribution [(Healthy, 99/100), (Ill, 1/100)]

test :: Health -> Distribution TestResult
test Healthy = distribution [(Positive, 5/100), (Negative, 95/100)]
test Ill = distribution [(Positive, 90/100), (Negative, 10/100)]

experimentPearl :: Distribution Health
experimentPearl = do
  patient <- prior
  result <- test patient
  reading <- distribution [(result, 80/100), (neg result, 20/100)]
  observe (reading == Positive)
  return patient

experimentPearl2 :: Distribution Health
experimentPearl2 = do
  patient <- prior
  result <- test patient
  reading <- distribution [ (Positive, 80/100), (Negative, 20/100) ]
  observe (reading == result)
  return patient

experimentPearl3 :: Distribution Health
experimentPearl3 = do
  procedure <- distribution [
    (do patient <- prior ; r <- test patient ; observe (r == Positive) ; return patient, 80/100) ,
    (do patient <- prior ; r <- test patient ; observe (r == Negative) ; return patient, 20/100) ]
  procedure


experimentJeffrey :: Distribution Health
experimentJeffrey = do
  patient1 <- prior
  result1 <- test patient1
  observe (result1 == Positive)
  patient2 <- prior
  result2 <- test patient2
  observe (result2 == Negative)
  distribution [(patient1, 80/100), (patient2, 20/100)]

experimentJeffrey2 :: Distribution Health
experimentJeffrey2 = do
  procedure <- return (do 
    patient <- prior
    result <- test patient
    return (patient, result))
  (p1, r1) <- procedure
  (p2, r2) <- procedure
  observe (r1 == Positive)
  observe (r2 == Negative)
  distribution [(p1, 80/100), (p2, 20/100)]

-- Applicative-like combinators
cbn :: (Eq a) => a -> Distribution a
cbn = return  

(<%>) :: (Eq a, Eq b) => (a -> b) -> (Distribution a -> Distribution b)
f <%> d = dmap f d

observeBn :: Distribution Bool -> Distribution ()
observeBn d = do
  x <- d
  observe x
  return ()

experimentPearl4 :: Distribution Health
experimentPearl4 = do
  patient <- prior
  result <- cbn (test patient)
  r1 <- result
  reading <- distribution [(r1, 80/100), (neg r1, 20/100)]
  observe (reading == Positive)
  return patient

experiment :: Distribution Health
experiment = do
  patient <- cbn (prior)
  result <- test <%> patient
  reading <- distribution [(Positive, 80/100), (Negative, 20/100)]
  _ <- observeBn ((reading ==) <%> result)
  patient
