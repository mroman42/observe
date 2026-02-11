{-# LANGUAGE RebindableSyntax #-}

module ExamplePearlJeffrey where

import FinMonad.Subdistribution
import Prelude hiding (id, (.), (>>=), (>>), return, id, Left, Right)
import Data.MultiSet qualified as MSet
import Control.Category
import Control.Arrow
import qualified Control.Category as Control


data Health = Healthy | Ill deriving (Show, Eq)
data TestResult = Positive | Negative deriving (Show, Eq)

neg :: TestResult -> TestResult
neg Positive = Negative
neg Negative = Positive

prior :: Subdistribution Health
prior = subdistribution [(Healthy, 99/100), (Ill, 1/100)]

test :: Health -> Subdistribution TestResult
test Healthy = subdistribution [(Positive, 5/100), (Negative, 95/100)]
test Ill = subdistribution [(Positive, 90/100), (Negative, 10/100)]



experimentPearl :: Subdistribution Health
experimentPearl = do
  patient <- prior
  result <- test patient
  reading <- subdistribution [(result, 80/100), (neg result, 20/100)]
  observe (reading == Positive)
  return patient
--- >>> experimentPearl
-- <Subdistribution>
-- Validity: 2351 % 10000
-- Posterior: <Distribution> [(Healthy,2277 % 2351),(Ill,74 % 2351)]  

experimentPearl2 :: Subdistribution Health
experimentPearl2 = do
  patient <- prior
  result <- test patient
  reading <- subdistribution [ (Positive, 80/100), (Negative, 20/100) ]
  observe (reading == result)
  return patient
--- >>> experimentPearl2
-- <Subdistribution>
-- Validity: 2351 % 10000
-- Posterior: <Distribution> [(Healthy,2277 % 2351),(Ill,74 % 2351)]

experimentPearl3 :: Subdistribution Health
experimentPearl3 = do
  procedure <- subdistribution [
    (do patient <- prior ; r <- test patient ; observe (r == Positive) ; return patient, 80/100) ,
    (do patient <- prior ; r <- test patient ; observe (r == Negative) ; return patient, 20/100) ]
  procedure
--- >>> experimentPearl3
-- <Subdistribution>
-- Validity: 2351 % 10000
-- Posterior: <Distribution> [(Healthy,2277 % 2351),(Ill,74 % 2351)]

experimentJeffrey :: Subdistribution Health
experimentJeffrey = do
  patient1 <- prior
  result1 <- test patient1
  observe (result1 == Positive)
  patient2 <- prior
  result2 <- test patient2
  observe (result2 == Negative)
  subdistribution [(patient1, 80/100), (patient2, 20/100)]
--- >>> experimentJeffrey
-- <Subdistribution>
-- Validity: 220311 % 4000000
-- Posterior: <Distribution> [(Healthy,21461 % 24479),(Ill,3018 % 24479)]

experimentJeffrey2 :: Subdistribution Health
experimentJeffrey2 = do
  procedure <- return (do 
    patient <- prior
    result <- test patient
    return (patient, result))
  (p1, r1) <- procedure
  (p2, r2) <- procedure
  observe (r1 == Positive)
  observe (r2 == Negative)
  subdistribution [(p1, 80/100), (p2, 20/100)]
--- >>> experimentJeffrey2  
-- <Subdistribution>
-- Validity: 220311 % 4000000
-- Posterior: <Distribution> [(Healthy,21461 % 24479),(Ill,3018 % 24479)]

-- Applicative-like combinators
cbn :: (Eq a) => a -> Subdistribution a
cbn = return  


observeBn :: Subdistribution Bool -> Subdistribution ()
observeBn d = do
  x <- d
  observe x
  return ()

experimentPearl4 :: Subdistribution Health
experimentPearl4 = do
  patient <- prior
  result <- cbn (test patient)
  r1 <- result
  reading <- subdistribution [(r1, 80/100), (neg r1, 20/100)]
  observe (reading == Positive)
  return patient

experimentPearl5 :: Subdistribution Health
experimentPearl5 = do
  patient <- cbn (prior)
  result <- test <%> patient
  reading <- subdistribution [(Positive, 80/100), (Negative, 20/100)]
  _ <- observeBn ((reading ==) <%> result)
  patient

experimentJeffrey3 :: Subdistribution Health
experimentJeffrey3 = do
  p1  <- procedure Positive
  p2  <- procedure Negative
  subdistribution [(p1, 80/100), (p2, 20/100)]

 where
  procedure :: TestResult -> Subdistribution Health
  procedure r = do
    patient <- prior
    result <- test patient
    observe (result == r)
    return patient

experiment :: Subdistribution Health
experiment = do
  s <- subdistribution [(Positive, 80/100), (Negative, 20/100)]
  procedure s

 where
  procedure :: TestResult -> Subdistribution Health
  procedure r = do
    patient <- prior
    result <- test patient
    observe (result == r)
    return patient
