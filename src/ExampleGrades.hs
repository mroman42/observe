{-# LANGUAGE RebindableSyntax #-}

module ExampleGrades where

import FinMonad.Subdistribution
import Prelude hiding ((>>=), (>>), return, Left, Right)
import Data.MultiSet qualified as MSet


data Difficulty = Easy | Hard deriving (Eq, Show, Ord)
data Intelligence = Low | High deriving (Eq, Show, Ord)
data Grade = A | B | C deriving (Eq, Show, Ord)
data SatScore = Bad | Good deriving (Eq, Show, Ord)
data Letter = Positive | Negative deriving (Eq, Show, Ord)

student :: Subdistribution Intelligence
student = do
  diff <- subdistribution [(Easy, 6/10), (Hard, 4/10)]
  intel <- subdistribution [(Low, 7/10), (High, 3/10)]
  grade <- case (intel, diff) of
    (Low,  Easy) -> subdistribution [(A, 30/100), (B, 40/100), (C, 30/100)]
    (Low,  Hard) -> subdistribution [(A,  5/100), (B, 25/100), (C, 70/100)]
    (High, Easy) -> subdistribution [(A, 90/100), (B,  8/100), (C,  2/100)]
    (High, Hard) -> subdistribution [(A, 50/100), (B, 30/100), (C, 20/100)]
  sat <- case intel of
    Low  -> subdistribution [(Good,  5/100), (Bad, 95/100)]
    High -> subdistribution [(Good, 80/100), (Bad, 20/100)]
  letter <- case grade of
    A -> subdistribution [(Positive, 10/100), (Negative, 90/100)]
    B -> subdistribution [(Positive, 40/100), (Negative, 60/100)]
    C -> subdistribution [(Positive, 99/100), (Negative,  1/100)]
  observe (grade == C)
  observe (sat == Good)
  return intel

--- >>> student
-- <Subdistribution>
-- Validity: 1909 % 50000
-- Posterior: <Distribution> [(Low,35 % 83),(High,48 % 83)]
