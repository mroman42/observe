{-# LANGUAGE RebindableSyntax #-}

module ExamplePrisoners where

import Prelude hiding ((>>=), (>>), return, Left, Right)
import Subdistribution

data Prisoner = PrisonerA | PrisonerB | PrisonerC deriving (Eq, Show, Ord)

prisoners :: Subdistribution Prisoner
prisoners = do
  governor <- uniform [PrisonerA, PrisonerB, PrisonerC]  
  warden <- case governor of
    PrisonerA -> uniform [PrisonerB,PrisonerC]
    PrisonerB -> uniform [PrisonerB]
    PrisonerC -> uniform [PrisonerC]
  observe (warden == PrisonerB)
  return governor


--- >>> prisoners
-- <Subdistribution>
-- Validity: 1 % 2
-- Posterior: <Distribution> [(PrisonerA,1 % 3),(PrisonerB,2 % 3)]
