{-# LANGUAGE RebindableSyntax #-}

module ExamplePrisoners where

import Subdistribution
import Prelude hiding ((>>=), (>>), return, Left, Right)

data Prisoner = PrisonerA | PrisonerB | PrisonerC deriving (Eq, Show, Ord)

prisoners :: Distribution Prisoner
prisoners = do
  governor <- uniform [PrisonerA, PrisonerB, PrisonerC]  
  warden <- case governor of
    PrisonerA -> uniform [PrisonerB,PrisonerC]
    PrisonerB -> uniform [PrisonerB]
    PrisonerC -> uniform [PrisonerC]
  observe (warden == PrisonerB)
  return governor