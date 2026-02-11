{-# LANGUAGE RebindableSyntax #-}

module ExamplePiranha where

import FinMonad.Subdistribution
import Prelude hiding ((>>=), (>>), return)

-- Understanding probability : chance rules in everyday life. Tijms.

data Fish = Goldfish | Piranha deriving (Eq, Show) 

piranhaProblem :: Subdistribution Fish
piranhaProblem = do
    fish1 <- uniform [Goldfish, Piranha]
    fish2 <- return Piranha
    sample <- uniform [fish1, fish2]
    observe (sample == Piranha)
    return fish1

--- >>> piranhaProblem
-- <Subdistribution>
-- Validity: 3 % 4
-- Posterior: <Distribution> [(Goldfish,1 % 3),(Piranha,2 % 3)]
