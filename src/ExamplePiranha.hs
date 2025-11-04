{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use let" #-}

module ExamplePiranha where

import Subdistribution
import Prelude hiding ((>>=), (>>), return)

-- Understanding probability : chance rules in everyday life. Tijms.

data Fish = Goldfish | Piranha deriving (Eq, Show) 

piranhaProblem :: Distribution Fish
piranhaProblem = do
    fish1 <- uniform [Goldfish, Piranha]
    fish2 <- return Piranha
    sample <- uniform [fish1, fish2]
    observe (sample == Piranha)
    return fish1