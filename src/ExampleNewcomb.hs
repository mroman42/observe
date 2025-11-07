{-# LANGUAGE RebindableSyntax #-}

module ExampleNewcomb where

import Subdistribution
import Prelude hiding ((>>=), (>>), return)

data Action = OneBox | TwoBox deriving (Eq, Show) 

newcomb :: Action -> Subdistribution Double
newcomb x = do
    action <- uniform [OneBox, TwoBox]
    prediction <- uniform [OneBox, TwoBox]
    observe (action == prediction)
    observe (action == x)
    return $ case (action, prediction) of
        (OneBox, OneBox) -> 100
        (OneBox, TwoBox) -> 0
        (TwoBox, OneBox) -> 101
        (TwoBox, TwoBox) -> 1

--- >>> newcomb OneBox
-- <Subdistribution>
-- Validity: 1 % 4
-- Posterior: <Distribution> [(100.0,1 % 1)]

--- >>> newcomb TwoBox
-- <Subdistribution>
-- Validity: 1 % 4
-- Posterior: <Distribution> [(1.0,1 % 1)]
